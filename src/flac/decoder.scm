(define-module (flac decoder)
  #:use-module (flac reader)
  #:use-module (flac format)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-11)

  #:use-module (rnrs arithmetic bitwise)

  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:export (read-flac-frame))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#section-10.1-1
(define (read/assert-frame-sync-code)
  (unless (= #b111111111111100 (flac-read-uint 15))
    #f))

(define (decode-blocking-strategy raw)
  (enum-lookup flac-frame-blocking-strategy-type raw))

(define (between? n a b)
  (and (>= n a) (<= n b)))

(define (decode-channel-assignment raw)
  (enum-lookup
   flac-channel-assignment-type
   (cond
    ((between? raw #b0000 #b0111) 0)
    ((= raw #b1000) 1)
    ((= raw #b1001) 2)
    ((= raw #b1010) 3)
    (else #f))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-block-size-bits
(define (decode-block-size raw)
  (cond
   ((= raw #b0000) 'reserved)
   ((= raw #b0001) 192)
   ((between? raw #b0010 #b0101) (* 576 (expt 2 (- raw 2))))
   ((= raw #b0110) (+ 1 (flac-read-uint 8)))
   ((= raw #b0111) (+ 1 (flac-read-uint 16)))
   ((between? raw #b1000 #b1111) (* 256 (expt 2 (- raw 8))))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-sample-rate-bits
(define (decode-sample-rate stream-info raw)
  (case raw
    ((#b0000) (stream-info-sample-rate stream-info))
    ((#b0001) 88200)
    ((#b0010) 17640)
    ((#b0011) 19200)
    ((#b0100)  8000)
    ((#b0101) 16000)
    ((#b0110) 22050)
    ((#b0111) 24000)
    ((#b1000) 32000)
    ((#b1001) 44100)
    ((#b1010) 48000)
    ((#b1011) 96000)
    ((#b1100) (* 1000 (flac-read-uint 8)) ; sample rate in kHz
    ((#b1101) (flac-read-uint 16)) ; sample rate in Hz
    ((#b1110) (* 10 (flac-read-uint 16))) ; sample rate in tens of Hz
    ((#b1111) 'invalid))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-coded-number
(define (flac-read-coded-number)
  (let coded-number-loop ((coded-sample-number (flac-read-uint 8)))
    (if (< coded-sample-number #b11000000)
        coded-sample-number
        (begin
          (flac-read-uint 8)
          (coded-number-loop (bitwise-and (bitwise-arithmetic-shift coded-sample-number 1) #xff))))))

(define (decode-bits-per-sample stream-info raw)
  (case raw
    ((#b000) (stream-info-bits-per-sample stream-info))
    ((#b001) 8)
    ((#b010) 12)
    ((#b011) 'reserved)
    ((#b100) 16)
    ((#b101) 20)
    ((#b110) 24)
    ((#b111) 'reserved)))

(define (read/assert-subframe-sync)
  (when (= 1 (flac-read-uint 1))
    (error "invalid subframe sync")))

(define (read-subframe-wasted-bits)
  (if (= 1 (flac-read-uint 1))
      (let wasted-loop ([unary 0])
        (if (= 0 (flac-read-uint 1))
            (wasted-loop (+ 1 unary))
            (+ 1 unary)))
      0))

(define (calculate-sample-depth bps wasted-bits channel-assignment channel)
  (+ (- bps wasted-bits)
     (match channel-assignment
       ('left  (if (= channel 1) 1 0))
       ('right (if (= channel 0) 1 0))
       ('mid   (if (= channel 1) 1 0))
       (_ 0))))

(define (read-entropy-coding-method-info)
  (values (case (flac-read-uint 2) [(#b00) 'rice] [(#b01) 'rice2]) (flac-read-uint 4)))

(define (restore-linear-prediction warmup residuals coefficients order shift)
  (fold (λ (residual samples)
          (append!
           samples
           (list
            (+ residual
               (bitwise-arithmetic-shift-right
                (fold (λ (residual coefficient predictor)
                        (+ predictor (* residual coefficient)))
                      0
                      (take-right samples order)
                      coefficients)
                shift)))))
        warmup
        residuals))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-coded-residual
(define (read-residual-partitioned-rice blocksize predictor-order)
  (let-values (((coding-method partition-order) (read-entropy-coding-method-info)))
    (let ((param-bits (match coding-method ('rice 4) ('rice2 5) (_ #f)))
          (escape-param (match coding-method ('rice #xf) ('rice2 #x1f)))
          (partitions (bitwise-arithmetic-shift 1 partition-order))
          (partition-samples (bitwise-arithmetic-shift-right blocksize partition-order)))
      (let residual-loop ((sample 0)
                          (partition 0)
                          (parameters '())
                          (raw-bits '())
                          (residual '()))
        (if (>= partition partitions)
            residual
            (let ((rice-parameter (flac-read-uint param-bits)))
              (if (< rice-parameter escape-param)
                  (let ((count (if (= 0 partition) (- partition-samples predictor-order) partition-samples)))
                    (residual-loop (+ sample count)
                                   (+ 1 partition)
                                   (cons rice-parameter parameters)
                                   (cons 0 raw-bits)
                                   (append residual (list-ec (: c count) (flac-read-rice-sint rice-parameter)))))
                  (let ((num-bits (flac-read-sint 5)))
                    (residual-loop sample
                                   (+ 1 partition)
                                   (cons rice-parameter parameters)
                                   (cons num-bits raw-bits)
                                   (let ((order (- partition-samples (if (= 0 partition) predictor-order 0))))
                                     (append residual
                                             (if (= 0 num-bits)
                                                 (make-list order 0)
                                                 (list-ec (: o order) (flac-read-sint num-bits))))))))))))))

;;; 000000 constant
;;; 000001 verbatim
;;; 00001x reserved
;;; 0001xx reserved
;;; 001xxx if xxx <= 4 fixed, xxx = order ; else reserved
;;; 01xxxx reserved
;;; 1xxxxx lpc xxxxx = order - 1
(define (read-subframe-type)
  (let ((raw (flac-read-uint 6)))
    (cond
     ((= raw #b000000) (values #f 'constant))
     ((= raw #b000001) (values #f 'verbatim))
     ((between? raw #b001000 #b001100) (values (bit-extract raw 0 3) 'fixed))
     ((between? raw #b100000 #b111111) (values (+ 1 (bit-extract raw 0 5)) 'lpc))
     (else (values #f #f)))))

                                        ; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-interchannel-decorrelation
(define (stereo-decorrelation samples channel-assignment)
  (match channel-assignment
    ('independent samples)
    ('left (list (first samples) (map! - (first samples) (second samples))))
    ('right (list (second samples) (map! + (first samples) (second samples))))
    ('mid (fold
           (λ (sample-0 sample-1 samples)
             (let* ((prev-samples-0 (first samples))
                    (prev-samples-1 (second samples))
                    (side sample-1)
                    (right (- sample-0 (bitwise-arithmetic-shift-right side 1))))
               (list
                (append prev-samples-0 (list right))
                (append prev-samples-1 (list (+ right side))))))
           '(() ())
           (first samples)
           (second samples)))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-subframe-header
(define (read-subframe-header)
  (read/assert-subframe-sync)
  (receive (order type)
      (read-subframe-type)
    (%make-subframe-header type order (read-subframe-wasted-bits))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-subframes
(define (read-subframe frame-header channel)
  (let* ((subframe-header (read-subframe-header))
         (wasted-bits (subframe-header-wasted-bits subframe-header))
         (predictor-order (subframe-header-predictor-order subframe-header))
         (sample-depth (calculate-sample-depth
                        (frame-header-bits-per-sample frame-header)
                        wasted-bits
                        (frame-header-channel-assignment frame-header)
                        channel))
         (blocksize (frame-header-blocksize frame-header)))
    (match (subframe-header-subframe-type subframe-header)
      ('constant (read-subframe-constant blocksize sample-depth wasted-bits))
      ('verbatim (read-subframe-verbatim blocksize sample-depth wasted-bits))
      ('fixed (read-subframe-fixed predictor-order blocksize sample-depth))
      ('lpc (read-subframe-lpc predictor-order blocksize sample-depth)))))


;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#section-5.3-2.1.1
(define (read-subframe-verbatim blocksize sample-depth wasted-bits)
  (list-ec (: b blocksize) (bitwise-arithmetic-shift (flac-read-sint sample-depth) wasted-bits)))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#section-5.3-2.2.1
(define (read-subframe-constant blocksize sample-depth wasted-bits)
  (make-list blocksize (bitwise-arithmetic-shift (flac-read-sint sample-depth) wasted-bits)))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-fixed-predictor-subframe
(define (read-subframe-fixed predictor-order blocksize sample-depth)
  (let ((warmup (list-ec (: o predictor-order) (flac-read-sint sample-depth)))
        (fixed-coefficients '(() (1) (2 -1) (3 -3 1) (4 -6 4 -1)))
        (residual (read-residual-partitioned-rice blocksize predictor-order)))
    (restore-linear-prediction warmup residual (list-ref fixed-coefficients predictor-order) predictor-order 0)))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-linear-predictor-subframe
(define (read-subframe-lpc lpc-order blocksize sample-depth)
  (let* ((warmup (list-ec (: o lpc-order) (flac-read-sint sample-depth)))
         (precision (+ 1 (flac-read-uint 4)))
         (shift (flac-read-sint 5))
         (coefs (reverse (list-ec (: o lpc-order) (flac-read-sint precision))))
         (residual (read-residual-partitioned-rice blocksize lpc-order)))
    (restore-linear-prediction warmup residual coefs lpc-order shift)))

(define (read-subframes stream-info frame-header)
  (let ((channels (stream-info-channels stream-info))
        (channel-assignment (frame-header-channel-assignment frame-header)))
    (map! (λ (channel) (read-subframe frame-header channel)) (iota channels))))

;;; TODO: actually verify the checksum
(define (read-frame-footer)
  (flac-read-uint 16))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-frame-header
(define-public (read-frame-header stream-info)
  (read/assert-frame-sync-code)
  (let* ((blocking-strategy (decode-blocking-strategy (flac-read-uint 1)))
         (raw-blocksize (flac-read-uint 4))
         (raw-sample-rate (flac-read-uint 4))
         (channel-assignment (decode-channel-assignment (flac-read-uint 4)))
         (bits-per-sample (decode-bits-per-sample stream-info (flac-read-uint 3)))
         (ignore-reserved (flac-read-uint 1))
         (frame/sample-number (flac-read-coded-number))
         (blocksize (decode-block-size raw-blocksize))
         (sample-rate (decode-sample-rate stream-info raw-sample-rate))
         (crc (flac-read-uint 8)))
    (%make-frame-header
     blocking-strategy
     blocksize
     sample-rate
     channel-assignment
     bits-per-sample
     frame/sample-number
     crc)))

(define (read-flac-frame stream-info)
  (let* ((header (read-frame-header stream-info))
         (samples (read-subframes stream-info header)))
    (align-to-byte)
    (let ((footer (read-frame-footer)))
      (%make-frame header footer samples))))
