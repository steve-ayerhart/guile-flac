(define-module (flac decoder)
  #:use-module (flac reader)
  #:use-module (flac format)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-11)

  #:use-module (rnrs arithmetic bitwise)

  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:export (read-flac-frame
            current-stream-info
            current-frame-header
            current-subframe-header
            current-frame-samples
            current-frame-footer
            with-initialized-decoder))

;; we try to avoid gc so we define some parameters to mutate/reuse objects
(define current-frame-header (make-parameter #f))
(define current-subframe-header (make-parameter #f))
(define current-stream-info (make-parameter #f))
(define current-frame-samples (make-parameter #f))
(define current-frame-footer (make-parameter #f))

;;; TODO: recover from bad frame
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
(define (decode-sample-rate raw)
  (case raw
    ((#b0000) (stream-info-sample-rate (current-stream-info)))
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

(define (decode-bits-per-sample raw)
  (case raw
    ((#b000) (stream-info-bits-per-sample (current-stream-info)))
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

(define (restore-linear-prediction channel coefficients order shift)
  (let ((blocksize (frame-header-blocksize (current-frame-header))))
    (do-ec (: i order blocksize)
           (let ((residual (array-cell-ref (current-frame-samples) channel i)))
             (array-cell-set!
              (current-frame-samples)
              (+ residual
                 (bitwise-arithmetic-shift-right
                  (sum-ec (: c (index j) coefficients) (* (array-cell-ref (current-frame-samples) channel (- i (- order j))) c))
                  shift))
              channel i)))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-coded-residual
;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#coded-residual
(define (read-residual-partitioned-rice channel blocksize predictor-order)
  (let-values (((coding-method partition-order) (read-entropy-coding-method-info)))
    (let ((param-bits (match coding-method ('rice 4) ('rice2 5) (_ #f)))
          (escape-param (match coding-method ('rice #b1111) ('rice2 #b11111)))
          (partitions (bitwise-arithmetic-shift 1 partition-order))
          (partition-samples (bitwise-arithmetic-shift-right blocksize partition-order))
          (current-sample-index predictor-order))
      (do-ec (:range partition 0 partitions)
             (let ((count (if (= 0 partition) (- partition-samples predictor-order) partition-samples))
                   (rice-parameter (flac-read-uint param-bits)))
               (if (= rice-parameter escape-param)
                   (let ((num-bits (flac-read-uint 5)))
                     (do-ec (:range i 0 count)
                            (begin
                              (array-cell-set! (current-frame-samples) (flac-read-sint num-bits) channel current-sample-index)
                              (set! current-sample-index (1+ current-sample-index)))))
                   (do-ec (:range i 0 count)
                          (begin
                            (array-cell-set! (current-frame-samples) (flac-read-rice-sint rice-parameter) channel current-sample-index)
                            (set! current-sample-index (1+ current-sample-index))))))))))

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

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-interchannel-decorrelation
(define (stereo-decorrelation channel-assignment)
  (let ((blocksize (frame-header-blocksize (current-frame-header))))
    (match channel-assignment
      ('independent #t)
      ('left
       (do-ec (:range b 0 blocksize)
              (array-cell-set! (current-frame-samples) (- (array-cell-ref (current-frame-samples) 0 b) (array-cell-ref (current-frame-samples) 1 b)) 1 b)))
      ('right
       (do-ec (:range b 0 blocksize)
              (array-cell-set! (current-frame-samples) (+ (array-cell-ref (current-frame-samples) 0 b) (array-cell-ref (current-frame-samples) 1 b)) 0 b)))
      ('mid
       (do-ec (:range b 0 blocksize)
              (let* ((side (array-cell-ref (current-frame-samples) 1 b))
                     (right (- (array-cell-ref (current-frame-samples) 0 b) (bitwise-arithmetic-shift-right side 1))))
                (array-cell-set! (current-frame-samples) right 1 b)
                (array-cell-set! (current-frame-samples) (+ right side) 0 b)))))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-subframe-header
(define (read-subframe-header)
  (read/assert-subframe-sync)
  (receive (order type)
      (read-subframe-type)
    (set-subframe-header-subframe-type! (current-subframe-header) type)
    (set-subframe-header-predictor-order! (current-subframe-header) order)
    (set-subframe-header-wasted-bits! (current-subframe-header) (read-subframe-wasted-bits))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-subframes
(define (read-subframe channel)
  (read-subframe-header)
  (let* ((wasted-bits (subframe-header-wasted-bits (current-subframe-header)))
         (predictor-order (subframe-header-predictor-order (current-subframe-header)))
         (sample-depth (calculate-sample-depth
                        (frame-header-bits-per-sample (current-frame-header))
                        wasted-bits
                        (frame-header-channel-assignment (current-frame-header))
                        channel))
         (blocksize (frame-header-blocksize (current-frame-header))))
    (match (subframe-header-subframe-type (current-subframe-header))
      ('constant (read-subframe-constant channel blocksize sample-depth wasted-bits))
      ('verbatim (read-subframe-verbatim channel blocksize sample-depth wasted-bits))
      ('fixed (read-subframe-fixed channel predictor-order blocksize sample-depth))
      ('lpc (read-subframe-lpc channel predictor-order blocksize sample-depth)))))


;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#section-5.3-2.1.1
(define (read-subframe-verbatim channel blocksize sample-depth wasted-bits)
  (do-ec (:range block 0 blocksize)
         (array-cell-set! (current-frame-samples) (bitwise-arithmetic-shift (flac-read-sint sample-depth) wasted-bits) channel block)))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#section-5.3-2.2.1
(define (read-subframe-constant channel blocksize sample-depth wasted-bits)
  (let ((sample (bitwise-arithmetic-shift (flac-read-sint sample-depth) wasted-bits)))
    (do-ec (:range block 0 blocksize)
           (array-cell-set! (current-frame-samples) sample channel block))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-fixed-predictor-subframe
(define (read-subframe-fixed channel predictor-order blocksize sample-depth)
  (let ((fixed-coefficients '(() (1) (2 -1) (3 -3 1) (4 -6 4 -1))))
    ;; warmup
    (do-ec (:range po 0 predictor-order)
           (array-cell-set! (current-frame-samples) (flac-read-sint sample-depth) channel po))
    ;; residuals
    (read-residual-partitioned-rice channel blocksize predictor-order)
    (restore-linear-prediction channel (list-ref fixed-coefficients predictor-order) predictor-order 0)))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-linear-predictor-subframe
(define (read-subframe-lpc channel lpc-order blocksize sample-depth)
  ;; warmup
  (do-ec (:range o 0 lpc-order)
         (array-cell-set! (current-frame-samples) (flac-read-sint sample-depth) channel o))

  (let* ((precision (+ 1 (flac-read-uint 4)))
         (shift (flac-read-sint 5))
         (coefs (reverse (list-ec (: o lpc-order) (flac-read-sint precision)))))

    ;; residuals
    (read-residual-partitioned-rice channel blocksize lpc-order)
    (restore-linear-prediction channel coefs lpc-order shift)))

(define (read-subframes)
  (let ((channels (stream-info-channels (current-stream-info)))
        (channel-assignment (frame-header-channel-assignment (current-frame-header))))
    (do ((channel 0 (1+ channel)))
        ((>= channel channels))
      (read-subframe channel))))

;;; TODO: actually verify the checksum
(define (read-frame-footer)
  (set-frame-footer-crc! (current-frame-footer) (flac-read-uint 16)))

(define (set-current-frame-header-fields! blocking-strategy blocksize sample-rate channel-assignment bits-per-sample frame/sample-number crc)
  (set-frame-header-blocking-strategy! (current-frame-header) blocking-strategy)
  (set-frame-header-blocksize! (current-frame-header) blocksize)
  (set-frame-header-sample-rate! (current-frame-header) sample-rate)
  (set-frame-header-channel-assignment! (current-frame-header) channel-assignment)
  (set-frame-header-bits-per-sample! (current-frame-header) bits-per-sample)
  (set-frame-header-frame/sample-number! (current-frame-header) frame/sample-number)
  (set-frame-header-crc! (current-frame-header) crc))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-frame-header
(define-public (read-frame-header)
  (read/assert-frame-sync-code)
  (let* ((blocking-strategy (decode-blocking-strategy (flac-read-uint 1)))
         (raw-blocksize (flac-read-uint 4))
         (raw-sample-rate (flac-read-uint 4))
         (channel-assignment (decode-channel-assignment (flac-read-uint 4)))
         (bits-per-sample (decode-bits-per-sample (flac-read-uint 3)))
         (ignore-reserved (flac-read-uint 1))
         (frame/sample-number (flac-read-coded-number))
         (blocksize (decode-block-size raw-blocksize))
         (sample-rate (decode-sample-rate raw-sample-rate))
         (crc (flac-read-uint 8)))
    (set-current-frame-header-fields!
     blocking-strategy blocksize sample-rate channel-assignment bits-per-sample frame/sample-number crc)))

(define (read-flac-frame)
  (read-frame-header)
  (read-subframes)
  (stereo-decorrelation (frame-header-channel-assignment (current-frame-header)))
  (align-to-byte)
  (read-frame-footer))

(define (with-initialized-decoder stream-info thunk)
  (let* ((channels (stream-info-channels stream-info))
         (max-block-samples (stream-info-max-block-size stream-info))
         (channels-array (make-array 0 channels max-block-samples)))

    (parameterize ((current-stream-info stream-info)
                   (current-frame-header (%make-frame-header #f #f #f #f #f #f #f))
                   (current-subframe-header (%make-subframe-header #f #f #f))
                   (current-frame-footer (%make-frame-footer #f))
                   (current-frame-samples channels-array))
      (thunk))))
