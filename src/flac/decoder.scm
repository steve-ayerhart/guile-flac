(define-module (flac decoder)
  #:use-module (flac reader)
  #:use-module (flac format)
  #:use-module (flac metadata)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-11)

  #:use-module (rnrs arithmetic bitwise)

  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 binary-ports)

  #:export (read-flac-frame
            current-stream-info
            current-frame-header
            current-subframe-header
            current-frame-samples
            current-frame-footer
            log-port
            with-flac-file-decoder
            with-initialized-decoder))

;; we try to avoid gc so we define some parameters to mutate/reuse objects
(define current-frame-header (make-parameter #f))
(define current-subframe-header (make-parameter #f))
(define current-stream-info (make-parameter #f))
(define current-frame-samples (make-parameter #f))
(define current-frame-footer (make-parameter #f))
(define log-port (make-parameter #f))

;;; TODO: recover from bad frame
;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#section-10.1-1
(define (read/assert-frame-sync-code)
  (unless (= #b111111111111100 (flac-read-uint 15))
    (error "frame code")))

(define (decode-blocking-strategy raw)
  (enum-lookup flac-frame-blocking-strategy-type raw))

(define (between? n a b)
  (and (>= n a) (<= n b)))

(define (decode-channel-assignment raw)
  (enum-lookup
   flac-channel-assignment-type
   (cond
    ((between? raw #b0000 #b0111) 0) ; independent
    ((= raw #b1000) 1) ; left
    ((= raw #b1001) 2) ; right
    ((= raw #b1010) 3) ; mid
    (else #f))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-block-size-bits
(define (decode-block-size raw)
  (cond
   ((= raw #b0000) 'reserved)
   ((= raw #b0001) 192)
   ((between? raw #b0010 #b0101) (* 576 (expt 2 (- raw 2))))
   ((= raw #b0110) (1+ (flac-read-uint 8)))
   ((= raw #b0111) (1+ (flac-read-uint 16)))
   ((between? raw #b1000 #b1111) (* 256 (expt 2 (- raw 8))))))


;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-sample-rate-bits
(define (decode-sample-rate raw)
  (match raw
    (#b0000 (stream-info-sample-rate (current-stream-info)))
    (#b0001 88200)
    (#b0010 17640)
    (#b0011 19200)
    (#b0100  8000)
    (#b0101 16000)
    (#b0110 22050)
    (#b0111 24000)
    (#b1000 32000)
    (#b1001 44100)
    (#b1010 48000)
    (#b1011 96000)
    (#b1100 (* 1000 (flac-read-uint 8))) ; sample rate in kHz
    (#b1101 (flac-read-uint 16)) ; sample rate in Hz
    (#b1110 (* 10 (flac-read-uint 16))) ; sample rate in tens of Hz
    (#b1111 'invalid)))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-coded-number
(define (flac-read-coded-number)
  (let coded-number-loop ((coded-sample-number (flac-read-uint 8)))
    (if (< coded-sample-number #b11000000)
        coded-sample-number
        (begin
          (flac-read-uint 8)
          (coded-number-loop (bitwise-and (bitwise-arithmetic-shift coded-sample-number 1) #xff))))))

(define (decode-bits-per-sample raw)
  (match raw
    (#b000 (stream-info-bits-per-sample (current-stream-info)))
    (#b001 8)
    (#b010 12)
    (#b011 'reserved)
    (#b100 16)
    (#b101 20)
    (#b110 24)
    (#b111 'reserved)))

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
  (let ((blocksize (assoc-ref (current-frame-header) 'blocksize)))
    (do-ec (: i order blocksize)
           (let* ((residual (array-cell-ref (current-frame-samples) channel i))
                  (prediction-sum (sum-ec (: c (index j) coefficients)
                                          (* (array-cell-ref (current-frame-samples) channel (- i (- order j))) c)))
                  (prediction (bitwise-arithmetic-shift-right prediction-sum shift))
                  (result (+ residual prediction)))
             (array-cell-set! (current-frame-samples) result channel i)))))

;;; RFC 9639 Section 9.2.2: Apply wasted bits by left-shifting samples
;;; "A decoder MUST add k least-significant zero bits by shifting left (padding)
;;;  after decoding a subframe sample."
(define (apply-wasted-bits-to-channel channel blocksize wasted-bits)
  "Shift all samples in CHANNEL left by WASTED-BITS positions to restore original bit depth"
  (when (> wasted-bits 0)
    (do-ec (:range i 0 blocksize)
           (array-cell-set! (current-frame-samples)
                           (bitwise-arithmetic-shift-left
                            (array-cell-ref (current-frame-samples) channel i)
                            wasted-bits)
                           channel i))))

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

(define (stereo-decorrelate-left blocksize)
  (do-ec (:range b 0 blocksize)
         (array-cell-set! (current-frame-samples) (- (array-cell-ref (current-frame-samples) 0 b) (array-cell-ref (current-frame-samples) 1 b)) 1 b)))

(define (stereo-decorrelate-right blocksize)
  (do-ec (:range b 0 blocksize)
         (array-cell-set! (current-frame-samples) (+ (array-cell-ref (current-frame-samples) 0 b) (array-cell-ref (current-frame-samples) 1 b)) 0 b)))

(define (stereo-decorrelate-mid blocksize)
  (do-ec (:range b 0 blocksize)
         (let* ((mid (array-cell-ref (current-frame-samples) 0 b))
                (side (array-cell-ref (current-frame-samples) 1 b))
                ;; Shift mid left by 1 bit and add 1 if side is odd
                (mid-adjusted (+ (bitwise-arithmetic-shift-left mid 1)
                                 (if (odd? side) 1 0)))
                ;; Reconstruct left and right
                (left (bitwise-arithmetic-shift-right (+ mid-adjusted side) 1))
                (right (bitwise-arithmetic-shift-right (- mid-adjusted side) 1)))
           (array-cell-set! (current-frame-samples) left 0 b)
           (array-cell-set! (current-frame-samples) right 1 b))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-interchannel-decorrelation
(define (stereo-decorrelation channel-assignment)
  (let ((blocksize (assoc-ref (current-frame-header) 'blocksize)))
    (match channel-assignment
      ('independent #t) ; do nothing
      ('left (stereo-decorrelate-left blocksize))
      ('right (stereo-decorrelate-right blocksize))
      ('mid (stereo-decorrelate-mid blocksize)))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-subframe-header
(define (read-subframe-header)
  (read/assert-subframe-sync)
  (receive (order type)
      (read-subframe-type)
    (current-subframe-header
      `((subframe-type . ,type)
        (predictor-order . ,order)
        (wasted-bits . ,(read-subframe-wasted-bits))))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-subframes
(define (read-subframe channel)
  (read-subframe-header)
  (let* ((wasted-bits (assoc-ref (current-subframe-header) 'wasted-bits))
         (predictor-order (assoc-ref (current-subframe-header) 'predictor-order))
         (sample-depth (calculate-sample-depth
                        (assoc-ref (current-frame-header) 'bits-per-sample)
                        wasted-bits
                        (assoc-ref (current-frame-header) 'channel-assignment)
                        channel))
         (blocksize (assoc-ref (current-frame-header) 'blocksize)))
    (match (assoc-ref (current-subframe-header) 'subframe-type)
      ('constant (read-subframe-constant channel blocksize sample-depth))
      ('verbatim (read-subframe-verbatim channel blocksize sample-depth))
      ('fixed (read-subframe-fixed channel predictor-order blocksize sample-depth))
      ('lpc (read-subframe-lpc channel predictor-order blocksize sample-depth)))
    (apply-wasted-bits-to-channel channel blocksize wasted-bits)))


;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#section-5.3-2.1.1
(define (read-subframe-verbatim channel blocksize sample-depth)
  (do-ec (:range block 0 blocksize)
         (array-cell-set! (current-frame-samples) (flac-read-sint sample-depth) channel block)))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#section-5.3-2.2.1
(define (read-subframe-constant channel blocksize sample-depth)
  (let ((sample (flac-read-sint sample-depth)))
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
    ;; coefficients need to be reversed to apply to most recent samples first
    (restore-linear-prediction channel (reverse (list-ref fixed-coefficients predictor-order)) predictor-order 0)))

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
        (channel-assignment (assoc-ref (current-frame-header) 'channel-assignment)))
    (do ((channel 0 (1+ channel)))
        ((>= channel channels))
      (read-subframe channel))))

;;; TODO: actually verify the checksum
(define (read-frame-footer)
  (current-frame-footer `((crc . ,(flac-read-uint 16)))))

(define (set-current-frame-header-fields! blocking-strategy blocksize sample-rate channel-assignment bits-per-sample frame/sample-number crc)
  (current-frame-header
    `((blocking-strategy . ,blocking-strategy)
      (blocksize . ,blocksize)
      (sample-rate . ,sample-rate)
      (channel-assignment . ,channel-assignment)
      (bits-per-sample . ,bits-per-sample)
      (frame/sample-number . ,frame/sample-number)
      (crc . ,crc))))

;;; https://www.ietf.org/archive/id/draft-ietf-cellar-flac-07.html#name-frame-header
(define-public (read-frame-header)
  (read/assert-frame-sync-code)
  (let* ((blocking-strategy (decode-blocking-strategy (flac-read-uint 1)))
         (raw-blocksize (flac-read-uint 4))
         (raw-sample-rate (flac-read-uint 4))
         (channel-assignment (decode-channel-assignment (flac-read-uint 4)))
         (bits-per-sample-raw (decode-bits-per-sample (flac-read-uint 3)))
         (ignore-reserved (flac-read-uint 1))
         (frame/sample-number (flac-read-coded-number))
         (blocksize (decode-block-size raw-blocksize))
         (sample-rate (decode-sample-rate raw-sample-rate))
         (crc (flac-read-uint 8)))

    (when (eq? blocksize 'reserved)
      (error "FLAC frame header contains reserved blocksize value"))
    (when (eq? sample-rate 'invalid)
      (error "FLAC frame header contains invalid sample rate value"))
    (let ((bits-per-sample (if (eq? bits-per-sample-raw 'reserved)
                                (stream-info-bits-per-sample (current-stream-info))
                                bits-per-sample-raw)))
      (set-current-frame-header-fields!
       blocking-strategy blocksize sample-rate channel-assignment bits-per-sample frame/sample-number crc))))

(define (read-frame)
  (read-frame-header)
  (read-subframes)
  (stereo-decorrelation (assoc-ref (current-frame-header) 'channel-assignment))
  (align-to-byte)
  (read-frame-footer))

(define (read-flac-frame)
  (if (end-of-flac-stream?)
      (eof-object)
      (read-frame)))

(define (with-initialized-decoder stream-info thunk)
  (let* ((channels (stream-info-channels stream-info))
         (max-block-samples (stream-info-max-block-size stream-info))
         (channels-array (make-array 0 channels max-block-samples)))

    (parameterize ((current-stream-info stream-info)
                   (current-frame-header (make-frame-header-alist))
                   (current-subframe-header (make-subframe-header-alist))
                   (current-frame-footer (make-frame-footer-alist))
                   (current-frame-samples channels-array))
      (thunk))))

(define (with-flac-file-decoder infile thunk)
  (with-input-from-file infile
    (lambda ()
      (with-flac-input-port
       (current-input-port)
       (lambda ()
         (flac-read/assert-magic)
         (let* ((metadata (read-flac-metadata))
                (stream-info (find metadata-stream-info? metadata)))
           (unless stream-info
             (error "FLAC file missing required STREAMINFO metadata block"))
           (with-initialized-decoder stream-info thunk)))))
    #:binary #t))
