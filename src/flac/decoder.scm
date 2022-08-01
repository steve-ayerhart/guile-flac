(define-module (flac decoder)
  #:use-module (flac reader)
  #:use-module (flac format)

  #:use-module (srfi srfi-42)

  #:use-module (rnrs arithmetic bitwise)

  #:use-module (ice-9 match)
  #:use-module (ice-9 receive))

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
    [(between? raw #b0000 #b0111) 0]
    [(= raw #b1000) 1]
    [(= raw #b1001) 2]
    [(= raw #b1010) 3]
    [else #f])))

(define (decode-block-size raw)
  (cond
   [(= raw #b0000) 'reserved]
   [(= raw #b0001) 192]
   [(between? raw #b0010 #b0101) (* 576 (expt 2 (- raw 2)))]
   [(= raw #b0110) (+ 1 (flac-read-uint 8))]
   [(= raw #b0111) (+ 1 (flac-read-uint 16))]
   [(between? raw #b1000 #b1111) (* 256 (expt 2 (- raw 8)))]))

(define (decode-sample-rate stream-info raw)
  (case raw
    [(#b0000) (stream-info-sample-rate stream-info)]
    [(#b0001) 88200]
    [(#b0010) 17640]
    [(#b0011) 19200]
    [(#b0100)  8000]
    [(#b0101) 16000]
    [(#b0110) 22050]
    [(#b0111) 24000]
    [(#b1000) 32000]
    [(#b1001) 44100]
    [(#b1010) 48000]
    [(#b1011) 96000]
    [(#b1100) (* 1000 (flac-read-uint 8))] ; sample rate in kHz
    [(#b1101) (flac-read-uint 16)] ; sample rate in Hz
    [(#b1110) (* 10 (flac-read-uint 16))] ; sample rate in tens of Hz
    [(#b1111) 'invalid]))

(define (decode-bits-per-sample stream-info raw)
  (case raw
    [(#b000) (stream-info-bits-per-sample stream-info)]
    [(#b001) 8]
    [(#b010) 12]
    [(#b011) 'reserved]
    [(#b100) 16]
    [(#b101) 20]
    [(#b110) 24]
    [(#b111) 'reserved]))

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

(define (read-subframe-constant blocksize sample-depth wasted-bits)
  (%make-subframe-constant (make-list blocksize (bitwise-arithmetic-shift (flac-read-sint sample-depth) wasted-bits))))

(define (read-subframe-verbatim blocksize sample-depth wasted-bits)
  (%make-subframe-verbatim
   (list-ec (: b blocksize) (bitwise-arithmetic-shift (flac-read-sint sample-depth) wasted-bits))))

(define (read-subframe-fixed predictor-order blocksize sample-depth)
  (format #t "bitdepth: ~a\n" sample-depth)
  (let ((warmup ((list-ec (: o predictor-order) (flac-read-sint sample-depth)))))
    raw))

(define (read-subframe-lpc) #f)

;;; 000000 constant
;;; 000001 verbatim
;;; 00001x reserved
;;; 0001xx reserved
;;; 001xxx if xxx <= 4 fixed, xxx = order ; else reserved
;;; 01xxxx reserved
;;; 1xxxxx lpc xxxxx = order - 1
(define (read-subframe-type)
  (let ([raw (flac-read-uint 6)])
    (format #t "raw: ~a\n" raw)
    (cond
     [(= raw #b000000) (values #f 'constant)]
     [(= raw #b000001) (values #f 'verbatim)]
     [(between? raw #b001000 #b001100) (values (bit-extract raw 0 3) 'fixed)]
     [(between? raw #b100000 #b111111) (values (bit-extract raw 0 6) 'lpc)]
     (else (values #f #f)))))

(define (read-subframe-header)
  (read/assert-subframe-sync)
  (receive (order type)
      (read-subframe-type)
    (%make-subframe-header type order (read-subframe-wasted-bits))))

(define (read-subframe frame-header channel)
  (let* ([subframe-header (read-subframe-header)]
         [wasted-bits (subframe-header-wasted-bits subframe-header)]
         [predictor-order (subframe-header-predictor-order subframe-header)]
         [sample-depth (calculate-sample-depth
                        (frame-header-bits-per-sample frame-header)
                        wasted-bits
                        (frame-header-channel-assignment frame-header)
                        channel)]
         [blocksize (frame-header-blocksize frame-header)])
    (format #t "sf: ~a\n" subframe-header)
    (%make-subframe
     subframe-header
     (match (subframe-header-subframe-type subframe-header)
       ('constant (read-subframe-constant blocksize sample-depth wasted-bits))
       ('verbatim (read-subframe-verbatim blocksize sample-depth wasted-bits))
       ('fixed (read-subframe-fixed predictor-order blocksize sample-depth))
       ('lpx (read-subframe-lpc))))))

(define (read-subframes stream-info frame-header)
  (let ([channels (stream-info-channels stream-info)])
    (map
     (λ (header channel)
       (read-subframe header channel))
     (make-list channels frame-header)
     (iota channels))))

(define-public (read-frame-header stream-info)
  (read/assert-frame-sync-code)
  (let* ([blocking-strategy (decode-blocking-strategy (flac-read-uint 1))]
         [raw-blocksize (flac-read-uint 4)]
         [raw-sample-rate (flac-read-uint 4)]
         [channel-assignment (decode-channel-assignment (flac-read-uint 4))]
         [bits-per-sample (decode-bits-per-sample stream-info (flac-read-uint 3))]
         [ignore-reserved (flac-read-uint 1)]
         [frame/sample-number (flac-read-coded-number)]
         [blocksize (decode-block-size raw-blocksize)]
         [sample-rate (decode-sample-rate stream-info raw-sample-rate)]
         [crc (flac-read-uint 8)])
    (make-frame-header
     blocking-strategy
     blocksize
     sample-rate
     channel-assignment
     bits-per-sample
     frame/sample-number
     crc)))

(define (read-frame-footer) #f)

(define-public (read-flac-frame stream-info)
  (let* ([header (read-frame-header stream-info)]
         [subframes (read-subframes stream-info header)]
         [footer (read-frame-footer)])
    (%make-frame header subframes footer)))
