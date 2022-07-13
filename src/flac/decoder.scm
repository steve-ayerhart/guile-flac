(define-module (flac decoder)
  #:use-module (flac reader)
  #:use-module (flac format))

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

(define (read-frame-header stream-info)
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
