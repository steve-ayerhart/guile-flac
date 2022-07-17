(define-module (flac reader)
  #:use-module (flac format)
  #:use-module (srfi srfi-9)

  #:use-module (ice-9 binary-ports)

  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)

  #:export (flac-read-uint
            flac-read-sint
            flac-read-bytes
            flac-read-coded-number
            with-flac-input-port
            new-flac-reader
            make-flac-reader
            flac-read/assert-magic
            current-flac-reader))

(define current-flac-reader (make-parameter #f))

(define-record-type <flac-reader>
  (make-flac-reader port bit-buffer bit-buffer-length)
  flac-reader?
  (port flac-reader-port)
  (bit-buffer flac-reader-bit-buffer set-flac-reader-bit-buffer!)
  (bit-buffer-length flac-reader-bit-buffer-length set-flac-reader-bit-buffer-length!))

(define (with-flac-input-port port thunk)
  (with-input-from-port port
    (λ ()
      (parameterize ((current-flac-reader (make-flac-reader (current-input-port) 0 0)))
        (thunk)))))

(define (new-flac-reader port)
  (current-flac-reader (make-flac-reader port 0 0))
  (current-flac-reader))

(define (flac-read-bits reader bits)
  (while
      (< (flac-reader-bit-buffer-length reader) bits)
    (let ((byte-read (get-u8 (flac-reader-port reader))))
      (set-flac-reader-bit-buffer! reader
                                   (bitwise-ior (bitwise-arithmetic-shift (flac-reader-bit-buffer reader) 8)
                                                byte-read))
      (set-flac-reader-bit-buffer-length! reader (+ (flac-reader-bit-buffer-length reader) 8))))

  (set-flac-reader-bit-buffer-length! reader (- (flac-reader-bit-buffer-length reader) bits))

  (let ((uint (bitwise-and (bitwise-arithmetic-shift-right
                            (flac-reader-bit-buffer reader) (flac-reader-bit-buffer-length reader))
                           (- (bitwise-arithmetic-shift 1 bits) 1))))
    (set-flac-reader-bit-buffer! reader
                                 (bitwise-and
                                  (flac-reader-bit-buffer reader)
                                  (- (bitwise-arithmetic-shift 1 (flac-reader-bit-buffer-length reader)) 1)))
    uint))

(define (flac-read-bytes n)
  (u8-list->bytevector (map (λ (_) (flac-read-uint 8)) (iota n))))

(define (flac-read-uint bits)
  (flac-read-bits (current-flac-reader) bits))

(define (flac-read-sint bits)
  (let ([uint (flac-read-uint bits)])
    (- uint (bitwise-arithmetic-shift (bitwise-arithmetic-shift-right uint (- bits 1)) bits))))

(define (flac-read/assert-magic)
  (unless (= FLAC-MAGIC (flac-read-uint 32))
    #f))

(define (flac-read-coded-number)
  (let coded-number-loop ((coded-sample-number (flac-read-uint 8)))
    (if (< coded-sample-number #b11000000)
        coded-sample-number
        (coded-number-loop (bitwise-and (bitwise-arithmetic-shift coded-sample-number 1) #xff)))))
