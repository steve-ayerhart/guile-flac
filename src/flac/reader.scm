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
            flac-read-rice-sint
            with-flac-input-port
            with-flac-input-file
            end-of-flac-stream?
            align-to-byte
            %make-flac-reader
            flac-read/assert-magic
            current-flac-reader))

(define current-flac-reader (make-parameter #f))
(define default-bit-reader-capacity 65536)

;;; TODO: redo api? callback based?

(define-record-type <flac-reader>
  (%make-flac-reader input-port bit-buffer bit-buffer-length)
  flac-reader?
  (input-port flac-reader-port)
  (bit-buffer flac-reader-bit-buffer set-flac-reader-bit-buffer!)
  (bit-buffer-length flac-reader-bit-buffer-length set-flac-reader-bit-buffer-length!))

(define (with-flac-input-port port thunk)
  (with-input-from-port port
    (λ ()
      (parameterize ((current-flac-reader (%make-flac-reader (current-input-port) 0 0)))
        (thunk)))))

(define (with-flac-input-file filename thunk)
  (with-input-from-file filename
    (λ ()
      (parameterize ((current-flac-reader (%make-flac-reader (current-input-port) 0 0)))
        (thunk)))))

(define (end-of-flac-stream?)
  (eof-object? (lookahead-u8 (flac-reader-port (current-flac-reader)))))

(define (flac-read-bits reader bits)
  (while (< (flac-reader-bit-buffer-length reader) bits)
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

(define (align-to-byte)
  "Discard bits from the buffer to align to the next byte boundary."
  (let* ((reader (current-flac-reader))
         (bit-length (flac-reader-bit-buffer-length reader))
         (bits-to-discard (modulo bit-length 8)))
    (set-flac-reader-bit-buffer-length! reader (- bit-length bits-to-discard))))

(define (flac-read-bytes n)
  "Read N bytes from the FLAC stream and return as a bytevector."
  (u8-list->bytevector (map (λ (_) (flac-read-uint 8)) (iota n))))

(define (flac-read-uint bits)
  "Read an unsigned integer of BITS width from the FLAC stream."
  (flac-read-bits (current-flac-reader) bits))

(define (flac-read-sint bits)
  "Read a signed integer of BITS width from the FLAC stream (two's complement)."
  (let ((uint (flac-read-uint bits)))
    (- uint (bitwise-arithmetic-shift (bitwise-arithmetic-shift-right uint (- bits 1)) bits))))

(define (flac-read-rice-sint param)
  "Read a Rice-encoded signed integer with the given Rice parameter.
   Uses unary coding for quotient and binary coding for remainder,
   then applies zigzag decoding to convert unsigned to signed."
  (let rice-loop ((quotient 0))
    (if (= 0 (flac-read-uint 1))
        ;; Read unary quotient: count zeros until we hit a one
        (rice-loop (+ 1 quotient))
        ;; Combine quotient and remainder, then zigzag decode
        (let ((unsigned-val (bitwise-ior
                             (bitwise-arithmetic-shift quotient param)
                             (flac-read-uint param))))
          ;; Zigzag decode: (n >> 1) ^ -(n & 1)
          (bitwise-xor
           (bitwise-arithmetic-shift-right unsigned-val 1)
           (* -1 (bitwise-and unsigned-val 1)))))))

(define* (flac-read/assert-magic #:key (max-scan-bytes 65536))
  "Find and verify the FLAC magic number, positioning the reader at the start of FLAC metadata.
   Scans up to max-scan-bytes for the marker if not found at current position."
  (let ((magic (flac-read-uint 32)))
    (if (= FLAC-MAGIC magic)
        #t
        (let* ((reader (current-flac-reader))
               (port (flac-reader-port reader)))
          (set-flac-reader-bit-buffer! reader 0)
          (set-flac-reader-bit-buffer-length! reader 0)
          (let scan-loop ((bytes-scanned 4))
            (if (>= bytes-scanned max-scan-bytes)
                (error "Not a valid FLAC stream - magic marker not found"
                       (format #f "Scanned ~a bytes" max-scan-bytes))
                (let ((byte (get-u8 port)))
                  (cond
                   ((eof-object? byte)
                    (error "Not a valid FLAC stream - reached EOF"))
                   ((= byte #x66)
                    (let ((b1 (get-u8 port))
                          (b2 (get-u8 port))
                          (b3 (get-u8 port)))
                      (if (and (= b1 #x4C) (= b2 #x61) (= b3 #x43))
                          (begin
                            (format (current-error-port)
                                    "Found FLAC marker at offset ~a\n"
                                    (- bytes-scanned 3))
                            #t)
                          (scan-loop (+ bytes-scanned 4)))))
                   (else (scan-loop (+ bytes-scanned 1)))))))))))

(define (flac-read-coded-number)
  "Read a UTF-8-style variable-length coded number from the FLAC stream.
   Used for frame/sample numbers in the frame header."
  (let coded-number-loop ((coded-sample-number (flac-read-uint 8)))
    (if (< coded-sample-number #b11000000)
        coded-sample-number
        (begin
          (flac-read-uint 8)
          (coded-number-loop (bitwise-and (bitwise-arithmetic-shift coded-sample-number 1) #xff))))))
