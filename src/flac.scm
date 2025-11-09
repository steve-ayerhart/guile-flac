(define-module (flac)
  #:use-module (flac reader)
  #:use-module (flac decoder)
  #:use-module (flac metadata)
  #:use-module (flac format)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42)
  #:use-module (rnrs arithmetic bitwise)

  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module ((scheme base)
                #:select (bytevector-append))
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)

  #:export (decode-flac-file
            with-flac-file-decoder
            flac-file-decode-logger
            flac-file-frame-logger
            current-md5-bytevectors
            gcrypt-available?
            verify-md5?
            md5-mismatch-handler
            compute-md5-hash
            bytevector->hex-string))

;; Conditionally load gcrypt for MD5 verification
(define gcrypt-available?
  (catch #t
    (lambda ()
      (resolve-module '(gcrypt hash))
      #t)
    (lambda (key . args)
      #f)))

;; Import hash functions if gcrypt is available
;; Only import hash-related symbols to avoid conflicts with bytestructures (uint32, uint16)
(when gcrypt-available?
  (let* ((gcrypt-hash (resolve-module '(gcrypt hash)))
         (hash-syms '(hash-algorithm md5 bytevector-hash)))
    (for-each
     (lambda (sym)
       (module-define! (current-module) sym
                       (module-ref gcrypt-hash sym)))
     hash-syms)))

;; Parameter for MD5 accumulation - stores list of bytevectors for all frames
(define current-md5-bytevectors (make-parameter #f))

;; Parameter to control MD5 verification (defaults to enabled if gcrypt available)
(define verify-md5? (make-parameter gcrypt-available?))

;; Handler for MD5 mismatches - can be customized by user
;; Default: print warning to stderr
(define md5-mismatch-handler
  (make-parameter
   (lambda (expected computed)
     (format (current-error-port) "WARNING: MD5 checksum mismatch!~%")
     (format (current-error-port) "Expected: ~a~%Computed: ~a~%" expected computed))))

;; Format bytevector as hex string for MD5 display
(define (bytevector->hex-string bv)
  (string-join
   (map (lambda (byte) (format #f "~2,'0x" byte))
        (bytevector->u8-list bv))
   ""))

;; Compute MD5 hash (only called when gcrypt is available)
(define (compute-md5-hash bv)
  (if gcrypt-available?
      ((module-ref (resolve-module '(gcrypt hash)) 'bytevector-hash)
       bv
       ((module-ref (resolve-module '(gcrypt hash)) 'lookup-hash-algorithm) 'md5))
      #f))

;; Convert frame samples to bytevector for MD5 computation
;; FLAC spec: MD5 of interleaved signed little-endian PCM samples
(define (frame-samples->bytevector)
  (let* ((channels (stream-info-channels (current-stream-info)))
         (blocksize (assoc-ref (current-frame-header) 'blocksize))
         (bits-per-sample (assoc-ref (current-frame-header) 'bits-per-sample))
         (sample-bytes (ceiling-quotient bits-per-sample 8))
         (samples (list-ec (:range sample 0 blocksize)
                           (:range channel 0 channels)
                           (array-cell-ref (current-frame-samples) channel sample))))
    (sint-list->bytevector samples (endianness little) sample-bytes)))

;; Standard PCM WAV header
(define header-struct-pcm
  (bs:struct `((filetype ,(bs:string 4 'utf8))
               (filesize ,uint32)
               (filetype-header ,(bs:string 4 'utf8))
               (format-chunk-marker ,(bs:string 4 'utf8))
               (format-chunk-length ,uint32)
               (format-type ,uint16)
               (num-channels ,uint16)
               (sample-freq ,uint32)
               (bytes/sec ,uint32)
               (block-alignment ,uint16)
               (bits-per-sample ,uint16)
               (data-chunk-header ,(bs:string 4 'utf8))
               (data-chunk-size ,uint32))))

;; WAVE_FORMAT_EXTENSIBLE header for >2 channels or non-standard bit depths
(define header-struct-extensible
  (bs:struct `((filetype ,(bs:string 4 'utf8))
               (filesize ,uint32)
               (filetype-header ,(bs:string 4 'utf8))
               (format-chunk-marker ,(bs:string 4 'utf8))
               (format-chunk-length ,uint32)  ; 40 for extensible (16 + 24 extra)
               (format-type ,uint16)          ; 0xFFFE for extensible
               (num-channels ,uint16)
               (sample-freq ,uint32)
               (bytes/sec ,uint32)
               (block-alignment ,uint16)
               (bits-per-sample ,uint16)      ; Container size (storage)
               (cb-size ,uint16)              ; 22 bytes of extra data
               (valid-bits-per-sample ,uint16) ; Actual significant bits
               (channel-mask ,uint32)
               (sub-format-guid ,(bs:vector 16 uint8))  ; PCM GUID
               (data-chunk-header ,(bs:string 4 'utf8))
               (data-chunk-size ,uint32))))

;; PCM SubFormat GUID: {00000001-0000-0010-8000-00aa00389b71}
(define pcm-subformat-guid
  #vu8(#x01 #x00 #x00 #x00 #x00 #x00 #x10 #x00
            #x80 #x00 #x00 #xaa #x00 #x38 #x9b #x71))

(define (get-channel-mask num-channels)
  "Return the channel mask for NUM-CHANNELS using standard speaker positions
  See: https://learn.microsoft.com/en-us/windows/win32/api/mmreg/ns-mmreg-waveformatextensible"
  (case num-channels
    ((1) #x4)        ; FC
    ((2) #x3)        ; FL | FR
    ((3) #x7)        ; FL | FR | FC
    ((4) #x33)       ; FL | FR | BL | BR (quad)
    ((5) #x607)      ; FL | FR | FC | SL | SR (5.0)
    ((6) #x60F)      ; FL | FR | FC | LFE | SL | SR (5.1)
    ((7) #x70F)      ; FL | FR | FC | LFE | BC | SL | SR (6.1)
    ((8) #x63F)      ; FL | FR | FC | LFE | BL | BR | SL | SR (7.1)
    (else #x0)))     ; Undefined

(define (build-wav-header)
  (let* ((bits-per-sample (stream-info-bits-per-sample (current-stream-info)))
         ;; Round up to next byte boundary for storage (12->16, 20->24, etc.)
         (storage-bytes (cond
                         ((<= bits-per-sample 8) 1)
                         ((<= bits-per-sample 16) 2)
                         ((<= bits-per-sample 24) 3)
                         (else 4)))
         (storage-bits (* storage-bytes 8))
         (num-channels (stream-info-channels (current-stream-info)))
         (sample-rate (stream-info-sample-rate (current-stream-info)))
         (num-samples (stream-info-samples (current-stream-info)))
         (block-alignment (* num-channels storage-bytes))
         (bytes-per-sec (* sample-rate block-alignment))
         (data-size (* num-samples block-alignment))
         ;; RIFF chunks must be word-aligned: add padding byte if data size is odd
         (padding-size (if (odd? data-size) 1 0))
         ;; Use WAVE_FORMAT_EXTENSIBLE when:
         ;; - More than 2 channels, OR
         ;; - Bits per sample differs from storage (e.g., 12-bit in 16-bit, 20-bit in 24-bit), OR
         ;; - 24-bit samples (standard practice for 24-bit files)
         (use-extensible? (or (> num-channels 2)
                              (not (= bits-per-sample storage-bits))
                              (= bits-per-sample 24))))
    (if use-extensible?
        ;; WAVE_FORMAT_EXTENSIBLE
        (bytestructure
         header-struct-extensible
         `((filetype "RIFF")
           (filesize ,(+ 60 data-size padding-size))  ; 4 (WAVE) + 8 (fmt) + 40 (fmt data) + 8 (data) + data-size + padding
           (filetype-header "WAVE")
           (format-chunk-marker "fmt ")
           (format-chunk-length 40)
           (format-type #xFFFE)
           (num-channels ,num-channels)
           (sample-freq ,sample-rate)
           (bytes/sec ,bytes-per-sec)
           (block-alignment ,block-alignment)
           (bits-per-sample ,storage-bits)
           (cb-size 22)
           (valid-bits-per-sample ,bits-per-sample)
           (channel-mask ,(get-channel-mask num-channels))
           (sub-format-guid ,pcm-subformat-guid)
           (data-chunk-header "data")
           (data-chunk-size ,data-size)))
        ;; Standard PCM
        (bytestructure
         header-struct-pcm
         `((filetype "RIFF")
           (filesize ,(+ 36 data-size padding-size))  ; 4 (WAVE) + 8 (fmt) + 16 (fmt data) + 8 (data) + data-size + padding
           (filetype-header "WAVE")
           (format-chunk-marker "fmt ")
           (format-chunk-length 16)
           (format-type #x0001)
           (num-channels ,num-channels)
           (sample-freq ,sample-rate)
           (bytes/sec ,bytes-per-sec)
           (block-alignment ,block-alignment)
           (bits-per-sample ,storage-bits)
           (data-chunk-header "data")
           (data-chunk-size ,data-size))))))

(define (write-frame)
  (let* ((bits-per-sample (assoc-ref (current-frame-header) 'bits-per-sample))
         ;; Round up to next byte boundary for storage (12->16, 20->24, etc.)
         (storage-bytes (cond
                         ((<= bits-per-sample 8) 1)
                         ((<= bits-per-sample 16) 2)
                         ((<= bits-per-sample 24) 3)
                         (else 4)))
         (storage-bits (* storage-bytes 8))
         ;; For non-byte-aligned samples, we need to left-align in the container
         ;; e.g., 12-bit samples in 16-bit container: shift left by 4
         (shift-amount (- storage-bits bits-per-sample))
         (data-bv (make-bytevector storage-bytes))
         ;; Calculate valid range based on ACTUAL bit depth
         ;; For 8-bit, samples are unsigned (0-255) in WAV, but signed in FLAC
         (max-val (if (= 8 bits-per-sample)
                      255
                      (- (expt 2 (- bits-per-sample 1)) 1)))
         (min-val (if (= 8 bits-per-sample)
                      0
                      (- (expt 2 (- bits-per-sample 1))))))
    ;; WAV format requires interleaved samples: ch0_s0, ch1_s0, ch0_s1, ch1_s1, ...
    ;; So we loop over samples first, then channels within each sample
    (do-ec (:range sample 0 (assoc-ref (current-frame-header) 'blocksize))
           (do-ec (:range channel 0 (stream-info-channels (current-stream-info)))
                  (let* ((val (array-cell-ref (current-frame-samples) channel sample))
                         ;; Convert 8-bit from signed to unsigned by adding 128
                         (adjusted-val (if (= 8 bits-per-sample) (+ val 128) val))
                         ;; Left-align the sample in its container
                         (final-val (bitwise-arithmetic-shift-left adjusted-val shift-amount)))
                    (when (or (< adjusted-val min-val) (> adjusted-val max-val))
                      (format (current-error-port) "ERROR: Sample out of range!\n")
                      (format (current-error-port) "Channel ~a, Sample ~a\n" channel sample)
                      (format (current-error-port) "Raw value: ~a\n" val)
                      (format (current-error-port) "After adjustment: ~a\n" adjusted-val)
                      (format (current-error-port) "Bits per sample: ~a (range: ~a to ~a)\n" bits-per-sample min-val max-val)
                      (format (current-error-port) "Frame header: ~s\n" (current-frame-header))
                      (error "Sample value out of range"))
                    ;; Use unsigned write for 8-bit, signed for everything else
                    (if (= 8 bits-per-sample)
                        (bytevector-u8-set! data-bv 0 final-val)
                        (bytevector-sint-set! data-bv 0 final-val (endianness little) storage-bytes))
                    (put-bytevector (current-output-port) data-bv))))))

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

(define (flac-file-frame-logger infile count)
  (parameterize ((log-port (current-output-port)))
    (with-flac-file-decoder
     infile
     (lambda ()
       (format #t "~s\n" (current-stream-info))
       (do-ec (:range frame 0 count) (read-flac-frame))))))

(define (flac-file-decode-logger infile)
  (with-flac-file-decoder
   infile
   (lambda ()
     (format #t "~s\n" (current-stream-info))
     (let frame-loop ((frame-number 0)
                      (frame (read-flac-frame)))
       (if (eof-object? frame)
           #t
           (begin
             (frame-loop (+ frame-number 1) (read-flac-frame))))))))

(define* (decode-flac-file infile outfile #:key (verify-md5 #f) (md5-on-error 'warn))
  "Decode INFILE (FLAC) to OUTFILE (WAV).

Optional keyword arguments:
  #:verify-md5 - Enable MD5 verification (default: #f)
                 #t: verify if gcrypt available (warn if not)
                 #f: don't verify
  #:md5-on-error - Action on MD5 mismatch (default: 'warn)
                   'warn: print warning to stderr
                   'error: throw error"

  ;; Determine if we should verify MD5
  (let ((should-verify? (and verify-md5
                             (if gcrypt-available?
                                 #t
                                 (begin
                                   (format (current-error-port)
                                           "WARNING: MD5 verification requested but guile-gcrypt not available~%")
                                   #f)))))

    ;; Set up MD5 error handler
    (let ((error-handler (cond
                          ((eq? md5-on-error 'warn)
                           (lambda (expected computed)
                             (format (current-error-port) "WARNING: MD5 checksum mismatch!~%")
                             (format (current-error-port) "Expected: ~a~%Computed: ~a~%" expected computed)))
                          ((eq? md5-on-error 'error)
                           (lambda (expected computed)
                             (error "MD5 checksum mismatch"
                                    (format #f "Expected: ~a~%Computed: ~a" expected computed))))
                          (else (error "Invalid md5-on-error value" md5-on-error)))))

      (parameterize ((verify-md5? should-verify?)
                     (md5-mismatch-handler error-handler))
        (with-flac-file-decoder
         infile
         (lambda ()
           (let* ((stream-info (current-stream-info))
                  (total-samples-unknown? (= 0 (stream-info-samples stream-info)))
                  (wav-header (build-wav-header))
                  ;; Open output file in read-write mode so we can seek back to update header
                  (port (open-file outfile "wb+")))
             ;; Initialize MD5 bytevector list if verification is enabled
             (when (and gcrypt-available? (verify-md5?))
               (current-md5-bytevectors '()))
             (catch #t
               (lambda ()
                 ;; Write initial header (may have size=0 if samples unknown)
                 (put-bytevector port (bytestructure-unwrap wav-header))

                 ;; Decode all frames, counting samples if needed
                 (let loop ((samples-written 0))
                   (let ((frame (read-flac-frame)))
                     (if (eof-object? frame)
                         ;; Done decoding
                         (let* ((channels (stream-info-channels stream-info))
                                (bits-per-sample (stream-info-bits-per-sample stream-info))
                                (storage-bytes (cond
                                                ((<= bits-per-sample 8) 1)
                                                ((<= bits-per-sample 16) 2)
                                                ((<= bits-per-sample 24) 3)
                                                (else 4)))
                                (final-samples (if total-samples-unknown? samples-written (stream-info-samples stream-info)))
                                (data-size (* final-samples channels storage-bytes))
                                (padding-size (if (odd? data-size) 1 0)))
                           ;; Write padding byte if data size is odd (RIFF word alignment)
                           (when (= padding-size 1)
                             (put-u8 port 0))
                           ;; Update header if samples were unknown
                           (when total-samples-unknown?
                             (let* ((use-extensible? (or (> channels 2)
                                                         (not (= bits-per-sample (* storage-bytes 8)))
                                                         (= bits-per-sample 24)))
                                    (header-overhead (if use-extensible? 60 36))
                                    (file-size (+ header-overhead data-size padding-size)))
                               ;; Seek back to beginning and rewrite header with correct sizes
                               (seek port 0 SEEK_SET)
                               ;; Update the bytestructure with actual sizes
                               (bytestructure-set! wav-header 'filesize file-size)
                               (bytestructure-set! wav-header 'data-chunk-size data-size)
                               (put-bytevector port (bytestructure-unwrap wav-header))))
                           ;; Verify MD5 if enabled
                           (when (and gcrypt-available? (verify-md5?) (current-md5-bytevectors))
                             (let* ((all-samples (apply bytevector-append (reverse (current-md5-bytevectors))))
                                    (computed-hash (compute-md5-hash all-samples))
                                    (expected-md5 (stream-info-md5 stream-info))
                                    ;; Check if expected MD5 is all zeros (means no checksum)
                                    (has-checksum? (not (every zero? (bytevector->u8-list expected-md5)))))
                               (when (and has-checksum? computed-hash)
                                 (unless (bytevector=? computed-hash expected-md5)
                                   ((md5-mismatch-handler)
                                    (bytevector->hex-string expected-md5)
                                    (bytevector->hex-string computed-hash)))))))
                         ;; Continue decoding
                         (begin
                           ;; Accumulate frame bytevector if MD5 verification enabled
                           (when (and gcrypt-available? (verify-md5?))
                             (let ((frame-data (frame-samples->bytevector)))
                               (current-md5-bytevectors (cons frame-data (current-md5-bytevectors)))))
                           (parameterize ((current-output-port port))
                             (write-frame))
                           (loop (+ samples-written (assoc-ref (current-frame-header) 'blocksize)))))))
                 (close-port port))
               (lambda (key . args)
                 (close-port port)
                 (apply throw key args))))))))))
