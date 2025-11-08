(define-module (flac)
  #:use-module (flac reader)
  #:use-module (flac decoder)
  #:use-module (flac metadata)
  #:use-module (flac format)

  #:use-module (srfi srfi-42)

  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)

  #:use-module (gcrypt hash)

  #:export (decode-flac-file
            with-flac-file-decoder
            flac-file-decode-logger
            flac-file-frame-logger))

(define header-struct
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

(define (build-wav-header)
  (bytestructure
   header-struct
   `((filetype "RIFF")
     (filesize ,(+ 36 (* (stream-info-samples (current-stream-info))
                         (stream-info-channels (current-stream-info))
                         (floor-quotient (stream-info-bits-per-sample (current-stream-info)) 8))))
     (filetype-header "WAVE")
     (format-chunk-marker "fmt ")
     (format-chunk-length 16)
     (format-type #x0001)
     (num-channels ,(stream-info-channels (current-stream-info)))
     (sample-freq ,(stream-info-sample-rate (current-stream-info)))
     (bytes/sec ,(* (stream-info-sample-rate (current-stream-info))
                    (stream-info-channels (current-stream-info))
                    (floor-quotient (stream-info-bits-per-sample (current-stream-info)) 8)))
     (block-alignment ,(* (stream-info-channels (current-stream-info))
                          (floor-quotient (stream-info-bits-per-sample (current-stream-info)) 8)))
     (bits-per-sample ,(stream-info-bits-per-sample (current-stream-info)))
     (data-chunk-header "data")
     (data-chunk-size ,(* (stream-info-samples (current-stream-info))
                          (stream-info-channels (current-stream-info))
                          (floor-quotient (stream-info-bits-per-sample (current-stream-info)) 8))))))

(define (write-frame)
  (let* ((total-bytes (floor-quotient (frame-header-bits-per-sample (current-frame-header)) 8))
         (addend (if (= 8 (frame-header-bits-per-sample (current-frame-header))) 128 0))
         (data-bv (make-bytevector total-bytes)))
    ;; WAV format requires interleaved samples: ch0_s0, ch1_s0, ch0_s1, ch1_s1, ...
    ;; So we loop over samples first, then channels within each sample
    (do-ec (:range sample 0 (frame-header-blocksize (current-frame-header)))
           (do-ec (:range channel 0 (stream-info-channels (current-stream-info)))
                  (let ((val (array-cell-ref (current-frame-samples) channel sample))
                        (final-val (+ addend (array-cell-ref (current-frame-samples) channel sample))))
                    (when (or (< final-val -32768) (> final-val 32767))
                      (format (current-error-port) "ERROR: Sample out of range!\n")
                      (format (current-error-port) "Channel ~a, Sample ~a\n" channel sample)
                      (format (current-error-port) "Raw value: ~a\n" val)
                      (format (current-error-port) "With addend (~a): ~a\n" addend final-val)
                      (format (current-error-port) "Frame header: ~s\n" (current-frame-header))
                      (error "Sample value out of range"))
                    (bytevector-sint-set! data-bv 0 final-val (endianness little) total-bytes)
                    (put-bytevector (current-output-port) data-bv))))))

(define (with-flac-file-decoder infile thunk)
  (with-input-from-file infile
    (lambda ()
      (with-flac-input-port
       (current-input-port)
       (lambda ()
         (flac-read/assert-magic)
         (with-initialized-decoder
          (flac-metadata-stream-info (read-flac-metadata))
          thunk))))
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
           (let* ((channels (stream-info-channels (current-stream-info)))
                  (blocksize (frame-header-blocksize (current-frame-header)))
                  (sample-bytes (floor-quotient (frame-header-bits-per-sample (current-frame-header)) 8))
                  (samples (list-ec (:range sample 0 blocksize)
                                    (:range channel 0 channels)
                                    (array-cell-ref (current-frame-samples) channel sample)))
                  (thing (sint-list->bytevector samples (endianness little) sample-bytes))
                  (frame-md5 (bytevector-hash (sint-list->bytevector samples (endianness little) sample-bytes) (hash-algorithm md5))))
             thing))))))

(define (decode-flac-file infile outfile)
  (let ((old-output (current-output-port)))
    (with-flac-file-decoder
     infile
     (lambda ()
       (let ((wav-header (build-wav-header)))
         (with-output-to-file outfile
           (lambda ()
             (put-bytevector (current-output-port) (bytestructure-unwrap wav-header))
             (while (not (eof-object? (read-flac-frame)))
               (write-frame)))))))))
