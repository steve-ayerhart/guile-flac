(define-module (flac)
  #:use-module (flac reader)
  #:use-module (flac decoder)
  #:use-module (flac metadata)
  #:use-module (flac format)

  #:use-module (srfi srfi-42)

  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)

  #:export (decode-flac-file with-flac-file-decoder))

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
    (do-ec (:range channel 0 (stream-info-channels (current-stream-info)))
           (do-ec (:range sample 0 (frame-header-blocksize (current-frame-header)))
                  (begin
                    (bytevector-sint-set! data-bv 0 (+ addend (array-cell-ref (current-frame-samples) channel sample)) (endianness little) total-bytes)
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
