(define-module (flac)
  #:use-module (flac reader)
  #:use-module (flac decoder)
  #:use-module (flac metadata)
  #:use-module (flac format)

  #:use-module (bytestructures guile)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)

  #:export (decode-flac-file))

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

(define (write-frame frame channels)
  (let* ((header (frame-header frame))
         (total-bytes (floor-quotient (frame-header-bits-per-sample header) 8))
         (addend (if (= 8 (frame-header-bits-per-sample header)) 128 0))
         (data-bv (make-bytevector total-bytes)))
    (for-each
     (lambda (block)
       (for-each
        (lambda (channel)
          (bytevector-sint-set! data-bv
                               0
                               (+ addend (list-ref (list-ref (frame-samples frame) channel) block))
                               (endianness little)
                               total-bytes)
          (put-bytevector (current-output-port) data-bv))
        (iota channels)))
     (iota (frame-header-blocksize header)))))

(define (decode-flac-file infile outfile)
  (let ((old-output (current-output-port)))
    (with-input-from-file infile
      (λ ()
        (with-flac-input-port
         (current-input-port)
         (λ ()
           (let* ((stream-info (flac-metadata-stream-info (read-flac-metadata)))
                  (sample-data-length (* (stream-info-samples stream-info)
                                         (stream-info-channels stream-info)
                                         (floor-quotient (stream-info-bits-per-sample stream-info) 8)))
                  (wav-header (bytestructure
                               header-struct
                               `((filetype "RIFF")
                                 (filesize ,(+ 36 (* (stream-info-samples stream-info)
                                                    (stream-info-channels stream-info)
                                                    (floor-quotient (stream-info-bits-per-sample stream-info) 8))))
                                 (filetype-header "WAVE")
                                 (format-chunk-marker "fmt ")
                                 (format-chunk-length 16)
                                 (format-type #x0001)
                                 (num-channels ,(stream-info-channels stream-info))
                                 (sample-freq ,(stream-info-sample-rate stream-info))
                                 (bytes/sec ,(* (stream-info-sample-rate stream-info)
                                               (stream-info-channels stream-info)
                                               (floor-quotient (stream-info-bits-per-sample stream-info) 8)))
                                 (block-alignment ,(* (stream-info-channels stream-info)
                                                     (floor-quotient (stream-info-bits-per-sample stream-info) 8)))
                                 (bits-per-sample ,(stream-info-bits-per-sample stream-info))
                                 (data-chunk-header "data")
                                 (data-chunk-size ,(* (stream-info-samples stream-info)
                                                     (stream-info-channels stream-info)
                                                     (floor-quotient (stream-info-bits-per-sample stream-info) 8)))))))
             (with-output-to-file outfile
               (lambda ()
                 (put-bytevector (current-output-port) (bytestructure-unwrap wav-header))
                 (let frame-loop ((frame-number 0)
                                  (frame (read-flac-frame stream-info)))
                   (if (= (stream-info-samples stream-info) frame-number)
                       #t
                       (begin
                         (write-frame frame (stream-info-channels stream-info))
                         (frame-loop (+ 1 frame-number)
                                     (read-flac-frame stream-info))))))))))))))
