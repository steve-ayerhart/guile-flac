(define-module (flac stream)
  #:use-module (flac format)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 binary-ports))

(define-class <stream> ()
  (metadata #:getter metadata #:init-value '() #:init-keyword #:metadata)
  (frames #:getter frames #:init-value '() #:init-keyword #:frames))

(define-class <file-stream> (<stream>)
  (port #:class <protected-slot>))


;;; TODO: make sure other formats are handled gracefully
;;; such as ID3 and WAV
(define (read-magic port)
  (let ((magic? (get-bytevector-n port flac-stream-sync-length)))
    magic?))

(define (read-metadata port)
  (let read-metadata-block ((metadata '()))
    (receive (last? type length)
        (parse-metadata-header (get-bytevector-n port 4))
      (if last?
          (append! metadata (list (parse-metadata-block type (get-bytevector-n port length))))
          (read-metadata-block
           (append! metadata (list (parse-metadata-block type (get-bytevector-n port length)))))))))

(define (read-frames port)
  '(frames))

(define (parse-frame-header bv)
  '(header))

(define (read-stream port)
  (read-magic port)
  (make <stream>
    #:metadata (read-metadata port)
    #:frames (read-frames port)))

(define (parse-metadata-block type data)
  (eval `(,(string->symbol (string-append "parse-" (symbol->string type))) ,data) (current-module)))

(define (parse-metadata-header bv)
  (let ((header (bytevector-u32-ref bv 0 'big)))
    (values
     (= 1 (bit-extract header 31 32))
     (match (bit-extract header 24 31)
       (0 'stream-info)
       (1 'padding)
       (2 'application)
       (3 'seek-table)
       (4 'vorbis-comment)
       (5 'cue-sheet)
       (6 'picture)
       (else 'invalid))
     (bit-extract header 0 24))))

(define (parse-invalid bv)
  'invalid)

(define-record-type <flac-padding>
  (make-flac-padding length)
  flac-padding?
  (length padding-length))

(define (parse-padding bv)
  (make-flac-padding (bytevector-length bv)))

(define (parse-application bv)
  ; we dont' do anything with application data yet
  (make <flac-application>
    #:id (utf8->string (bytevector-uint-ref bv 0 'big 4))))

(define (parse-seek-table bv)
  'seek-table)

(define (parse-vorbis-comment bv)
  'vorbis-comment)

(define (parse-cue-sheet bv)
  'cue-sheet)

(define (parse-picture bv)
  'picture)

(define (parse-stream-info bv)
  (let ((sample/channel-data (bytevector-uint-ref bv 10 'big 8)))
    (make-flac-stream-info
     (bytevector-uint-ref bv 0 'big 2)
     (bytevector-uint-ref bv 2 'big 2)
     (bytevector-uint-ref bv 4 'big 3)
     (bytevector-uint-ref bv 7 'big 3)
     (bit-extract sample/channel-data 44 65)
     (+ 1 (bit-extract sample/channel-data 41 44))
     (bit-extract sample/channel-data 0 36)
     (+ 1 (bit-extract sample/channel-data 36 41))
     (number->string (bytevector-uint-ref bv 18 'big 16) 16))))

(define (parse-seek-table bv)
  (let ((seek-table-length (bytevector-length bv)))
    (let parse-seek-point ((seek-points '())
                           (bytes-read 0))
      (if (= seek-table-length bytes-read)
          (make-flac-seek-table seek-points)
          (parse-seek-point
           (append! seek-points
                    (list (make-flac-seek-point
                           (bytevector-uint-ref bv bytes-read 'big 8)
                           (bytevector-uint-ref bv (+ bytes-read 8) 'big 8)
                           (bytevector-uint-ref bv (+ bytes-read 16) 'big 2))))
           (+ 18 bytes-read))))))


;; TODO: actually verify content vector format
;; https://www.xiph.org/vorbis/doc/v-comment.html
(define comment-content-rx (make-regexp "^([ [:alnum:][:punct:]]*)=(.*)$" regexp/newline))
(define (parse-vorbis-comment-content bytestring)
  (let ((comment-match (regexp-exec comment-content-rx (utf8->string bytestring))))
    (when comment-match
      (cons (string-upcase (match:substring comment-match 1))
            (match:substring comment-match 2)))))

(define (parse-vorbis-comment bv)
  (let* ((comments-length (bytevector-length bv))
         (vendor-string-length (bytevector-uint-ref bv 0 'little 4))
         (num-comments (bytevector-uint-ref bv (+ 4 vendor-string-length) 'little 4)))
    (let parse-comment ((bytes-read (+ 8 vendor-string-length))
                        (comments '()))
      (if (= comments-length bytes-read)
          (let ((vendor-bytestring (make-bytevector vendor-string-length)))
            (bytevector-copy! bv 4 vendor-bytestring 0 vendor-string-length)
            (make-flac-vorbis-comment
             (utf8->string vendor-bytestring)
             comments))
          (let* ((comment-length (bytevector-uint-ref bv bytes-read 'little 4))
                 (comment-bytestring (make-bytevector comment-length)))
            (bytevector-copy! bv (+ 4 bytes-read) comment-bytestring 0 comment-length)
            (parse-comment (+ 4 comment-length bytes-read)
                           (cons (parse-vorbis-comment-content comment-bytestring) comments)))))))
