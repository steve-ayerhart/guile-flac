(define-module (flac metadata)
  #:use-module (flac format)
  #:use-module (flac reader)

  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs enums)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)

  #:export (flac-metadata flac-file-metadata))

(define (read-metadata-block-header)
  (make-metadata-block-header
   (= 1 (flac-read-uint 1))
   (list-ref (enum-set->list flac-metadata-type) (flac-read-uint 7))
   (flac-read-uint 24)))

(define (read-metadata-block-stream-info)
  (make-metadata-stream-info
   (flac-read-uint 16)
   (flac-read-uint 16)
   (flac-read-uint 24)
   (flac-read-uint 24)
   (flac-read-uint 20)
   (+ 1 (flac-read-uint 3))
   (+ 1 (flac-read-uint 5))
   (flac-read-uint 36)
   (flac-read-bytes 16)))

(define (read-metadata-block-seek-table length)
  (make-metadata-seek-table
   (map (λ (_)
          (make-metadata-seek-point
           (flac-read-uint 64)
           (flac-read-uint 64)
           (flac-read-uint 16)))
        (iota (/ length 18)))))

(define (read-metadata-block-vorbis-comment)
  (define (read-native-u32) (bytevector-u32-native-ref (flac-read-bytes 4) 0))
  (make-metadata-vorbis-comment
   (utf8->string (flac-read-bytes (read-native-u32)))
   (map (λ (_) (string-split (utf8->string (flac-read-bytes (read-native-u32))) #\=)) (iota (read-native-u32)))))

(define (read-metadata-block-padding length)
  (flac-read-uint length)
  (make-metadata-padding length))

(define (read-metadata-block-picture)
  (make-metadata-picture
   (list-ref (enum-set->list flac-picture-type) (flac-read-uint 32))
   (utf8->string (flac-read-bytes (flac-read-uint 32)))
   (utf8->string (flac-read-bytes (flac-read-uint 32)))
   (flac-read-uint 32)
   (flac-read-uint 32)
   (flac-read-uint 32)
   (flac-read-uint 32)
   (flac-read-bytes (flac-read-uint 32))))

(define (read-metadata-block metadata length type)
  (match type
    ('stream-info (set-flac-metadata-stream-info! metadata (read-metadata-block-stream-info)))
    ('seek-table (set-flac-metadata-seek-table! metadata (read-metadata-block-seek-table length)))
    ('vorbis-comment (set-flac-metadata-vorbis-comment! metadata (read-metadata-block-vorbis-comment)))
    ('picture (set-flac-metadata-pictures!
               metadata
               (cons (read-metadata-block-picture)
                     (flac-metadata-pictures metadata))))
    ('padding (set-flac-metadata-padding! metadata (read-metadata-block-padding length))))
  metadata)

(define (read-flac-metadata)
  (flac-read/assert-magic)
  (let metadata-loop ((metadata (make-flac-metadata #f #f #f #f #f #f '()))
                      (header (read-metadata-block-header)))
    (if (metadata-block-header-last? header)
        (read-metadata-block metadata (metadata-block-header-length header) (metadata-block-header-type header))
        (metadata-loop (read-metadata-block
                        metadata
                        (metadata-block-header-length header)
                        (metadata-block-header-type header))
                       (read-metadata-block-header)))))

                                        ; FIXME: bail early if not in type
(define (read-flac-metadata-type type)
  (flac-read/assert-magic)
  (let metadata-loop ((header (read-metadata-block-header)))
    (if (or (metadata-block-header-last? header)
            (equal? type (metadata-block-header-type header)))
        (match type
          ('stream-info (read-metadata-block-stream-info))
          ('vorbis-comment (read-metadata-block-vorbis-comment))
          (_ #f))
        (begin
          (flac-read-bytes (metadata-block-header-length header))
          (metadata-loop (read-metadata-block-header))))))

(define* (flac-metadata port #:optional (type #f))
  (with-flac-input-port port
   (λ ()
     (if (symbol? type)
        (read-flac-metadata-type type)
        (read-flac-metadata)))))

(define* (flac-file-metadata filename #:optional (type #f))
  (with-flac-input-port (open-input-file filename #:binary #t)
   (λ () (flac-metadata (current-input-port) type)) #:binary #t))
