(define-module (flac metadata)
  #:use-module (flac format)
  #:use-module (flac reader)

  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)

  #:use-module (rnrs enums)
  #:use-module (rnrs bytevectors)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-42)

  #:export (read-flac-metadata flac-file-metadata filter-metadata find-metadata))

(define-public (read-metadata-block-header)
  (values
   (= 1 (flac-read-uint 1))
   (list-ref (enum-set->list flac-metadata-type) (flac-read-uint 7))
   (flac-read-uint 24)))

(define (read-metadata-block-stream-info)
  (%make-metadata-stream-info
   (flac-read-uint 16)
   (flac-read-uint 16)
   (flac-read-uint 24)
   (flac-read-uint 24)
   (flac-read-uint 20)
   (+ 1 (flac-read-uint 3))
   (+ 1 (flac-read-uint 5))
   (flac-read-uint 36)
   (flac-read-bytes 16)))

(define (read-metadata-block-seek-table block-length)
  (%make-metadata-seek-table
   (list-ec
    (: _ (/ block-length 18))
    (%make-metadata-seek-point
     (flac-read-uint 64)
     (flac-read-uint 64)
     (flac-read-uint 16)))))

(define (read-metadata-block-vorbis-comment)
  (define (read-native-u32) (bytevector-u32-native-ref (flac-read-bytes 4) 0))
  (%make-metadata-vorbis-comment
   (utf8->string (flac-read-bytes (read-native-u32)))
   (list-ec
    (: _ (read-native-u32))
    (string-split (utf8->string (flac-read-bytes (read-native-u32))) #\=))))

(define (read-metadata-block-padding block-length)
  (flac-read-bytes block-length)
  (%make-metadata-padding block-length))

(define (read-metadata-block-picture)
  (%make-metadata-picture
   (list-ref (enum-set->list flac-picture-type) (flac-read-uint 32))
   (utf8->string (flac-read-bytes (flac-read-uint 32)))
   (utf8->string (flac-read-bytes (flac-read-uint 32)))
   (flac-read-uint 32)
   (flac-read-uint 32)
   (flac-read-uint 32)
   (flac-read-uint 32)
   (flac-read-bytes (flac-read-uint 32))))

(define (read-metadata-block block-type block-length)
  (match block-type
    ('stream-info (read-metadata-block-stream-info))
    ('seek-table (read-metadata-block-seek-table block-length))
    ('vorbis-comment (read-metadata-block-vorbis-comment))
    ('picture (read-metadata-block-picture))
    ('cuesheet (flac-read-bytes block-length))
    ('application (flac-read-bytes block-length))
    ('padding (read-metadata-block-padding block-length))
    ('skip (begin (flac-read-bytes block-length) #f))))

(define (resolve-block-type search-type block-type)
  (if (symbol? search-type)
      (if (equal? search-type block-type) block-type 'skip)
      block-type))

(define* (read-flac-metadata #:optional (search-type #f))
  (let metadata-loop ((flac-metadata %empty-flac-metadata))
    (receive (last-block? block-type block-length)
        (read-metadata-block-header)
      (if last-block?
          (add-metadata
           flac-metadata
           (read-metadata-block (resolve-block-type search-type block-type) block-length))
          (metadata-loop
           (add-metadata flac-metadata (read-metadata-block (resolve-block-type search-type block-type) block-length)))))))

(define* (flac-file-metadata filename #:optional (search-type #f))
  (with-flac-input-port
   (open-input-file filename #:binary #t)
   (λ ()
     (flac-read/assert-magic)
     (read-flac-metadata search-type))))

(define  (predicate-lookup symbol)
  (match symbol
    ('picture metadata-picture?)
    ('seek-table metadata-seek-table?)
    ('padding metadata-padding?)
    ('vorbis-comment metadata-vorbis-comment?)
    ('cuesheet metadata-cuesheet?)
    (_ #f)))

(define (filter-metadata flac-metadata filter-type)
  (filter (predicate-lookup filter-type) (flac-metadata-metadata flac-metadata)))

(define (find-metadata flac-metadata search-type)
  (find (predicate-lookup search-type) (flac-metadata-metadata flac-metadata)))
