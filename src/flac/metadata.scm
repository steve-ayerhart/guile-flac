(define-module (flac metadata)
  #:use-module (flac format)
  #:use-module (flac reader)

  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (rnrs enums)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)

  #:export (read-flac-metadata flac-metadata flac-file-metadata))

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

(define (read-metadata-block-seek-table length)
  (%make-metadata-seek-table
   (map (λ (_)
          (%make-metadata-seek-point
           (flac-read-uint 64)
           (flac-read-uint 64)
           (flac-read-uint 16)))
        (iota (/ length 18)))))

(define (read-metadata-block-vorbis-comment)
  (define (read-native-u32) (bytevector-u32-native-ref (flac-read-bytes 4) 0))
  (%make-metadata-vorbis-comment
   (utf8->string (flac-read-bytes (read-native-u32)))
   (map (λ (_) (string-split (utf8->string (flac-read-bytes (read-native-u32))) #\=)) (iota (read-native-u32)))))

(define (read-metadata-block-padding length)
  (flac-read-bytes length)
  (%make-metadata-padding length))

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

(define (add-picture! metadata)
  (if (flac-metadata-pictures metadata)
      (set-flac-metadata-pictures!
       metadata
       (cons (read-metadata-block-picture)
             (flac-metadata-pictures metadata)))
      (set-flac-metadata-pictures!
       metadata
       (list (read-metadata-block-picture)))))

(define (read-metadata-block metadata length type)
  (match type
    ('stream-info (set-flac-metadata-stream-info! metadata (read-metadata-block-stream-info)))
    ('seek-table (set-flac-metadata-seek-table! metadata (read-metadata-block-seek-table length)))
    ('vorbis-comment (set-flac-metadata-vorbis-comment! metadata (read-metadata-block-vorbis-comment)))
    ('picture (add-picture! metadata))
    ('cuesheet (set-flac-metadata-cuesheet! metadata #f))
    ('application (set-flac-metadata-application! metadata #f))
    ('padding (set-flac-metadata-padding! metadata (read-metadata-block-padding length))))
  metadata)

(define (read-flac-metadata)
  (let metadata-loop ((metadata (%make-flac-metadata #f #f #f #f #f #f #f)))
    (receive (last-block? block-type block-length)
        (read-metadata-block-header)
      (if last-block?
          (read-metadata-block metadata block-length block-type)
          (metadata-loop (read-metadata-block metadata block-length block-type))))))

;;; FIXME: bail early if not in type
(define (read-flac-metadata-type type)
  (let metadata-loop ((metadata (%make-flac-metadata #f #f #f #f #f #f #f)))
    (receive (last-block? block-type block-length)
        (read-metadata-block-header)
      (if (or last-block? (equal? type block-type))
          (match type
            ('stream-info (set-flac-metadata-stream-info! metadata (read-metadata-block-stream-info)))
            ('vorbis-comment (set-flac-metadata-vorbis-comment! metadata (read-metadata-block-vorbis-comment)))
            ('picture (set-flac-metadata-pictures! metadata (read-metadata-block-picture)))
            ('seek-table (set-flac-metadata-seek-table! metadata (read-metadata-block-seek-table)))
            ('cuesheet (set-flac-metadata-cuesheet! metadata #f))
            (_ #f))
          (begin
            (flac-read-bytes block-length)
            (metadata-loop metadata))))))

(define* (flac-metadata port #:optional (type #f))
  (with-flac-input-port
   port
   (λ ()
     (if (symbol? type)
         (read-flac-metadata-type type)
         (read-flac-metadata)))))

(define* (flac-file-metadata filename #:optional (type #f))
  (with-flac-input-port
   (open-input-file filename #:binary #t)
   (λ ()
     (flac-read/assert-magic)
     (flac-metadata (current-input-port) type))))
