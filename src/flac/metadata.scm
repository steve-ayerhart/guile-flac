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

(define (metadata-type-number->symbol type-num)
  "Convert a metadata block type number to a symbol. Types 7-127 are invalid/reserved."
  (if (< type-num 7)
      (list-ref (enum-set->list flac-metadata-type) type-num)
      'invalid))

(define-public (read-metadata-block-header)
  (values
   (= 1 (flac-read-uint 1))
   (metadata-type-number->symbol (flac-read-uint 7))
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

(define (read-metadata-block-application block-length)
  (%make-metadata-application
   (flac-read-uint 32)
   (flac-read-bytes (- block-length 4))))

(define (read-cuesheet-index)
  (let ((offset (flac-read-uint 64))
        (number (flac-read-uint 8)))
    (flac-read-uint 24) ; reserved, discard
    (%make-metadata-cuesheet-index offset number)))

(define (read-cuesheet-track)
  (let* ((offset (flac-read-uint 64))
         (number (flac-read-uint 8))
         (isrc (utf8->string (flac-read-bytes 12)))
         (type-bit (flac-read-uint 1))
         (type (if (= type-bit 0) 'audio 'non-audio))
         (pre-emphasis (= 1 (flac-read-uint 1))))
    (flac-read-uint 110) ; reserved bits (6 + 13*8), discard
    (let* ((index-count (flac-read-uint 8))
           (indices (list-ec (: _ index-count) (read-cuesheet-index))))
      (%make-metadata-cuesheet-track offset number isrc type pre-emphasis indices))))

(define (read-metadata-block-cuesheet)
  (let ((catalog (utf8->string (flac-read-bytes 128)))
        (lead-in (flac-read-uint 64))
        (cd? (= 1 (flac-read-uint 1))))
    (flac-read-uint 2071) ; reserved bits (7 + 258*8), discard
    (let* ((track-count (flac-read-uint 8))
           (tracks (list-ec (: _ track-count) (read-cuesheet-track))))
      (%make-metadata-cuesheet catalog lead-in cd? tracks))))

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
    ('cuesheet (read-metadata-block-cuesheet))
    ('application (read-metadata-block-application block-length))
    ('padding (read-metadata-block-padding block-length))
    ('invalid (begin (flac-read-bytes block-length) #f))
    ('skip (begin (flac-read-bytes block-length) #f))))

(define (resolve-block-type search-type block-type)
  "Determine whether to read or skip a metadata block based on search-type."
  (if (and (symbol? search-type) (not (equal? search-type block-type)))
      'skip
      block-type))

(define* (read-flac-metadata #:optional (search-type #f))
  "Read all metadata blocks from the FLAC stream and return them as a list.
   The list is in the order they appear in the file (stream-info first, per spec).
   If search-type is provided, only that metadata type will be read (others skipped)."
  (let metadata-loop ((metadata-list '()))
    (receive (last-block? block-type block-length)
        (read-metadata-block-header)
      (let ((block (read-metadata-block (resolve-block-type search-type block-type) block-length)))
        (if last-block?
            (reverse (if block (cons block metadata-list) metadata-list))
            (metadata-loop (if block (cons block metadata-list) metadata-list)))))))

(define* (flac-file-metadata filename #:optional (search-type #f))
  (with-flac-input-port
   (open-input-file filename #:binary #t)
   (λ ()
     (flac-read/assert-magic)
     (read-flac-metadata search-type))))

(define  (predicate-lookup symbol)
  (match symbol
    ('stream-info metadata-stream-info?)
    ('picture metadata-picture?)
    ('seek-table metadata-seek-table?)
    ('padding metadata-padding?)
    ('vorbis-comment metadata-vorbis-comment?)
    ('cuesheet metadata-cuesheet?)
    ('application metadata-application?)
    (_ #f)))

(define (filter-metadata metadata-list filter-type)
  "Filter metadata list by type (e.g., 'picture, 'vorbis-comment, etc.)."
  (filter (predicate-lookup filter-type) metadata-list))

(define (find-metadata metadata-list search-type)
  "Find first metadata block of given type (e.g., 'stream-info, 'picture, etc.)."
  (find (predicate-lookup search-type) metadata-list))
