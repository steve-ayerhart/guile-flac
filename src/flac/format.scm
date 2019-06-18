(define-module (flac format)
  #:use-module (flac bindings)

  #:use-module (bytestructures guile)
  #:use-module ((system foreign)
                #:renamer (symbol-prefix-proc 'ffi:))
  #:use-module (rnrs enums)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (oop goops)


  #:export (flac-max-metadata-type-code
            flac-min-block-size
            flac-max-block-size
            flac-subset-max-block-size
            flac-max-channels
            flac-min-bits-per-sample
            flac-max-bits-per-sample
            flac-reference-codec-max-bits-per-sample
            flac-max-sample-rate
            flac-max-lpc-order
            flac-subset-max-lpc-order-4800hz
            flac-min-qlp-coeff-precision
            flac-max-qlp-coeff-precision
            flac-max-fixed-order
            flac-max-rice-partition-order
            flac-subset-max-rice-partition-order
            <stream-info>
            min-block-size max-block-size min-frame-size max-frame-size
            sample-rate channels bits-per-sample total-samples md5sum
            <vorbis-comment>
            vendor-string comments))

(define flac-max-metadata-type-code 126)
(define flac-min-block-size 16)
(define flac-max-block-size 65535)
(define flac-subset-max-block-size 4608)
(define flac-max-channels 8)
(define flac-min-bits-per-sample 4)
(define flac-max-bits-per-sample 32)
(define flac-reference-codec-max-bits-per-sample 24)
(define flac-max-sample-rate 655350)
(define flac-max-lpc-order 32)
(define flac-subset-max-lpc-order-4800hz 12)
(define flac-min-qlp-coeff-precision 5)
(define flac-max-qlp-coeff-precision 15)
(define flac-max-fixed-order 4)
(define flac-max-rice-partition-order 15)
(define flac-subset-max-rice-partition-order 8)

(define entropy-coding-method-type-enum
  (make-enumeration
   '(partinitioned-rice
     partitioned-rice-2)))

(define entropy-coding-method-type-enum-index
  (enum-set-indexer entropy-coding-method-type-enum))

(define metadata-type-enum
  (make-enumeration
   '(stream-info
     padding
     application
     seek-table
     vorbis-comment
     cuesheet
     picture
     undefined)))

(define metadata-type-enum-index
  (enum-set-indexer metadata-type-enum))

(define-class <stream-metadata-class> (<class>)
  (type #:class <protected-slot>))

(eval-when (expand load eval)
  (define stream-info-struct
    (bs:struct
     `((min-block-size ,unsigned-int)
       (max-block-size ,unsigned-int)
       (min-frame-size ,unsigned-int)
       (max-frame-size ,unsigned-int)
       (sample-rate ,unsigned-int)
       (channels ,unsigned-int)
       (bits-per-sample ,unsigned-int)
       (total-samples ,uint64)
       (md5sum ,(bs:vector 16 uint8)))))

  (define padding-struct
    (bs:struct
     `((dummy ,int))))

  (define application-struct
    (bs:struct
     `((id ,uint32be)
       (data ,(bs:pointer uint8)))))

  (define seek-point-struct
    (bs:struct
     `((sample-number ,uint64)
       (stream-offset ,uint64)
       (frame-samples ,unsigned-int))))

  (define seek-table-struct
    (bs:struct
     `((num-pointer ,unsigned-int)
       (points ,(bs:pointer seek-point-struct)))))

  (define vorbis-comment-entry-struct
    (bs:struct
     `((length ,uint32)
       (entry ,(bs:pointer uint8)))))

  (define vorbis-comment-struct
    (bs:struct
     `((vendor-string ,vorbis-comment-entry-struct)
       (num-comments ,uint32)
       (comments ,(bs:pointer uint8)))))

  (define metadata-data-union
    (bs:union
     `((stream-info ,stream-info-struct)
       (padding ,padding-struct)
       (application ,application-struct)
       (seek-table ,seek-table-struct)
       (vorbis-comment ,vorbis-comment-struct))))

  (define metadata-struct
    (bs:struct
     `((type ,int)
       (last? ,int)
       (length ,unsigned-int)
       (data ,metadata-data-union)))))

(define-class <stream-metadata> ()
  instance
  #:metaclass <stream-metadata-class>)

(define-method (initialize (c <stream-metadata-class>) args)
  (let ((type-name (or (get-keyword #:type args #f)
                       (error "NOOO"))))
    (slot-set! c 'type (metadata-type-enum-index type-name))
    (next-method)))

(define-method (allocate-instance (c <stream-metadata-class>) args)
  (let* ((allocation (next-method))
         (struct-size (bytestructure-descriptor-size metadata-struct)))
    (slot-set! allocation 'instance (ffi:pointer->bytevector
                                     (%metadata_object_new (slot-ref c 'type))
                                     struct-size))
    allocation))

(define-bytestructure-accessors
  metadata-struct unwrap-metadata-struct metadata-struct-ref metadata-struct-set!)

(define-class <stream-info> (<stream-metadata>) #:type 'stream-info)
(define-method (min-block-size (stream-info <stream-info>))
  (metadata-struct-ref (slot-ref stream-info 'instance) data stream-info min-block-size))
(define-method (max-block-size (stream-info <stream-info>))
  (metadata-struct-ref (slot-ref stream-info 'instance) data stream-info max-block-size))
(define-method (min-fame-size (stream-info <stream-info>))
  (metadata-struct-ref (slot-ref stream-info 'instance) data stream-info min-frame-size))
(define-method (max-frame-size (stream-info <stream-info>))
  (metadata-struct-ref (slot-ref stream-info 'instance) data stream-info max-frame-size))
(define-method (sample-rate (stream-info <stream-info>))
  (metadata-struct-ref (slot-ref stream-info 'instance) data stream-info sample-rate))
(define-method (channels (stream-info <stream-info>))
  (metadata-struct-ref (slot-ref stream-info 'instance) data stream-info channels))
(define-method (bits-per-sample (stream-info <stream-info>))
  (metadata-struct-ref (slot-ref stream-info 'instance) data stream-info bits-per-sample))
(define-method (total-samples (stream-info <stream-info>))
  (metadata-struct-ref (slot-ref stream-info 'instance) data stream-info total-samples))
(define-method (md5sum (stream-info <stream-info>))
  (string-concatenate
   (map (λ (index)
          (format #f "~2,'0x" (metadata-struct-ref
                               (slot-ref stream-info 'instance) data stream-info md5sum index)))
       (iota 16))))

(define-class <application> (<stream-metadata>)
  data
  #:type 'application)

(define-class <padding> (<stream-metadata>)
  #:type 'padding)

(define-class <vorbis-comment> (<stream-metadata>)
  comment
  #:type 'vorbis-comment)

(define-method (vendor-string (vorbis-comment <vorbis-comment>))
  (let* ((entry-length (metadata-struct-ref
                        (slot-ref vorbis-comment 'instance) data vorbis-comment vendor-string length))
         (entry (ffi:make-pointer
                 (metadata-struct-ref (slot-ref vorbis-comment 'instance) data vorbis-comment vendor-string entry))))
    (bytevector->string (ffi:pointer->bytevector entry entry-length) "utf-8")))
