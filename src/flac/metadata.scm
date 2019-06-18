(define-module (flac metadata)
  #:use-module (flac config)
  #:use-module (flac bindings)
  #:use-module (flac format)

  #:use-module (oop goops)
  #:use-module (system foreign-object)
  #:use-module (system foreign)
  #:use-module (rnrs enums)
  #:use-module (rnrs bytevectors))

(define (get-stream-info path)
  (let ((stream-info (make <stream-info>)))
    (if (= 1 (%metadata_get_streaminfo (string->pointer path)
                                       (bytevector->pointer (slot-ref stream-info 'instance))))
        stream-info
        (error "NOOOOO"))))

(define (get-tags path)
  (let* ((vorbis-comment (make <vorbis-comment>))
        (tags-pointer (scm->pointer (bytevector->pointer (slot-ref vorbis-comment 'instance)))))
    (if (= 1 (%metadata_get_tags (string->pointer path)
                                 tags-pointer))
        (values vorbis-comment tags-pointer (dereference-pointer tags-pointer))
        (error "NOO"))))
