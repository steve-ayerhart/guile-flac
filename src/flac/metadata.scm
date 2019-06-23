(define-module (flac metadata)
  #:use-module (flac config)

  #:use-module (oop goops)
  #:use-module (system foreign-object)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs enums)

  #:export (<stream-info>
            min-block-size
            max-block-size
            min-frame-size
            max-frame-size
            sample-rate
            channels
            bits-per-sample
            total-samples
            md5sum))

(define-class <stream-metadata-class> (<class>)
  (type #:class <protected-slot>))

(define-class <stream-metadata> ()
  (instance #:class <read-only-slot>)
  #:metaclass <stream-metadata-class>)

(eval-when (eval load compile)
  (dynamic-call "guile_flac_metadata_init"
                (dynamic-link (extensions-library "flac"))))

(eval-when (eval load)
  (%protect-stream-metadata-class! <stream-metadata>))

(define-method (initialize (self <stream-metadata-class>) args)
  (%initialize-stream-metadata-class! self args)
  (next-method))

(define-method (allocate-instance (self <stream-metadata-class>) args)
  (let ((instance (next-method)))
    (%allocate-stream-metadata-class self instance)))

(define-class <stream-info> (<stream-metadata>)
  (min-block-size #:allocation #:virtual
                  #:accessor min-block-size
                  #:slot-ref %stream-info-get-min-block-size
                  #:slot-set! (λ (a b) #f))

  (max-block-size #:allocation #:virtual
                  #:accessor max-block-size
                  #:slot-ref %stream-info-get-max-block-size
                  #:slot-set! (λ (a b) #f))

  (min-frame-size #:allocation #:virtual
                  #:accessor min-frame-size
                  #:slot-ref %stream-info-get-min-frame-size
                  #:slot-set! (λ (a b) #f))

  (max-frame-size #:allocation #:virtual
                  #:accessor max-frame-size
                  #:slot-ref %stream-info-get-max-frame-size
                  #:slot-set! (λ (a b) #f))

  (sample-rate #:allocation #:virtual
               #:accessor sample-rate
               #:slot-ref %stream-info-get-sample-rate
               #:slot-set! (λ (a b) #f))

  (channels #:allocation #:virtual
               #:accessor sample-rate
               #:slot-ref %stream-info-get-sample-rate
               #:slot-set! (λ (a b) #f))

  (bits-per-sample #:allocation #:virtual
               #:accessor bits-per-sample
               #:slot-ref %stream-info-get-bits-per-sample
               #:slot-set! (λ (a b) #f))

  (total-samples #:allocation #:virtual
               #:accessor total-samples
               #:slot-ref %stream-info-get-total-samples
               #:slot-set! (λ (a b) #f))

  (md5sum #:allocation #:virtual
          #:accessor md5sum
          #:slot-ref (λ (stream-info)
                       (string-concatenate
                        (map (λ (index)
                               (format #f "~2,'0x" (bytevector-u8-ref
                                                    (%stream-info-get-md5sum stream-info)
                                                    index)))
                             (iota 16))))
          #:slot-set! (λ (a b) #f))


  #:metaclass <stream-metadata-class>
  #:type 0)

(define-class <vorbis-comment> (<stream-metadata>)
  (vendor-string #:allocation #:virtual
                 #:accessor vendor-string
                 #:slot-ref %vorbis-comment-get-vendor-string
                 #:slot-set! (λ (a b) #f))

  (comments #:allocation #:virtual
            #:accessor comments
            #:slot-ref %vorbis-comment-get-comments
            #:slot-set! (λ (a b) #f))
  #:metaclass <stream-metadata-class>
  #:type 4)
