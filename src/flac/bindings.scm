(define-module (flac bindings)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)

  #:export (%metadata_object_new
            %metadata_get_streaminfo
            %metadata_get_tags
            %metadata_object_application_set_data))

(define libflac (dynamic-link "libFLAC"))

(define-syntax-rule (define-libflac name return c_name args)
  (define name
    (pointer->procedure return (dynamic-func c_name libflac) args)))

(define-libflac %metadata_object_new
  '* "FLAC__metadata_object_new" (list int))

(define-libflac %metadata_get_streaminfo
  int "FLAC__metadata_get_streaminfo" (list '* '*))

(define-libflac %metadata_get_tags
  int "FLAC__metadata_get_tags" (list '* '*))

(define-libflac %metadata_object_application_set_data
  int "FLAC__metadata_object_application_set_data" (list '* '* unsigned-int int))
