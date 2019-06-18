(define-module (flac config)
  #:export (extensions-library))

(define *flaclibdir*
  (cond ((getenv "GUILE_FLAC_BUILD_DIR")
         => (λ (builddir) (in-vicinity builddir ".libs")))
        (else "/usr/lib/guile/2.2/extensions")))

(define (extensions-library lib)
  (in-vicinity *flaclibdir* lib))
