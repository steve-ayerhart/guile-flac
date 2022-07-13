(define-module (flac tests utils)
  #:use-module (srfi srfi-64)
  #:export (with-tests))

(define-syntax-rule (with-tests name body ...)
  (begin
    (test-begin name)
    body ...
    (test-end name)))
;    (exit (zero? (test-runner-fail-count (test-end name))))))
