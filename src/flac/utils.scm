(define-module (flac utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-60)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 format)
  #:export (bytevector->base16-string
            base16-string->bytevector))

;;;
;;; Base 16.
;;;

(define (bytevector->base16-string bv)
  "Return the hexadecimal representation of BV's contents."
  (define len
    (bytevector-length bv))

  (let-syntax ((base16-chars (lambda (s)
                               (syntax-case s ()
                                 (_
                                  (let ((v (list->vector
                                            (unfold (cut > <> 255)
                                                    (lambda (n)
                                                      (format #f "~2,'0x" n))
                                                    1+
                                                    0))))
                                    v))))))
    (define chars base16-chars)
    (let loop ((i len)
               (r '()))
      (if (zero? i)
          (string-concatenate r)
          (let ((i (- i 1)))
            (loop i
                  (cons (vector-ref chars (bytevector-u8-ref bv i)) r)))))))

(define base16-string->bytevector
  (let ((chars->value (fold (lambda (i r)
                              (vhash-consv (string-ref (number->string i 16)
                                                       0)
                                           i r))
                            vlist-null
                            (iota 16))))
    (lambda (s)
      "Return the bytevector whose hexadecimal representation is string S."
      (define bv
        (make-bytevector (quotient (string-length s) 2) 0))

      (string-fold (lambda (chr i)
                     (let ((j (quotient i 2))
                           (v (and=> (vhash-assv chr chars->value) cdr)))
                       (if v
                           (if (zero? (logand i 1))
                               (bytevector-u8-set! bv j
                                                   (arithmetic-shift v 4))
                               (let ((w (bytevector-u8-ref bv j)))
                                 (bytevector-u8-set! bv j (logior v w))))
                           (error "invalid hexadecimal character" chr)))
                     (+ i 1))
                   0
                   s)
      bv)))

