(define (toString a) (java.util.Objects/toString a))
(define (str . args)
  (define s (java.lang.StringBuilder.))
  (for-each (lambda (arg)
              (.append s
                (toString arg))) args)
  (.toString s))
(define string-append str)