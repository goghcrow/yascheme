(define (for-each proc lst)
  (if (null? lst)
    (void)
    (begin
      (proc (car lst))
      (for-each proc (cdr lst)))))

(define (list-transform-search transform)
  (lambda (predicate)
    (lambda (object list)
      (do ([pair list (cdr pair)])
        ((or
           (null? pair)
           (predicate (car (transform pair)) object))
          (if (null? pair)
            #f
            (transform pair)))))))
(define list-search (list-transform-search (lambda (x) x)))
(define member (list-search equals?))