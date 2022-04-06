;; ------------------------------
;; string->class 中的 case 宏需要提前用到 member, 所以 member 加入 core0.ss
;; (member v lst [is-equal?]) → (or/c #f list? any/c)
;; ------------------------------
(define (list-transform-search transform)
  (lambda (predicate)
    (lambda (object lst)
      (let loop ([lst lst])
        (if (null? lst)
          #f
          (if (predicate (car (transform lst)) object)
            lst
            (loop (cdr lst))))))))
(define list-search (list-transform-search (lambda (x) x)))
(define member
  (case-lambda
    [(v lst)
      ((list-search
         (lambda (a b)
           (xiao.lang.RT/equal a b))) v lst)]
    [(v lst is-equal?)
      ((list-search is-equal?) v lst)]))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

; export
(provide
  list-transform-search
  list-search
  member)