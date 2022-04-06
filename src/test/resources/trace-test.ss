;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"树形递归"
(define (fib n)
  (cond
    [(< n 1) 0]
    [(= n 1) 1]
    [else (+
            (fib (- n 1))
            (fib (- n 2)))]))

(trace fib)
(println (fib 6))
(untrace fib)
(println (fib 6))
; untrace 之后可以再次 trace
(trace fib)
(println (fib 6))
(untrace fib)
(println (fib 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"线性递归"
(define (fact n)
  (fact-iter 1 1 n))
(define (fact-iter prod cnt max-cnt)
  (if (> cnt max-cnt)
    prod
    (fact-iter
      (* cnt prod)
      (add1 cnt)
      max-cnt)))
(trace fact-iter)
(println (fact 6))