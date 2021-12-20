;(let ((a 1))
;  (define a 2)
;  ; (println a)
;)

(println
  (letrec ((x (call/cc list)))
    (if (pair? x)
      ((car x) (lambda () x))
      (pair? (x)))
  ))

(println
  (let ((x (call/cc list)))
    (if (pair? x)
      ((car x) (lambda () x))
      (pair? (x)))))