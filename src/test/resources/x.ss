; lambda case-lambda
; if
; begin begin0
; letrec-syntaxes+values let-values letrec-values set!
; quote quote-syntax
; new .
; body(let\lambda): begin\define-values\define-syntaxes


; todo define, let, letrec, let*, let*-values, letrec-syntax





;letrec-syntaxes+values



;;;; todotodotodotodotodotodotodotodotodo
;;;; let let* letrec


(let-values ()



  (define-syntaxes (letrec)
    (lambda (stx)
      (let-values ([(pairs) (syntax-e (car (cdr (syntax-e stx))))]
                    [(bodys) (cdr (cdr (syntax-e stx)))])
        (datum->syntax
          (quote-syntax here)
          (cons 'letrec-values
            (cons
              (map (lambda (pair)
                     (let-values ([(id) (car (syntax-e pair))]
                                   [(val-expr) (car (cdr (syntax-e pair)))])
                       (list (list id) val-expr))) pairs)
              bodys))))))



  (define a 1)

  (let ([a 1] [b 2]) (+ a b))

  (letrec ([is-even? (lambda (n)
                       (println "is-even")
                       (println n)
                       (or (zero? n)
                       (is-odd? (sub1 n))))]
          [is-odd? (lambda (n)
                     (println "is-odd")
                     (println n)
                     (and (not (zero? n))
                       (is-even? (sub1 n))))])
    (is-odd? 11))
)