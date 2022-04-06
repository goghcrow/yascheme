;; internal defination

;; https://docs.racket-lang.org/guide/define.html
;; Expressions and internal definitions in a body sequence can be mixed,
;; as long as the last body is an expression. 🎈
;; Internal definitions in a particular body sequence are mutually recursive;
;; that is, any definition can refer to any other definition—as long as
;; the reference isn’t actually evaluated before its definition takes place.
;; If a definition is referenced too early, an error occurs.

; lambda
(lambda ()
  ; 内部定义语义 letrec
  (define (is-even? n)
    (or (zero? n)
      (is-odd? (sub1 n))))
  (define (is-odd? n)
    (and (not (zero? n))
      (is-even? (sub1 n))))

  (assert-equals true (is-odd? 11))
  (assert-equals true (is-even? 12)))

; let[rec]-[syntaxes+]values
(let-values ()
  (define (is-even? n)
    (or (zero? n)
      (is-odd? (sub1 n))))
  (define (is-odd? n)
    (and (not (zero? n))
      (is-even? (sub1 n))))

  (assert-equals true (is-odd? 11))
  (assert-equals true (is-even? 12)))

(letrec-values ()
  (define (is-even? n)
    (or (zero? n)
      (is-odd? (sub1 n))))
  (define (is-odd? n)
    (and (not (zero? n))
      (is-even? (sub1 n))))

  (assert-equals true (is-odd? 11))
  (assert-equals true (is-even? 12)))

(letrec-syntaxes+values () ()
  (define (is-even? n)
    (or (zero? n)
      (is-odd? (sub1 n))))
  (define (is-odd? n)
    (and (not (zero? n))
      (is-even? (sub1 n))))

  (assert-equals true (is-odd? 11))
  (assert-equals true (is-even? 12)))

;; Expressions and internal definitions in a body sequence can be mixed,
;; as long as the last body is an expression. 🎈
(lambda () (void))
;; ❌ 最后一个必须是表达式, 不能是 definition
;; (lambda () (define a 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;