(define NULL (.clear (java.util.ArrayList.))) ; 随便找了一个反射返回 void 的方法拿到 java null
(define null '())
(define empty '())
(define void (lambda () (xiao.lang2.Procedures/Void)))

;健康宏展开 #t\#f 不是 symbol 所以这里不这么处理
;(define #t java.lang.Boolean/TRUE)
;(define #f java.lang.Boolean/FALSE)
(define true #t)
(define false #f)

(define (null? x) (xiao.lang2.Procedures/isNull x))
(define (void? x) (.equals (void) x))
(define (pair? x) (xiao.lang2.Procedures/isPair x))
(define cons? pair?)
(define (list? x) (instance? xiao.lang2.Values$PList x))
(define (boolean? x) (or (.equals x #t) (.equals x #f)))
(define (string? x) (instance? java.lang.String x))
(define (number? x) (instance? java.lang.Number x))
(define (symbol? x) (instance? xiao.lang2.Values$Symbol x))
(define (procedure? x) (instance? xiao.lang2.Values$Procedure x))

(define (equals? a b) (java.util.Objects/equals a b))
(define equal? equals?)
(define (eq? v1 v2) (xiao.lang2.Procedures/eq v1 v2))

;; not and or cond

(define (not test) (if test #f #t))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test rest ...)
      (let ([$test test])
        (if $test
          (and rest ...)
          $test)))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test rest ...)
      (let ([$test test])
        (if $test
          $test
          (or rest ...))))))