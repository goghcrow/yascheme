(define true #t)
(define false #f)
(define null '())
(define NULL (.clear (java.util.ArrayList.))) ;; 随便找了一个反射返回 void 的方法拿到 java null
(define empty null)
(define (void) (xiao.lang.Procedures/Void))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

(define (quit) (java.lang.System/exit 0))
(define exit quit)
(define call/cc call-with-current-continuation)
(define error raise)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; print
(define (print a) (.print java.lang.System/out a) (void))
(define (println a) (.println java.lang.System/out a) (void))
(define (newline) (print "\n"))
(define display print)
(define displayln println)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; symbol
(define gensym
  (case-lambda
    [() (xiao.lang.Procedures/gensym)]
    [(prefix) (xiao.lang.Procedures/gensym prefix)]))

(define (string->symbol str)
  (if (string? str)
    (xiao.lang.Procedures/sym str)
    (raise "contract violation")))
(define (symbol->string sym)
  (if
    ;; (symbol? sym)
    ;; 这里不能用 symbol? 因为 symbol? 依赖 instance?, instance? 宏依赖 symbol->string 死循环
    (eq? (class sym) (java.lang.Class/forName "xiao.lang.Values$Symbol"))
    (.toString sym) ; (.substring (.toString sym) 1)
    (raise "contract violation")))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; string
(define (toString a) (java.util.Objects/toString a))
(define (str . args)
  (define s (java.lang.StringBuilder.))
  (let loop ([args args])
    (if (null? args)
      (.toString s)
      (let ()
        (.append s (toString (car args)))
        (loop (cdr args))))))
(define string-append str)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; conditions
(define (not test) (if test #f #t))

(define (equals? a b) (java.util.Objects/equals a b))
(define equal? equals?)
(define (eq? v1 v2) (xiao.lang.Procedures/eq v1 v2))

(define cons? pair?)
(define (void? x) (.equals (void) x))
(define (boolean? x) (or (.equals x #t) (.equals x #f)))
(define (string? x) (instance? java.lang.String x))
(define (number? x) (instance? java.lang.Number x))
(define (symbol? x) (instance? xiao.lang.Values$Symbol x))
(define (procedure? x) (instance? xiao.lang.Values$Procedure x))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; coerce
(define (byte a) (xiao.lang.Generated/castToByte a))
(define (short a) (xiao.lang.Generated/castToShort a))
(define (int a) (xiao.lang.Generated/castToInt a))
(define (long a) (xiao.lang.Generated/castToLong a))
(define (float a) (xiao.lang.Generated/castToFloat a))
(define (double a) (xiao.lang.Generated/castToDouble a))
(define (char a) (xiao.lang.Generated/castToChar a))
(define (bigint a) (xiao.lang.Generated/castToBigInteger a))
(define (bigdec a) (xiao.lang.Generated/castToBigDecimal a))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; list
(define (first l) (if (list? l) (car l) (raise "contract violation")))
(define (rest  l) (if (list? l) (cdr l) (raise "contract violation")))

(define (for-each proc lst)
  (if (null? lst)
    (void)
    (begin
      (proc (car lst))
      (for-each proc (cdr lst)))))

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
(define member (list-search equals?))


;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; math
(define <
  (case-lambda
    [(a) (begin a #t)]
    [(a b . rest) (and (xiao.lang.Generated/lt a b) (apply < b rest))]))

(define >
  (case-lambda
    [(a) (begin a #t)]
    [(a b . rest) (and (xiao.lang.Generated/gt a b) (apply > b rest))]))

(define <=
  (case-lambda
    [(a) (begin a #t)]
    [(a b . rest)
      (and
        (or
          (xiao.lang.Generated/lt a b)
          (= a b))
        (apply <= b rest))]))

(define >=
  (case-lambda
    [(a) (begin a #t)]
    [(a b . rest)
      (and
        (or
          (xiao.lang.Generated/gt a b)
          (= a b))
        (apply >= b rest))]))

(define =
  (case-lambda
    [(a) (xiao.lang.Generated/eq a a)] ; 类型检查
    [(a b . rest)
      (and
        (xiao.lang.Generated/eq a b)
        (apply = b rest ))]))

(define +
  (case-lambda
    [() 0]
    [(a) (xiao.lang.Generated/add 0 a)] ; 类型检查
    [(a b . rest) (apply + (xiao.lang.Generated/add a b) rest)]))

(define -
  (case-lambda
    [(a) (xiao.lang.Generated/sub 0 a)]
    [(a b) (xiao.lang.Generated/sub a b)]
    [(a b . rest) (apply - (xiao.lang.Generated/sub a b) rest)]))

(define *
  (case-lambda
    [() 1]
    [(a) (xiao.lang.Generated/mul 1 a)] ; 类型检查
    [(a b . rest) (apply * (xiao.lang.Generated/mul a b) rest)]))

(define /
  (case-lambda
    [(a) (xiao.lang.Generated/div 1 a)]
    [(a b) (xiao.lang.Generated/div a b)]
    [(a b . rest) (apply / (xiao.lang.Generated/div a b) rest)]))

(define (modulo a b) (xiao.lang.Generated/modulo a b))
(define rem modulo)
(define reminder rem)

(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))
(define inc add1)
(define dec sub1)
(define (zero? n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define (even? n) (zero? (modulo n 2)))
(define (odd? n) (not (even? n)))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; interop

;;; ------------------------------------------------
;;; (class obj)
;;; ------------------------------------------------
(define (class x) (.getClass x))

;; ------------------------------------------------
;; (arraylist a ...)
;; ------------------------------------------------
;(define-syntax-rule (arraylist el ...)
;  (doto
;    (java.util.ArrayList.)
;    (.add el) ...))
(define (arraylist . els)
  (java.util.ArrayList. els))


;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

; export
(provide
  error
  exit quit
  call/cc

  NULL
  null empty
  void

  true
  false

  print display
  println displayln
  newline

  first
  rest

  gensym
  symbol->string
  string->symbol

  toString
  str string-append

  not
  eq?
  equal? equals?
  cons?
  void?
  boolean?
  string?
  number?
  symbol?
  procedure?

  byte
  short
  int
  long
  float
  double
  char
  bigint
  bigdec
  first
  rest

  for-each
  member

  < > <= >= = + - * /
  modulo rem reminder

  add1 inc
  sub1 dec
  zero?
  positive?
  negative?
  even?
  odd?

  class
  arraylist)