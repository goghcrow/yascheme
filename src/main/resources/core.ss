;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; misc
(define (quit) (java.lang.System/exit 0))
(define exit quit)
(define (NULL? x) (eq? NULL x))
(define NULL (.clear (java.util.ArrayList.))) ;; 随便找了一个反射返回 void 的方法拿到 java null, todo 换成 RT.null

(provide
  exit
  quit
  NULL?
  NULL)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; control construct
(define raise
  (case-lambda
    [() (throw (new java.lang.RuntimeException))]
    [(msg) (throw (new java.lang.RuntimeException msg))]
    [(fmt . args) (throw (new java.lang.RuntimeException
                           (apply format fmt args)))]))
(define error raise)
(define call/cc call-with-current-continuation)
(define (force promise) (promise))
;; The expression (delay-force expression) is conceptually similar to (delay (force expression)),
;; with the difference that forcing the result of delay-force will in effect result
;; in a tail call to (force expression), while forcing the result of (delay (force expression)) might not.
;; Thus iterative lazy algorithms that might result in a long series of chains of delay and force
;; can be rewritten using delay-force to prevent consuming unbounded space during evaluation.
;; Using delay-force or lazy is equivalent. The name delay-force is from R7RS; the name lazy is from the older SRFI-45.
(define (delay-force expr)
  (delay (force expr)))

(provide
  raise error
  call/cc
  force delay-force)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; equality
(define (eq? a b) (xiao.lang.RT/eq a b))
(define (eqv? a b)
  (if (and
        (number? a)
        (number? b)
        (eq? (class a) (class b)))
    (= a b)
    (eq? a b)))
(define (equal? a b) (. xiao.lang.RT (equal a b)))

(provide
  eq?
  eqv?
  equal?)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; number
(define (number? x) (instance? java.lang.Number x))
(define (zero? n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define (even? n) (zero? (modulo n 2)))
(define (odd? n) (not (even? n)))

(provide
  number?
  zero?
  positive?
  negative?
  even?
  odd?)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; boolean
(define true #t)
(define false #f)
(define (not test) (if test #f #t))
(define (boolean? x) (or (.equals x #t) (.equals x #f)))
(define boolean=?
  (case-lambda
    [(a b) (and (boolean? a) (boolean? b) (equal? a b))]
    [(a b . rest)
      (and
        (boolean=? a b)
        (apply boolean=? b rest))]))

(provide
  true
  false
  not
  boolean?
  boolean=?)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; procedure
(define (procedure? x) (instance? xiao.lang.Values$Procedure x))

(provide
  procedure?)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; void
(define (void? x) (.equals (void) x))

(provide
  void?)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; pair
(define cons? pair?)

(provide
  cons?)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; char
(define (char? x) (instance? java.lang.Character x))

(provide
  char?)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; symbol operations
(define gensym
  (case-lambda
    [() (xiao.lang.RT/gensym)]
    [(prefix) (xiao.lang.RT/gensym prefix)]))
(define (symbol? x) (instance? xiao.lang.Values$Symbol x))
(define symbol=?
  (case-lambda
    [(a b)
      (and
        (symbol? a)
        (symbol? b)
        (eq? a b))]
    [(a b . rest)
      (and
        (symbol=? a b)
        (apply symbol=? b rest))]))
(define (string->symbol str)
  (if (string? str)
    (xiao.lang.RT/sym str)
    (raise "contract violation")))
(define (symbol->string sym)
  (if
    ;; (symbol? sym)
    ;; 这里不能用 symbol? 因为 symbol? 依赖 instance?, instance? 宏依赖 symbol->string 死循环
    (eq? (class sym) (java.lang.Class/forName "xiao.lang.Values$Symbol"))
    (.toString sym) ; (.substring (.toString sym) 1)
    (raise "contract violation")))
(provide
  gensym
  symbol?
  symbol=?
  symbol->string
  string->symbol)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; array operations
(define (array? x)
  (if (NULL? x)
    #f
    (.. x (getClass) (isArray))))
(define (new-array class size)
  (java.lang.reflect.Array/newInstance
    (cond
      [(class? class) class]
      [(symbol? class) (symbol->class class)]
      [(string? class) (string->class class)]
      [else (raise "contract violation")])
    size))
(define into-array
  (case-lambda
    [(lst) (.toArray lst)]
    [(class lst)
      (case class
        ; 特殊处理 int long double 其他 primitive 类型没有处理, 会报错
        [(char) (let* ([sz (length lst)]
                        [a (new-array 'char sz)])
                  (do ([i 0 (+ i 1)])
                    ((>= i sz) a)
                    (array-set! a i (.get lst i))))]
        [(int) (.. lst
                 (stream)
                 (mapToInt (xiao.lang.RT/toIntFunction))
                 (toArray))]
        [(long) (.. lst
                  (stream)
                  (mapToLong (xiao.lang.RT/toLongFunction))
                  (toArray))]
        [(double) (.. lst
                    (stream)
                    (mapToDouble (xiao.lang.RT/toDoubleFunction))
                    (toArray))]
        [else (.toArray lst (new-array class (length lst)))])]))
(define (array . args)
  (into-array args))
; class: symbol
(define (typed-array class . args)
  (into-array class args))
(define (array-ref a i)
  (java.lang.reflect.Array/get a i))
(define (array-set! a i v)
  (java.lang.reflect.Array/set a i v) (void))
(define (array-size a)
  (java.lang.reflect.Array/getLength a))
(define list->array
  (case-lambda
    [(lst) (if (list? lst)
             (.toArray lst)
             (throw "contract violation"))]
    [(lst a) (if (and (list? lst) (array? a))
               (.toArray lst a)
               (throw "contract violation"))]))

(provide
  array?
  array
  new-array
  into-array
  typed-array
  array-ref
  array-set!
  array-size
  list->array)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; vector operations
(define vector array)
(define vector? array?)
(define vector-length array-size)
(define vector-ref array-ref)
(define make-vector
  (case-lambda
    [(size) (new-array 'java.lang.Object size)]
    [(size v) (into-array (make-list size v))]))
(define vector-set! array-set!)
(define list->vector
  (case-lambda
    [(lst)
      (unless (list? lst)
        (raise "contract violation"))
      (.toArray lst)]
    [(lst start)
      (list->vector start (length lst))]
    [(lst start end)
      (list->vector (.subList lst start end))]))
(define vector->list
  (case-lambda
    [(vec)
      (xiao.lang.RT/list vec)]
    [(vec start)
      (vector->list vec start (vector-length vec))]
    [(vec start end)
      (.subList (vector->list vec) start end)]))
(define vector-copy
  (case-lambda
    [(vec)
      (vector-copy vec 0)]
    [(vec start)
      (unless (vector? vec)
        (raise "contract violation"))
      (vector-copy vec start (vector-length vec))]
    [(vec start end)
      (unless (vector? vec)
        (raise "contract violation"))
      (java.util.Arrays/copyOfRange vec start end)]))
(define vector-copy!
  (case-lambda
    [(dst dst-start src)
      (vector-copy! dst dst-start src 0)]
    [(dst dst-start src src-start)
      (unless (vector? src)
        (raise "contract violation"))
      (vector-copy! dst dst-start src src-start (vector-length src))]
    [(dst dst-start src src-start src-end)
      (unless (and (vector? src) (vector? dst))
        (raise "contract violation"))
      (java.lang.System/arraycopy
        src
        src-start
        dst
        dst-start
        (- src-end src-start))]))
(define vector-fill!
  (case-lambda
    [(vec v)
      (vector-fill! vec v 0)]
    [(vec v start)
      (unless (vector? vec)
        (raise "contract violation"))
      (vector-fill! vec v start (vector-length vec))]
    [(vec v start end)
      (unless (and (vector? vec) (number? start) (number? end))
        (raise "contract violation"))
      (java.util.Arrays/fill vec start end v)]))
(define (vector-append . vecs)
  (for-each (lambda (it)
              (unless (vector? it)
                (raise "contract violation")))
    vecs)
  (list->vector
    (apply append
      (map vector->list vecs))))

(provide
  vector
  vector?
  vector-length
  vector-ref
  make-vector
  vector-set!
  vector->list
  list->vector
  vector-copy
  vector-copy!
  vector-fill!
  vector-append)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; pair constructors and selectors
(define null '())
(define empty null)
(define (first l)
  (unless (list? l)
    (raise "contract violation"))
  (car l))
(define (rest  l)
  (unless (list? l)
    (raise "contract violation"))
  (cdr l))
; (build-list n proc) → list?
(define (build-list n proc)
  (let loop ([i 0] [lst '()])
    (if (< i n)
      (loop (add1 i) (cons (proc i) lst))
      (reverse lst))))
; (make-list k v) → list?
(define (make-list k v)
  (build-list k (lambda (a) v)))

(provide
  ; pair? null? cons car cdr
  ; list? list list*
  null
  empty
  first
  rest
  build-list
  make-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list operations
(define (length lst)
  (unless (list? lst)
    (raise "contract violation"))
  (.size lst))
(define (reverse lst)
  (unless (list? lst)
    (raise "contract violation"))
;  (let ([lst (list-copy lst)])
;    (java.util.Collections/reverse lst)
;    lst)
  (if (null? lst)
    null
    (append (reverse (rest lst)) (cons (first lst) null))))
(define (list-ref lst pos) (.get lst pos))
(define (list-tail lst k)
  (.subList lst k (length lst)))
(define (list-set! lst pos value)
  (.set_ lst pos value))
(define (list-copy lst)
  (if (list? lst)
    (xiao.lang.RT/listColl lst)
    lst))

(provide
  length
  reverse
  ; append ; boot.ss @ RT.append
  list-ref
  list-tail
  list-set!
  list-copy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list iteration
(define (for-each proc lst)
  (cond
    [(list? lst)
      (if (null? lst)
        (void)
        (begin
          (proc (car lst))
          (for-each proc (cdr lst))))]
    [(array? lst)
      (let ([sz (array-size lst)])
        (do ([i 0 (+ i 1)])
          ((>= i sz) (void))
          (proc (array-ref lst i))))]))
;; todo andmap ormap foldl foldr

(provide
  for-each)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list filtering
;; todo filter remove remq remv remove* remq* remv* sort

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list searching
(define memv (list-search eqv?))
(define memq (list-search eq?))
;(assoc v lst [is-equal?])
(define assoc
  (case-lambda
    [(v lst) (assoc v lst equal?)]
    [(v lst is-equal?)
      (cond
        [(null? lst) #f]
        [(is-equal? (caar lst) v) (car lst)]
        [else (assoc v (cdr lst) is-equal?)])]))
(define (assq v lst) (assoc v lst eq?))
(define (assv v lst) (assoc v lst eqv?))

(provide
  memq
  memv
  assoc
  assq
  assv)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; number operations

; java.lang.Math 一堆方法都返回 double
(define (-fix-number n cls)
  ; double -> int
  (if (and
        (= 0 (remainder n 1))
        (eq? (class n) cls))
    (cond
      [(> n java.lang.Long/MAX_VALUE) (bigint n)]
      [(> n java.lang.Integer/MAX_VALUE) (long n)]
      [else (int n)])
    n))
(define (-fix-number-double n)
  (-fix-number n (class 1d)))
(define (-fix-number-long n)
  (-fix-number n (class 1l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arithmetic
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
(define (square x) (* x x))

;mod 和 rem 是取余函数，div 和 quot 是求商函数，但是在计算负数的时候，运算结果
;会有些不同。这里简单说明一下不同点。我们在 GHCi 里运行一下:
;1 2 3 4 5 6 7 8
;可以看出，div 与 mod 是一组，因为商-3 乘以 5 再加上-3 正好是 12。
; 同理，quot 与 rem 是 一组。在计算的过程中，在保证余数 r 的绝对值小于除数的情况下，div 总是要结果逼近于 负无穷，而 quot 总是需要将结果逼近 0
;; %
(define (remainder a b) (xiao.lang.Generated/remainder a b))
;; https://newbedev.com/mod-in-java-produces-negative-numbers
;; https://stackoverflow.com/questions/4412179/best-way-to-make-javas-modulus-behave-like-it-should-with-negative-numbers/25830153#25830153
;; (a % b + b) % b
(define (modulo a b)
  (remainder
    (+
      (remainder a b)
      b)
    b))
(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))
(define inc add1)
(define dec sub1)
(define (abs n)
  (java.lang.Math/abs n))
; (max x ...+) → real?
(define max
  (case-lambda
    [(x) x]
    [(x y) (if (>= x y) x y)]
    [(x y . rest) (apply max (max x y) rest)]))
; (min x ...+) → real?
(define min
  (case-lambda
    [(x) x]
    [(x y) (if (<= x y) x y)]
    [(x y . rest) (apply min (min x y) rest)]))
(define gcd
  (case-lambda
    [(a b) (abs (cond
                  [(= b 0) a]
                  [else (gcd b (remainder a b))]))]
    [() 0]))
(define lcm
  (case-lambda
    [(a b) (cond
             [(or (zero? a) (zero? b)) 0]
             [else (abs (* b (-fix-number-double (floor (/ a (gcd a b))))))])]
    [() 1]))
(define (round a)
  (-fix-number-long (java.lang.Math/round (double a))))
(define (floor a)
  (java.lang.Math/floor (double a)))
(define (ceiling a)
  (java.lang.Math/ceil (double a)))

(provide
  + - * /
  square
  remainder
  modulo
  add1 inc
  sub1 dec
  abs
  max min
  gcd lcm
  round
  ceiling floor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; number comparison
(define =
  (case-lambda
    [(a) (xiao.lang.Generated/eq a a)] ; 类型检查
    [(a b . rest)
      (and
        (xiao.lang.Generated/eq a b)
        (apply = b rest ))]))
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

(provide
  = < > <= >=)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powers and roots
(define (sqrt z)
  (-fix-number-double (java.lang.Math/sqrt (double z))))
(define (integer-sqrt n)
  (cond
    [(= n 0) 0]
    [(> n 0) (-fix-number-double (floor (sqrt n)))]
    [(< n 0)
      (raise "todo 不支持虚数")
      ;(* (integer-sqrt (- n)) 0+1i)
    ]))
(define (exact-integer-sqrt n)
  (letrec ([root (integer-sqrt n)]
            [rem (- n (* root root))])
    (values root (-fix-number-double rem))))


(define (expt a b)
  (let ([d (java.lang.Math/pow (double a) (double b))])
    (-fix-number-double d)))
(define (exp z)
  (-fix-number-double (java.lang.Math/exp (double z))))
(define log
  (case-lambda
  [(z) (java.lang.Math/log (double z))]
  [(z b) (/ (log z) (log b))]))

(provide
  sqrt integer-sqrt exact-integer-sqrt
  expt exp
  log)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  num str conv operations
(define string->number
  (let ([double?
          (lambda (s)
            (or
              (.contains s ".")
              (.contains s "e")
              (.contains s "E")))])
    (case-lambda
      [(s)
        (if (double? s)
          (java.lang.Double/parseDouble s)
          (java.lang.Integer/parseInt s))]
      [(s radix)
        (if (double? s)
          (raise "double 不支持 radix")
          (java.lang.Integer/parseInt s radix))])))
;; (number->string z [radix]) → string?
(define number->string
  (case-lambda
    [(n) (toString n)]
    [(n radix)
      (cond
        [(member (class n) (list (class 1b) (class 1s) (class 1) (class 1l)))
          (java.lang.Long/toString (long n) radix)]
        [(eq? (class n) (class 1n))
          (.toString n radix)]
        [else
          (if (= radix 10)
            (toString n)
            (raise "radix 只支持 10"))])]))

(provide
  number->string
  string->number)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; coerce operations
(define (byte a) (xiao.lang.Generated/castToByte a))
(define (short a) (xiao.lang.Generated/castToShort a))
(define (int a) (xiao.lang.Generated/castToInt a))
(define (long a) (xiao.lang.Generated/castToLong a))
(define (float a) (xiao.lang.Generated/castToFloat a))
(define (double a) (xiao.lang.Generated/castToDouble a))
(define (char a) (xiao.lang.Generated/castToChar a))
(define (bigint a) (xiao.lang.Generated/castToBigInteger a))
(define (bigdec a) (xiao.lang.Generated/castToBigDecimal a))
; clojure str
(define (str . args)
  (define s (java.lang.StringBuilder.))
  (let loop ([args args])
    (if (null? args)
      (toString s)
      (let ()
        (.append s (toString (car args)))
        (loop (cdr args))))))

(provide
  byte
  short
  int
  long
  float
  double
  char
  bigint
  bigdec
  str)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; string operations
;; string Constructors, selectors
(define (string? x) (instance? java.lang.String x))
;; (make-string k [char]) → string?
;;    char : char? = #\nul
(define make-string
  (case-lambda
    [(k) (make-string k #\nul)]
    [(k ch) (string-repeat (string ch) k)]))
;; (string char ...) → string?
;;    char : char?
(define (string . chars)
  (for-each (lambda (it)
              (unless (char? it)
                (raise "contract violation"))) chars)
  (java.lang.String. (apply typed-array 'char chars)))
(define (string-length str) (.length str))
;; (string-ref str k) → char?
(define (string-ref str k) (.charAt str k))
(define (string-set! str k char)
  (array-set! (.value str) k char)
  (void))
; 这里没区分 mutable 和 immutable string
; 不能用 substring 实现 string-copy
; 因为 java 语义 string 是不可变对象, 所以 (substring str 0) 返回 this
; 但是 string-set! 把 java string 语义改了
(define (-copy-string s)
  (java.lang.String. s))
(define -string-copy
  (case-lambda
    [(str) (.substring str 0)]
    [(str start) (.substring str start)]
    [(str start end) (.substring str start end)]))
(define (string-copy . args)
  (-copy-string (apply -string-copy args)))
(define substring string-copy)
(define (string-copy! . args)
  (unless (and (>= (length args) 3) (string? (car args)))
    (raise "contract violation"))
  (apply vector-copy! `(,(.value (car args))
                         ,(cadr args)
                         ,(.value (caddr args))
                         ,@(cdddr args))))
(define (string-fill! . args)
  (unless (and (>= (length args) 1) (string? (car args)))
    (raise "contract violation"))
  (apply vector-fill! (cons (.value (car args)) (cdr args))))
(define (string-append . strs)
  (for-each (lambda (it)
              (unless (string? it)
                (raise "contract violation")))
    strs)
  (apply str strs))
;(define (string->list . args)
;  (unless (and (>= (length args) 1) (string? (car args)))
;    (raise "contract violation"))
;  (apply vector->list
;    (cons (.value (car args)) (cdr args))))
(define string->list
  (case-lambda
    [(str)
      (string->list str 0)]
    [(str start)
      (string->list str start (string-length str))]
    [(str start end)
      (unless (and (string? str) (number? start) (number? end))
        (raise "contract violation"))
      (let loop ([start start]
                  [end end])
        (if (< start end)
          (cons
            (string-ref str start)
            (loop (+ start 1) end))
            '()))]))
(define (string->vector . args)
  (list->vector (apply string->list args)))
(define list->string
  (case-lambda
    [(lst)
      (unless (list? lst)
        (raise "contract violation"))
      (apply string lst)]
    [(lst start)
      (list->string lst start (length lst))]
    [(lst start end)
      (list->string (.subList lst start end))]))
(define (vector->string . args)
  (unless (and (>= (length args) 1) (vector? (car args)))
    (raise "contract violation"))
  (apply list->string
    (cons
      (vector->list (car args))
      (cdr args))))

(provide
  string?
  make-string
  string
  string-length
  string-ref
  string-set!
  substring
  string-copy
  string-copy!
  string-fill!
  string-append
  string->list
  list->string
  string->vector
  vector->string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string comparisons
(define (-string-cmp cmp sensitive)
  (define f (case-lambda
              [(a b) (and
                       (string? a)
                       (string? b)
                       (if sensitive
                         (cmp a b)
                         (cmp (.toLowerCase a) (.toLowerCase b))))]
              [(a b . rest)
                (and
                  (f a b)
                  (apply f b rest))]))
  f)
(define ((-cmp-zero cmp) a b)
  (cmp (.compareTo a b) 0))
(define string=? (-string-cmp equal? #t))
(define string<? (-string-cmp (-cmp-zero <) #t))
(define string<=? (-string-cmp (-cmp-zero <=) #t))
(define string>? (-string-cmp (-cmp-zero >) #t))
(define string>=? (-string-cmp (-cmp-zero >=) #t))
(define string-ci=? (-string-cmp equal? #f))
(define string-ci<? (-string-cmp (-cmp-zero <) #f))
(define string-ci<=? (-string-cmp (-cmp-zero <=) #f))
(define string-ci>? (-string-cmp (-cmp-zero >) #f))
(define string-ci>=? (-string-cmp (-cmp-zero >=) #f))

(provide
  string=?
  string<?
  string<=?
  string>?
  string>=?
  string-ci=?
  string-ci<?
  string-ci<=?
  string-ci>?
  string-ci>=?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string conversions
(define (string-upcase str)
  (unless (string? str)
    (raise "contract violation"))
  (.toUpperCase str))
(define (string-downcase str)
  (unless (string? str)
    (raise "contract violation"))
  (.toLowerCase str))
(define (string-foldcase str)
  (unless (string? str)
    (raise "contract violation"))
  ;; https://docs.atlassian.com/software/jira/docs/api/7.1.6/com/atlassian/jira/util/CaseFolding.html
  ;; Basically the value is case folded by calling String.toUpperCase(locale).toLowerCase(locale).
  ;; This is a little better than calling only String.toLowerCase(locale).
  ;; For example, in German the string "ß" upercase equilavent is "SS".
  ;; If we only called only String.toLowerCase(java.util.Locale) we would store "ß"in the index
  ;; which would make matching "SS" impossible. This does not always case fold correctly,
  ;; for example the Turkish 'I', but it is a good compromise.
  (.toLowerCase (.toUpperCase str)))

(provide
  string-upcase
  string-downcase
  string-foldcase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional string functions
(define string-repeat
  (case-lambda
    [(s n) (string-repeat s n "")]
    [(s n sep) (java.lang.String/join sep
                 (java.util.Collections/nCopies n s))]))
(define (toString a)
  (if (array? a)
    (java.util.Arrays/toString a)
    (java.util.Objects/toString a)))

(provide
  string-repeat
  toString)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; IO
(define (print x)
  (.print java.lang.System/out
    (if (array? x)
      (java.util.Arrays/toString x)
      x))
  (void))
(define (newline) (print "\n"))
(define (println x) (print x) (newline))
(define display print)
(define displayln println)
(define (printf fmt . args)
  (.printf java.lang.System/out fmt (.toArray args)))
(define (call-with-output-string proc)
  (let* ([std-out (. java.lang.System out)] ;; todo java.lang.System/out
          [baos (java.io.ByteArrayOutputStream.)]
          [utf8 (.name java.nio.charset.StandardCharsets/UTF_8)]
          [ps (java.io.PrintStream. baos #t utf8)])
    (.setOut java.lang.System ps)
    (proc)
    (.setOut java.lang.System std-out)
    (.close ps)
    (.toString baos utf8)))
(define (format fmt . args)
  (java.lang.String/format fmt (list->array args)))

(provide
  print display
  println displayln
  printf
  newline
  format
  call-with-output-string)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; class operations
(define (class? x)
  (instance? java.lang.Class x))
(define (class x)
  (if (eq? x NULL)
    NULL
    (.getClass x)))
(define (string->class str)
  (if (string? str)
    (case str
      [("boolean") (. java.lang.Boolean TYPE)]
      [("byte") (. java.lang.Byte TYPE)]
      [("short") (. java.lang.Short TYPE)]
      [("int") (. java.lang.Integer TYPE)]
      [("long") (. java.lang.Long TYPE)]
      [("float") (. java.lang.Float TYPE)]
      [("double") (. java.lang.Double TYPE)]
      [("char") (. java.lang.Character TYPE)]
      [("void") (. java.lang.Void TYPE)] ;; 歧义
      [else (java.lang.Class/forName str)])
    (raise "contract violation")))
(define (symbol->class sym)
  (string->class (symbol->string sym)))

(provide
  class?
  class
  string->class
  symbol->class)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; interop operations
(define (-native->procedure ctor? class method-name param-types)
  (let* ([->string (lambda (it)
                     (when (symbol? it)
                       (set! it (symbol->string it)))
                     (if (string? it) it (raise "contract violation")))]
          [->class (lambda (it)
                     (when (symbol? it)
                       (set! it (symbol->string it)))
                     (when (string? it)
                       (set! it (string->class it)))
                     (if (class? it) it (raise "contract violation")))]
          [param-types (apply
                         typed-array
                           'java.lang.Class
                         (map ->class param-types))])
    (if ctor?
      (let ([ctor (.constructor
                    (xiao.lang.Reflect/klass (->class class))
                    param-types)])
        (lambda args
          (.invoke ctor (list->array args))))
      (let ([method (.method
                      (xiao.lang.Reflect/klass (->class class))
                      (->string method-name)
                      param-types)])
        (lambda (ins . args)
          (.. method
            (instance (if (null? ins) NULL ins))
            (invoke (list->array args))))))))
;;; ------------------------------------------------
;;; (constructor->procedure class . param-types)
;;; ------------------------------------------------
(define (constructor->procedure class . param-types)
  (-native->procedure #t class #f param-types))
;;; ------------------------------------------------
;;; (method->procedure class method-name . param-types)
;;; ------------------------------------------------
(define (method->procedure class method-name . param-types)
  (-native->procedure #f class method-name param-types))
;; ------------------------------------------------
;; (arraylist a ...)
;; ------------------------------------------------
;(define-syntax-rule (arraylist el ...)
;  (doto
;    (java.util.ArrayList.)
;    (.add el) ...))
(define (arraylist . els)
  (java.util.ArrayList. els))

(provide
  constructor->procedure
  method->procedure
  arraylist)