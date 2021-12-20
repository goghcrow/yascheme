(define <
  (case-lambda
    [(a) (begin a #t)]
    [(a b . rest) (and (xiao.lang2.Generated/lt a b) (apply < b rest))]))

(define >
  (case-lambda
    [(a) (begin a #t)]
    [(a b . rest) (and (xiao.lang2.Generated/gt a b) (apply > b rest))]))

(define <=
  (case-lambda
    [(a) (begin a #t)]
    [(a b . rest)
      (and
        (or
          (xiao.lang2.Generated/lt a b)
          (= a b))
        (apply <= b rest))]))

(define >=
  (case-lambda
    [(a) (begin a #t)]
    [(a b . rest)
      (and
        (or
          (xiao.lang2.Generated/gt a b)
          (= a b))
        (apply >= b rest))]))

(define =
  (case-lambda
    [(a) (xiao.lang2.Generated/eq a a)] ; 类型检查
    [(a b . rest)
      (and
        (xiao.lang2.Generated/eq a b)
        (apply = b rest ))]))

(define +
  (case-lambda
    [() 0]
    [(a) (xiao.lang2.Generated/add 0 a)] ; 类型检查
    [(a b . rest) (apply + (xiao.lang2.Generated/add a b) rest)]))

(define -
  (case-lambda
    [(a) (xiao.lang2.Generated/sub 0 a)]
    [(a b) (xiao.lang2.Generated/sub a b)]
    [(a b . rest) (apply - (xiao.lang2.Generated/sub a b) rest)]))

(define *
  (case-lambda
    [() 1]
    [(a) (xiao.lang2.Generated/mul 1 a)] ; 类型检查
    [(a b . rest) (apply * (xiao.lang2.Generated/mul a b) rest)]))

(define /
  (case-lambda
    [(a) (xiao.lang2.Generated/div 1 a)]
    [(a b) (xiao.lang2.Generated/div a b)]
    [(a b . rest) (apply / (xiao.lang2.Generated/div a b) rest)]))

(define (modulo a b) (xiao.lang2.Generated/modulo a b))
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

;(define-syntax <
;  (syntax-rules ()
;    ((< a) (begin a #t))
;    ((< a b rest ...)
;      (let ([$b b])
;        (and (xiao.lang2.Generated/lt a $b) (< $b rest ...))))))
;
;(define-syntax >
;  (syntax-rules ()
;    ((> a) (begin a #t))
;    ((> a b rest ...)
;      (let ([$b b])
;        (and (xiao.lang2.Generated/gt a $b) (> $b rest ...))))))
;
;(define-syntax <=
;  (syntax-rules ()
;    ((<= a) (begin a #t))
;    ((<= a b rest ...)
;      (let ([$b b])
;        (and
;          (or
;            (xiao.lang2.Generated/lt a $b)
;            (= a $b))
;          (<= $b rest ...))))))
;
;(define-syntax >=
;  (syntax-rules ()
;    ((>= a) (begin a #t))
;    ((>= a b rest ...)
;      (let ([$b b])
;        (and
;          (or
;            (xiao.lang2.Generated/gt a $b)
;            (= a $b))
;          (>= $b rest ...))))))
;
;(define-syntax =
;  (syntax-rules ()
;    ((= a) (xiao.lang2.Generated/eq a a)) ; 类型检查
;    ((= a b rest ...)
;      (let ([$b b])
;        (and (xiao.lang2.Generated/eq a $b) (= $b rest ...))))))
;(define-syntax +
;  (syntax-rules ()
;    ((+) 0)
;    ((+ a) (xiao.lang2.Generated/add 0 a)) ; 类型检查
;    ((+ a b rest ...)
;      (+ (xiao.lang2.Generated/add a b) rest ...))))
;
;(define-syntax -
;  (syntax-rules ()
;    ((- a) (xiao.lang2.Generated/sub 0 a))
;    ((- a b) (xiao.lang2.Generated/sub a b))
;    ((- a b rest ...)
;      (- (xiao.lang2.Generated/sub a b) rest ...))))
;
;(define-syntax *
;  (syntax-rules ()
;    ((*) 1)
;    ((* a) (xiao.lang2.Generated/mul 1 a)) ; 类型检查
;    ((* a b rest ...)
;      (* (xiao.lang2.Generated/mul a b) rest ...))))
;
;(define-syntax /
;  (syntax-rules ()
;    ((/ a) (xiao.lang2.Generated/div 1 a))
;    ((/ a b) (xiao.lang2.Generated/div a b))
;    ((/ a b rest ...)
;      (/ (xiao.lang2.Generated/div a b) rest ...))))