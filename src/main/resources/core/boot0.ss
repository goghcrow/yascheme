; boot0 没有宏展开, 只能用内置 procedure 和 syntax (包括 .)
(define-values (throw) (lambda (t) (. xiao.lang2.Procedures (throw1 t))))
(define-values (raise) (lambda (msg) (throw (new java.lang.RuntimeException msg))))
(define-values (null?) (lambda (x) (. xiao.lang2.Procedures (isNull x))))
(define-values (pair?) (lambda (x) (. xiao.lang2.Procedures (isPair x))))
(define-values (list?) (lambda (x) (. xiao.lang2.Procedures (isList x))))
(define-values (cons) (lambda (x y) (. xiao.lang2.Procedures (cons x y))))
(define-values (car) (lambda (x) (. xiao.lang2.Procedures (car x))))
(define-values (cdr) (lambda (x) (. xiao.lang2.Procedures (cdr x))))
(define-values (cadr) (lambda (x) (car (cdr x))))
(define-values (list) (lambda xs xs))
;; ------------------------------
;; (map proc lst ...+) → list?
;; ------------------------------
(define-values (map)
  (case-lambda
    [(proc lst)
      (if (null? lst)
          '()
        (cons
          (proc (car lst))
          (map proc (cdr lst))))]
    [(proc lst . rest)
      (if (null? lst)
          '()
        (cons
          (apply proc (cons (car lst) (map car rest)))
          (apply map `(,proc ,(cdr lst) ,@(map cdr rest)))))]))

;; ------------------------------
;; 定义 list*, 修正 apply
;; 用 apply1 定义 list*, 再用 list* 定义 apply
;; (apply1 proc lst) → any
;; (list* v ... tail) → any
;; (apply proc v ... lst) → any
;; ------------------------------
(define-values (list*)
  (letrec-values ([(apply1) apply]
                   [(list*)
                     (case-lambda
                       [(v) v]
                       [(a b) (cons a b)]
                       [(a . rest) (cons a (apply1 list* rest))])])
    ;; 只有 boot0 可以改写 apply, 因为 boot0 没有被 expand
    ;; expand 之后 apply 直接引用 procedure 对象, 而不是个 symbol
    ;; set! 第一个参数需要 symbol
    ;; 效果就是 内部 procedure 可以被 redefine, 但是不能被 set!
    (set! apply (lambda (proc . args)
                  (apply1 proc
                    (apply1 list* args))))
    list*))


;; case-lambda
;(define map
;  (case-lambda
;    [(proc lst)
;      (if (null? lst)
;          '()
;        (cons
;          (proc (car lst))
;          (map proc (cdr lst))))]
;    [(proc lst . rest)
;      (if (null? lst)
;          '()
;        (cons
;          (apply proc (car lst) (map car rest))
;          (apply map proc (cdr lst) (map cdr rest))))]))
;; let 版本
;(define map
;  (lambda (proc ls . rest)
;    (if (null? rest)
;      (let map1 ((ls ls))
;        (if (null? ls)
;            '()
;          (cons (proc (car ls))
;            (map1 (cdr ls)))))
;      (let map-rest ((ls ls) (rest rest))
;        (if (null? ls)
;            '()
;          (cons (apply proc (car ls) (map car rest))
;            (map-rest (cdr ls)
;              (map cdr rest))))))))