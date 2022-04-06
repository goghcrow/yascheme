; boot.ss 没有宏展开, 只能用内置 procedure 和 syntax (包括 .)

(define-values (void) (lambda () (. xiao.lang.RT Void)))
(define-values (throw) (lambda (t) (. xiao.lang.RT (throw1 t))))
(define-values (raise) (lambda (msg) (throw (new java.lang.RuntimeException msg))))

;; pair constructors and selectors
(define-values (pair?) (lambda (x) (. xiao.lang.RT (isPair x))))
(define-values (null?) (lambda (x) (. xiao.lang.RT (isNull x))))
(define-values (cons) (lambda (x y) (. xiao.lang.RT (cons x y))))
(define-values (car) (lambda (x) (. xiao.lang.RT (car x))))
(define-values (cdr) (lambda (x) (. xiao.lang.RT (cdr x))))

(define-values (caar) (lambda (x) (car (car x))))
(define-values (cadr) (lambda (x) (car (cdr x))))
(define-values (caddr) (lambda (x) (car (cdr (cdr x)))))
(define-values (cadddr) (lambda (x) (car (cdr (cdr (cdr x))))))
(define-values (cddr) (lambda (x) (cdr (cdr x))))
(define-values (cdddr) (lambda (x) (cdr (cdr (cdr x)))))

(define-values (list?) (lambda (x) (. xiao.lang.RT (isList x))))
(define-values (list) (lambda xs xs))
(define-values (append) (lambda xs (. xiao.lang.RT (append (. xs (toArray))))))

;(define-values (equal?) (lambda (a b) (. xiao.lang.RT (equal a b))))

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
;          解释器去掉了 quasiquote 的支持
;          (apply map `(,proc ,(cdr lst) ,@(map cdr rest)))
          (apply map (list* proc (cdr lst) (map cdr rest)))
        ))]))

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
    ;; 只有 boot.ss 可以改写 apply, 因为 boot.ss 没有被 expand
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
