; begin 应该实现成 syntax 的...
;(define (begin . xs)
;  (if (.isEmpty xs)
;    (void)
;    (.get xs (xiao.lang2.Generated/sub (.size xs) 1))))
(define seq begin)

(define (begin0 . xs)
  (if (.isEmpty xs)
    (void)
    (.get xs 0)))


(define λ lambda)
(define call/cc call-with-current-continuation)
(define define-syntax define)

(define (make-environment) (xiao.lang.Env.))

(define (quit) (java.lang.System/exit 0))
(define exit quit)
;(define (load path)
;  (eval (xiao.lang2.Reader/read
;          (xiao.lang2.Misc/read path))))

; todo check (string?)
(define (string->symbol str) (xiao.lang2.Procedures/sym str))
; todo check (symbol?)
;(define (symbol->string sym) (.substring (.toString sym) 1))  ; 移除'
(define (symbol->string sym) (.toString sym))  ; 移除'

(define (cons x y) (xiao.lang2.Procedures/cons x y))
(define (car x) (xiao.lang2.Procedures/car x))
(define (cdr x) (xiao.lang2.Procedures/cdr x))
(define (list . args) args)
(define (first l)
  (if (list? l)
    (car l)
    (raise "contract violation")))
(define (rest l)
  (if (list? l)
    (cdr l)
    (raise "contract violation")))

; 只支持单个 list 参数的 apply
; (apply proc lst) → any
(define apply-list apply)
; 定义 list*
; (list* v ... tail) → any/c
(define list*
  (case-lambda
    [() (raise "arity mismatch")]
    [(v) v]
    [(a b) (cons a b)]
    [(a . rest) (cons a (apply-list list* rest))]))
; 再用 list* 重新定义 apply
; (apply proc v ... lst) → any
(define (apply proc . args)
  (apply-list proc
    (apply-list list* args)))
; 再用 apply 定义 map
; (map proc lst ...+) → list?
; case-lambda 版本
(define map
  (case-lambda
    [(proc lst)
      (if (null? lst)
        null
        (cons
          (proc (car lst))
          (map proc (cdr lst))))]
    [(proc lst . rest)
      (if (null? lst)
        null
        (cons
          (apply proc (car lst) (map car rest))
          (apply map proc (cdr lst) (map cdr rest))))]))
; let 版本
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------------------------------
;; (define-syntax-rule pattern template)
;; ------------------------------------------------
(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (name params ...) template)
      (define-syntax name
        (syntax-rules ()
          ((name params ...) template))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------------------------------
;; (let ([id val-expr] ...) body ...+)
;; (let proc-id ([id init-expr] ...) body ...+)
;; ------------------------------------------------
(define-syntax let
  (syntax-rules ()
    ((let ([id val] ...) body ...+)
      ((lambda (id ...) body ...+) val ...))
    ; Named let
    ;(let proc-id ([arg-id init-expr] ...) body ...+)
    ;   is equivalent to
    ;(letrec ([proc-id (lambda (arg-id ...) body ...+)]) (proc-id init-expr ...))
    ((let proc ([id init] ...) body ...+)
      ; 这里 letrec 因为 body 必须能引用到 proc
      (letrec ([proc (lambda (id ...) body ...+)])
        (proc init ...)))))

;; ------------------------------------------------
;; (let* ([id val-expr] ...) body ...+)
;; ------------------------------------------------
(define-syntax let*
  (syntax-rules ()
    ((let* ([id1 e1] [id2 e2] ...) body ...+) ;; >= 1
      (let ([id1 e1])
        (let* ([id2 e2] ...) body ...+)))
    ((let* () body ...+) ;; 0
      (let () body ...+))))

;; ------------------------------------------------
;; (letrec ([id val-expr] ...) body ...+)
;; ------------------------------------------------
(define-syntax-rule (letrec ([id val] ...) body ...+)
  ((lambda ()
     (define id val) ...
     body ...+)))
(define let-syntax let)
(define letrec-syntax letrec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (throw t) (xiao.lang2.Procedures/throw1 t))
(define (raise msg) (throw (java.lang.RuntimeException. msg)))
(define error raise)