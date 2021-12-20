;; https://legacy.cs.indiana.edu/~dyb/pubs/fixing-letrec.pdf
;;(letrec ([x1 e1] ... [xn en]) body)
;;→ (let ([x1 undefined] ... [xn undefined])
;;    (let ([t1 e1] ... [tn en])
;;      (set! x1 t1)
;;      ...
;;      (set! xn tn))
;;    body)
;; 因为没法批量引入局部变量, 重新改写了下论文中的逻辑, 比上面写法的区别是
;; 比如 (letrec ([a b] [b a]) (println a) (println b))
;; a b 都是 'undefine, 上面写法直接报错, 貌似直接报错更好
(define-syntax letrec1
  (syntax-rules ()
    ((letrec1 () body ...) ;; 0
      (let () body ...))
    ((letrec1 ([id val-expr] [id-rest val-expr-rest] ...) body ...) ; >= 1
      (let ([tmp val-expr])
        (set! id tmp)
        (letrec1 ([id-rest val-expr-rest] ...) body ...)))))
(define-syntax letrec
  (syntax-rules ()
    ((letrec ([id val-expr] ...) body ...)
      (let ([id 'undefined] ...)
        (letrec1 ([id val-expr] ...) body ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax do
  (syntax-rules ()
    ; 只能写 0 或者 1 个 step, 多了无效
    ; 没写 step 默认是 variable
    ((do ((variable init step ...) ...)
       (test expression ...)
       command ...)
      (let loop ((variable init) ...)
        (if test
          (begin expression ...)
          (begin
            command ...
            (loop (do "step" variable step ...) ...)))))
    ; tricky
    ; 处理 0 或者 1 个 step
    ; 如果多余 1 个 step 的话, command 会被执行一次, 然后报错
    ; 理论上应该立即报错, 如果只执行一次满足终止条件也不会报错，总之也不是一个严谨的实现
    ((do "step" variable)
      variable)
    ((do "step" variable step)
      step)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ------------------------------------------------
;; 改写 lambda, body 自动加 begin
;;; ------------------------------------------------
;(define primitive-lambda lambda)
;(define-syntax lambda
;  (syntax-rules ()
;    ((lambda (param ...) form)
;      (primitive-lambda (param ...) form))
;    ((lambda (param ...) form ...)
;      (primitive-lambda (param ...) (begin form ...)))))

;; ------------------------------------------------
;; 改写 define 支持 define lambda
;; ------------------------------------------------
;(define primitive-define define)
;(define-syntax define
;  (syntax-rules ()
;    ((define (name param ...) form ...+)
;      (primitive-define name (lambda (param ...) form ...)))
;    ((define name form)
;      (primitive-define name form))))
