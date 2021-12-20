;(define (values . things)
;  (call-with-current-continuation
;    (lambda (cont) (apply cont things))))
;
;(define (call-with-values producer consumer)
;  (apply consumer (producer)))

; (define call-with-current-continuation #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define values #f)
;(define call-with-values #f)
;
;(let ((magic (cons 'multiple 'values))) ;; 或者这里的 magic 直接引用 CorePrimitives.magic
;  (define magic?
;    (lambda (x)
;      (and (pair? x) (
;                       ;eq? ;; 这里暂时不能用 eq? java 代码和 ss都声明了 magic
;                       equals?
;                       (car x) magic))))
;  (set! call-with-current-continuation
;    (let ((primitive-call/cc
;            call-with-current-continuation))
;      (lambda (p)
;        (primitive-call/cc
;          (lambda (k)
;            (p (lambda args
;                 (k (apply values args)))))))))
;  (set! values
;    (lambda args
;      (if (and (not (null? args)) (null? (cdr args)))
;        (car args)
;        (cons magic args))))
;  (set! call-with-values
;    (lambda (producer consumer)
;      (let ((x (producer)))
;        (if (magic? x)
;          (apply consumer (cdr x))
;          (consumer x))))))

;; ------------------------------------------------
; (define-values (id ...) expr)
;; ------------------------------------------------
;(define-syntax define-values
;  (syntax-rules ()
;    ((define-values (id) expr) ; 1
;      (define id expr))
;    ((define-values (id ...+) expr) ; > 1
;      (begin
;        (define id #f) ...+
;        (call-with-values
;          (lambda () expr)
;          (lambda ids
;            ; todo 数量相等
;            (define (gen)
;              (define v (car ids))
;              (set! ids (cdr ids))
;              v)
;            (set! id (gen)) ...))))))

;; ------------------------------------------------
; (let-values ([(id ...) val-expr] ...) body ...+)
;; ------------------------------------------------
; todo 这里写的应该不对, 要开一个新作用域, 翻译成 let...
;(define-syntax let-values
;  (syntax-rules ()
;    ((let-values () body ...+) ; 0
;      (begin body ...+))
;    ((let-values ([(id ...) val] [(id-rest ...) val-rest] ...) body ...+) ; >= 1
;      (call-with-values
;        (lambda () val)
;        (lambda (id ...)
;          (let-values ([(id-rest ...) val-rest] ...) body ...+))))))

;; ------------------------------------------------
; (let*-values ([(id ...) expr] ...)  body ...+)
;; ------------------------------------------------
(define-syntax let*-values
  (syntax-rules ()
    ((let*-values ([(id1 ...) e1] [(id2 ...) e2] ...) body ...+) ;; >= 1
      (let-values ([(id1 ...) e1])
        (let*-values ([(id2 ...) e2] ...) body ...+)))
    ((let*-values () body ...+) ;; 0
      (let-values () body ...+))))

;; ------------------------------------------------
; (letrec-values ([(id ...) expr] ...) body ...+)
;; ------------------------------------------------
; 注意 val 只支持 lambda
;(define-syntax-rule (letrec-values ([(id ...) val] ...) body ...+)
;  ((lambda ()
;     (begin (define id #f) ...) ...
;     (call-with-values
;       (lambda () val)
;       (lambda ids
;         ; todo 数量相等
;         (define (gen)
;           (define v (car ids))
;           (set! ids (cdr ids))
;           v)
;         (set! id (gen)) ...)) ...
;     body ...+)))