;;--------------------------------------------
;; (define id expr)
;; (define (head args) body ...+)
;;   head = id |	(head args)
;;   args = arg-id ... | arg-id ... . rest-id
;;--------------------------------------------
(define-syntaxes (define)
  (lambda (stx)
    ;; step1: 处理 (define id expr)
    (let-values ([(m) (try-match-syntax stx '(define id expr))])
      (if m
        (let-values ([(id) (m 'id)]
                      [(expr) (m 'expr)])
          (datum->syntax
            (quote-syntax here)
            (list 'define-values (list id) expr)))
        ;; step2: 处理 (define (id . formal) body ...+)
        ;; 注意这里不能写成 ' 因为 dot 在 quote 中会被处理成 cons
        ;; '(define (id . formal) body ...+) ==> (list 'define (cons 'id ;formal) 'body '...+)
        (let-values ([(m) (try-match-syntax stx (list 'define (list 'id '. 'formal) 'body '...+) '(.))])
          (if m
            (let-values ([(id) (m 'id)]
                          [(formal) (m 'formal)]
                          [(body) (m 'body)])
              (datum->syntax
                (quote-syntax here)
                (list 'define id (cons 'lambda (cons formal body)))))
            ;; step3: 处理 (define (id formals ...) body ...+)  ==> (define id (lambda (formals ...) body ...)
            (letrec-values ([(m) (match-syntax stx '(define (id formals ...) body ...+))]
                             [(id) (m 'id)]
                             [(formals) (m 'formals)]
                             [(body) (m 'body)])
              (datum->syntax
                (quote-syntax here)
                (list 'define id (cons 'lambda (cons formals body)))))))))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; let-values, letrec-values, letrec-syntaxes+values
;; define-syntaxes, define-values

;; let*-values, let, let*, letrec

;;--------------------------------------------------
;; (let*-values ([(id ...) val-expr] ...) body ...+)
;;--------------------------------------------------
(define-syntaxes (let*-values)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(let*-values ([(id ...) val-expr] ...) body ...+))]
                     [(idss) (m 'id)]
                     [(vals) (m 'val-expr)]
                     [(body) (m 'body)])
      (if (null? idss)
        (datum->syntax
          (quote-syntax here)
          (cons 'let-values (cons '() body)))
        (datum->syntax
          (quote-syntax here)
          (cons 'let-values
            (cons
              (list (list (car idss) (car vals)))
              (list (cons 'let*-values
                      (cons
                        (map
                          (lambda (ids val) (list ids val))
                          (cdr idss)
                          (cdr vals))
                        body))))))))))

;; ------------------------------------------------
;; (let ([id val-expr] ...) body ...+)
;; Named let
;; (let proc-id ([id init-expr] ...) body ...+)
;;    is equivalent to
;;    (letrec ([proc-id (lambda (id ...) body ...+)]) (proc-id init-expr ...))
;; ------------------------------------------------
;(define-syntax let
;  (syntax-rules ()
;    ((let ([id val] ...) body ...+)
;      ((lambda (id ...) body ...+) val ...))
;    ((let proc ([id init] ...) body ...+)
;      ; 这里 letrec 因为 body 必须能引用到 proc
;      (letrec ([proc (lambda (id ...) body ...+)])
;        (proc init ...)))))
(define-syntaxes (let)
  (lambda (stx)
    (let-values ([(m) (try-match-syntax stx '(let ([id val-expr] ...) body ...+))])
      (if m
        (let-values ([(ids) (m 'id)]
                      [(vals) (m 'val-expr)]
                      [(body) (m 'body)])
          (datum->syntax
            (quote-syntax here)
            (cons 'let-values
              (cons
                (map (lambda (id val) (list (list id) val)) ids vals)
                body))))
        (letrec-values ([(m) (match-syntax stx '(let id:proc ([id init-expr] ...) body ...+))]
                         [(proc) (m 'id:proc)]
                         [(ids) (m 'id)]
                         [(inits) (m 'init-expr)]
                         [(body) (m 'body)])
          (datum->syntax
            (quote-syntax here)
            (list 'letrec-values
              (list (list
                      (list proc)
                      (cons
                          'lambda
                        (cons ids body))))
              (cons proc inits))))))))

;; ------------------------------------------------
;; Sequential Binding
;; (let* ([id val-expr] ...) body ...+)
;; ------------------------------------------------
(define-syntaxes (let*)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(let* ([id val-expr] ...) body ...+))]
                     [(ids) (m 'id)]
                     [(vals) (m 'val-expr)]
                     [(body) (m 'body)])
      (datum->syntax
        (quote-syntax here)
        (cons 'let*-values
          (cons
            (map (lambda (id val) (list (list id) val)) ids vals)
            body))))))

;; ------------------------------------------------
;; Recursive Binding
;; (letrec ([id val-expr] ...) body ...+)
;; ------------------------------------------------
(define-syntaxes (letrec)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(letrec ([id val-expr] ...) body ...+))]
                     [(ids) (m 'id)]
                     [(vals) (m 'val-expr)]
                     [(body) (m 'body)])
      (datum->syntax
        (quote-syntax here)
        (cons 'letrec-values
          (cons
            (map (lambda (id val) (list (list id) val)) ids vals)
            body))))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; condition

;; --------------------
;; (and expr ...)
;; --------------------
;(define-syntax and
;  (syntax-rules ()
;    ((and) #t)
;    ((and test) test)
;    ((and test rest ...)
;      (let ([$test test])
;        (if $test
;          (and rest ...)
;          $test)))))
(define-syntaxes (and)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(and expr ...))]
                     [(exprs) (m 'expr)])
      (if (null? exprs)
        (quote-syntax #t)
        (datum->syntax
          (quote-syntax here)
          (if (null? (cdr exprs))
            ; 这里不用 let (car exprs) 因为只有一种 #f
            (car exprs)
            (list 'if
              (car exprs)
              (cons 'and (cdr exprs))
              (quote-syntax #f))))))))

;; --------------------
;; (or expr ...)
;; --------------------
;(define-syntax or
;  (syntax-rules ()
;    ((or) #f)
;    ((or test) test)
;    ((or test rest ...)
;      (let ([$test test])
;        (if $test
;          $test
;          (or rest ...))))))
(define-syntaxes (or)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(or expr ...))]
                     [(exprs) (m 'expr)])
      (if (null? exprs)
        (quote-syntax #f)
        (datum->syntax
          (quote-syntax here)
          (if (null? (cdr exprs))
            (car exprs)
            (list 'let (list (list 't (car exprs)))
              (list 'if 't 't (cons 'or (cdr exprs))))))))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; interop

;; ------------------------------------------------
;; (doto instance-expr
;;   (instanceMethodName-symbol args ...) ...)
;; (doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
;; ------------------------------------------------
;(define-syntax-rule (doto instance-expr
;                      (instance-method args ...) ...)
;  (let ([$instance instance-expr])
;    (instance-method $instance args ...) ...
;    $instance))
(define-syntaxes (doto)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(doto ins (id:method args ...) ...))]
                     [(ins) (m 'ins)]
                     [(methods) (m 'id:method)]
                     [(argss) (m 'args)])
      (datum->syntax
        (quote-syntax here)
        (list 'let (list (list 'ins ins))
          (cons 'begin
            (map (lambda (method args)
                   (cons method (cons 'ins args))) methods argss))
            'ins)))))

;; ------------------------------------------------
;; (.. instance-expr member+)
;; (.. Classname-symbol member+)
;; ------------------------------------------------
;(define-syntax ..
;  (syntax-rules ()
;    ((.. ins-or-name member)
;      (. ins-or-name member))
;    ((.. ins-or-name member member-rest ...)
;      (.. (. ins-or-name member) member-rest ...))))
(define-syntaxes (..)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(.. ins-or-cls member ...+))]
                     [(ins-or-cls) (m 'ins-or-cls)]
                     [(members) (m 'member)])
      (datum->syntax
        (quote-syntax here)
        (if (null? (cdr members))
          (list '. ins-or-cls (car members))
          (cons '..
            (cons
              (list '. ins-or-cls (car members))
              (cdr members))))))))

;; ------------------------------------------------
;; (-> x forms+)
;; Clojure: convert nested function calls into a linear flow of function calls
;; improving readability.
;; ------------------------------------------------
;(define-syntax ->
;  (syntax-rules ()
;    ((-> x (fst sec ...))
;      (fst x sec ...))
;    ((-> x (fst sec ...) (fst-rest sec-rest ...) ...)
;      (-> (fst x sec ...) (fst-rest sec-rest ...) ...))))
(define-syntaxes (->)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(-> x (fst sec ...) ...+))]
                     [(x) (m 'x)]
                     [(fsts) (m 'fst)]
                     [(secss) (m 'sec)])
      (datum->syntax
        (quote-syntax here)
        (if (null? (cdr fsts))
          (cons (car fsts) (cons x (car secss)))
          (cons '->
            (cons (cons (car fsts) (cons x (car secss)))
              (map (lambda (fst-rest sec-rest)
                     (cons fst-rest sec-rest))
                (cdr fsts) (cdr secss)))))))))
;(define-syntaxes (->)
;  (lambda (stx)
;    (letrec-values ([(m) (match-syntax stx '(-> x (fst sec ...) ...+))]
;                     [(x) (m 'x)]
;                     [(fsts) (m 'fst)]
;                     [(secss) (m 'sec)])
;      (datum->syntax
;        (quote-syntax here)
;        (if (null? (cdr fsts))
;            `(,(car fsts) ,x ,@(car secss))
;            `(-> ,`(,(car fsts) ,x ,@(car secss))
;               ,@(map (lambda (fst-rest sec-rest)
;                          `(,fst-rest ,@sec-rest))
;                   (cdr fsts) (cdr secss))))))))

;; ------------------------------------------------
;; (->> x forms+)
;; ------------------------------------------------
;(define-syntax ->>
;  (syntax-rules ()
;    ((->> x (fst sec ...))
;      (fst sec ... x))
;    ((->> x (fst sec ...) (fst-rest sec-rest ...) ...)
;      (->> (fst sec ... x) (fst-rest sec-rest ...) ...))))
(define-syntaxes (->>)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(->> x (fst sec ...) ...+))]
                     [(x) (m 'x)]
                     [(fsts) (m 'fst)]
                     [(secss) (m 'sec)])
      (datum->syntax
        (quote-syntax here)
        (if (null? (cdr fsts))
          `(,(car fsts) ,@(car secss) ,x)
          `(->> ,`(,(car fsts) ,@(car secss) ,x)
             ,@(map (lambda (fst-rest sec-rest)
                        `(,fst-rest ,@sec-rest))
                 (cdr fsts) (cdr secss))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------------------------------
;; (instance? class instance)
;; ------------------------------------------------
;(define-syntax-rule (instance? class instance)
;  (.isInstance
;    (java.lang.Class/forName (symbol->string 'class))
;    instance))
;(define-syntaxes (instance?)
;  (lambda (stx)
;    (letrec-values ([(m) (match-syntax stx '(instance? id:class instance))]
;                     [(class) (m 'id:class)]
;                     [(instance) (m 'instance)])
;      (datum->syntax
;        (quote-syntax here)
;        (list '.isInstance
;          (list '. 'java.lang.Class
;            (list 'forName
;              (list 'symbol->string
;                (list 'quote class))))
;          instance)))))
(define-syntaxes (instance?)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(instance? id:class instance))]
                     [(class) (m 'id:class)]
                     [(instance) (m 'instance)])
      (datum->syntax
        (quote-syntax here)
        `(.isInstance
           (. java.lang.Class (forName (symbol->string ',class)))
          ,instance)))))

;; ------------------------------------------------
;; (hashmap (k v) ...)
;; ------------------------------------------------
;(define-syntax-rule (hashmap (k v) ...)
;  (doto
;    (java.util.HashMap.)
;    (.put k v) ...))
(define-syntaxes (hashmap)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(hashmap (k v) ...))]
                     [(ks) (m 'k)]
                     [(vs) (m 'v)])
      (datum->syntax
        (quote-syntax here)
          `(doto
             (java.util.HashMap.)
             ,@(map (lambda (k v)
                        `(.put ,k ,v)) ks vs))))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;;
;;; ------------------------------------------------
;;; (do ([id init-expr step-expr-maybe] ...)
;;;  (stop?-expr finish-expr ...)
;;;  expr ...)
;;; step-expr-maybe	 	=
;;;                      | step-expr
;;; ------------------------------------------------
;; 只能写 0 或者 1 个 step
;; 没写 step-expr 默认是 id
;(define-syntax-rule (do ((id init step ...) ...)
;                      (stop? finish ...)
;                      expr ...)
;  (let $loop ((id init) ...)
;    (if stop?
;      (begin finish ...)
;      (begin
;        expr ...
;        ; 处理 0 或者 1 个 step
;        ; 这么写有个问题, 如果传了多个 step, 会默认使用最后一个 step
;        ; but 下面的写法会引入奇怪的 step
;        ($loop (begin id step ...) ...)))))
;
;
;;; ------------------------------------------------
;;; (cond cond-clause ...)
;;;        cond-clause	 	=
;;;          [test-expr then-body ...+]
;;;      |	 	[else then-body ...+]
;;;      |	 	[test-expr => proc-expr]
;;;      |	 	[test-expr]
;;; ------------------------------------------------
;(define-syntax cond
;  (syntax-rules (else =>)
;    ((cond) (void))  ; 无任何匹配返回啥?? void ? false
;    ((cond [else then then-rest ...]) ; else body 至少一个 form
;      (begin then then-rest ...))
;    ((cond [test => proc] clause ...)
;      (let ([$test test])
;        (if $test
;          (proc $test)
;          (cond clause ...))))
;    ((cond [test then then-rest ...] clause ...) ; then body 至少一个 form
;      (if test
;        (begin then then-rest ...)
;        (cond clause ...)))
;    ((cond [test] clause ...) ; == 0
;      test)))
;
;;; ------------------------------------------------
;;; (case val-expr case-clause ...)
;;;    case-clause	 	=	 	[(datum ...) then-body ...+]
;;;                  |	 	[else then-body ...+]
;;; ------------------------------------------------
;(define-syntax case
;  (syntax-rules (else)
;    ((case val)
;      (begin
;        val
;        (void))) ; void ? false
;    ((case val [else then then-rest ...])
;      (begin
;        val
;        then then-rest ...))
;    ((case val
;       [(datum ...) then then-rest ...]
;       clause ...)
;      (let ([$val val])
;        ; 注意这里 quote 了一次
;        (if (member $val '(datum ...))
;          (begin then then-rest ...)
;          (case $val clause ...))))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-syntax-rule (delay expr)
;  (let (($forced #f)
;         ($memo #f))
;    (lambda ()
;      (if $forced
;        $memo
;        (begin
;          (set! $memo expr)
;          (set! $forced #t)
;          $memo)))))
;
;(define (force promise) (promise))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; assert
;(define-syntax-rule (assert condtional-form)
;  (if condtional-form (void) (raise (str "assert fail: " (quote condtional-form)))))
(define-syntaxes (assert)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(assert test))]
                     [(test) (m 'test)])
      (datum->syntax
        (quote-syntax here)
          `(if ,test (void) (raise (str "assert fail: " ',test)))))))

;(define-syntax-rule (assert-equals a b)
;  ; 这里要用 equals 不能用.equals 否则 NPE
;  (assert (equals? a b)))
(define-syntaxes (assert-equals)
  (lambda (stx)
    (letrec-values ([(m) (match-syntax stx '(assert-equals a b))]
                     [(a) (m 'a)]
                     [(b) (m 'b)])
      (datum->syntax
        (quote-syntax here)
          `(assert (equals? ,a ,b))))))

; todo
;(error message-sym) → any
;message-sym : symbol?
;(error message-str v ...) → any
;message-str : string?
;v : any/c
;(error who-sym format-str v ...) → any
;who-sym : symbol?
;format-str : string?
;v : any/c
; 这里实现有问题, format 有问题
;(define-syntax error
;  (syntax-rules ()
;    ((error msg-sym)
;      (raise (str "error: " (symbol->string msg-sym))))
;    ;((error msg-str v ...) ())
;    ((error who-sym fmt-str v ...)
;      ; 可以把 v... 变成一个数组
;      (raise (str (symbol->string who-sym) ": " (java.lang.String/format fmt-str v ...))))))