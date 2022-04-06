(define-syntaxes (Ϟ1) ;ϟ
  (lambda (stx)
    (namespace-set-variable-value! 'core-syntax
      (lambda (d)
        ; 动态的添加一个#<procedure:core-syntax>
        ; 用当前 (Ϟ1) 的 scope 作为 core-syntax scope, 用来索引 core form 和 procedure
        ; 因为外头套了 let-values, 导致 coreStx 其实没有 syntax.ss 定义的宏
        ; 如果用 coreStx 可以这么写
        ; (datum->syntax (. xiao.lang.expander.Core coreStx) d)
        ; (namespace-syntax-introduce (datum->syntax #f d))
        (datum->syntax stx d)))
    (quote-syntax '())))
(Ϟ1)

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
(define-syntaxes (define-syntax)
  (lambda (stx)
    (let-values ([(m) (match-syntax stx '(define-syntax (id id:stx) body ...+))])
      (datum->syntax
        #'here
          `(define-syntaxes
             (,(m 'id))
             (lambda (,(m 'id:stx))
               (begin ,@(m 'body))))))))

(define-syntax (let-syntax stx)
  (let-values ([(m) (match-syntax stx '(let-syntax ([id trans-expr] ...) body ...+))])
    (datum->syntax
      #'here
      `(letrec-syntaxes+values
        ,(map
           (lambda (id trans) `((,id) ,trans))
           (m 'id)
           (m 'trans-expr))
        ()
        (begin ,@(m 'body))))))


;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;;--------------------------------------------
;; (define id expr)
;;--------------------------------------------
;; Function Shorthand
;; (define (head args) body ...+)
;;   head = id
;;        | (head args)
;;   args = arg-id ...
;;        | arg-id ... . rest-id
;;--------------------------------------------
;; Curried Function Shorthand
;; (define (head args) body ...+)
;;  head = id
;; 	 	  |	(head args)
;;  args = arg ...
;; 	 	  |	arg ... . rest-id
;;--------------------------------------------
;; 匹配 (head . formal) 而不是 (id . formal) 因为要支持 curry 定义
(define-syntax (define stx)
  (datum->syntax
    #'here
    (let-values ([(m) (try-match-syntax stx '(define id expr))])
      (if m `(define-values (,(m 'id)) ,(m 'expr))
        (let-values ([(m) (match-syntax stx '(define (head . formals) body ...+))])
            `(define ,(m 'head)
               (lambda ,(m 'formals) ,@(m 'body))))))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; let-values, letrec-values, letrec-syntaxes+values
;; define-syntaxes, define-values

;; let*-values, let, let*, letrec

;;--------------------------------------------------
;; (let*-values ([(id ...) val-expr] ...) body ...+)
;;--------------------------------------------------
(define-syntax (let*-values stx)
  (datum->syntax
    #'here
    (letrec-values ([(m) (match-syntax stx '(let*-values ([(id ...) val-expr] ...) body ...+))]
                     [(idss) (m 'id)]
                     [(vals) (m 'val-expr)]
                     [(body) (m 'body)])
      (if (null? idss)
          `(let-values () ,@(m 'body))
          `(let-values ([,(car idss) ,(car vals)])
             (let*-values
               ,(map list (cdr idss) (cdr vals))
               ,@(m 'body)))))))

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
(define-syntax (let stx)
  (datum->syntax
    #'here
    (let-values ([(m) (try-match-syntax stx '(let ([id val-expr] ...) body ...+))])
      (if m
          `(let-values
             ,(map
                (lambda (id val) `((,id) ,val))
                (m 'id)
                (m 'val-expr))
             ,@(m 'body))
        (letrec-values ([(m) (match-syntax stx '(let id:proc ([id init-expr] ...) body ...+))]
                         [(proc) (m 'id:proc)])
            `(letrec-values
               ([(,proc) (lambda ,(m 'id) ,@(m 'body))])
               (,proc ,@(m 'init-expr))))))))

;; ------------------------------------------------
;; Sequential Binding
;; (let* ([id val-expr] ...) body ...+)
;; ------------------------------------------------
(define-syntax (let* stx)
  (datum->syntax
    #'here
    (let-values ([(m) (match-syntax stx '(let* ([id val-expr] ...) body ...+))])
        `(let*-values
           ,(map
              (lambda (id val) `((,id) ,val))
              (m 'id)
              (m 'val-expr))
           ,@(m 'body)))))

;; ------------------------------------------------
;; Recursive Binding
;; (letrec ([id val-expr] ...) body ...+)
;; ------------------------------------------------
(define-syntax (letrec stx)
  (datum->syntax
    #'here
    (let-values ([(m) (match-syntax stx '(letrec ([id val-expr] ...) body ...+))])
        `(letrec-values
           ,(map
              (lambda (id val) `((,id) ,val))
              (m 'id)
              (m 'val-expr))
           ,@(m 'body)))))
;https://www.reddit.com/r/scheme/comments/devaa2/letrec_vs_letrec/
;The letrec is evaluated in some arbitrary order. It's order of evaluation is not guaranteed.
(define-syntax (letrec* stx)
  (datum->syntax
    #'here
    (let-values ([(m) (match-syntax stx '(letrec* ([id val-expr] ...) body ...+))])
        `(letrec-values
           ,(map
              (lambda (id val) `((,id) ,val))
              (m 'id)
              (m 'val-expr))
           ,@(m 'body)))))

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
(define-syntax (and stx)
  (datum->syntax
    #'here
    (letrec-values ([(m) (match-syntax stx '(and expr ...))]
                     [(exprs) (m 'expr)])
      (if (null? exprs)
        #t
        (if (null? (cdr exprs))
          ; 这里不用 let (car exprs) 因为只有一种 #f
          (car exprs)
            `(if ,(car exprs) (and ,@(cdr exprs)) #f))))))

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
(define-syntax (or stx)
  (datum->syntax
    #'here
    (letrec-values ([(m) (match-syntax stx '(or expr ...))]
                     [(exprs) (m 'expr)])
      (if (null? exprs)
        #f
        (if (null? (cdr exprs))
          (car exprs)
            `(let ([t ,(car exprs)])
               (if t t (or ,@(cdr exprs)))))))))

;; --------------------
;; (when test-expr body ...+)
;; --------------------
(define-syntax (when stx)
  (let-values ([(m) (match-syntax stx '(when test-expr body ...+))])
    (datum->syntax
      #'here
        `(if
           ,(m 'test-expr)
           (begin ,@(m 'body))
           (void)))))

;; --------------------
;; (unless test-expr body ...+)
;; --------------------
(define-syntax (unless stx)
  (let-values ([(m) (match-syntax stx '(unless test-expr body ...+))])
    (datum->syntax
      #'here
        `(if
           ,(m 'test-expr)
           (void)
           (begin ,@(m 'body))))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; interop

;(define-syntax (new stx)
;  (let-values ([(m) (match-syntax stx '(new id:class args ...))])
;    (datum->syntax #'here
;        `(.
;           (. xiao.lang.Interop$CallSite
;             (constructor
;               (symbol->class ',(syntax-e (m 'id:class)))))
;           (newInstance
;             (. (list ,@(m 'args)) (toArray)))))))

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
(define-syntax (doto stx)
  (datum->syntax
    #'here
    (let-values ([(m) (match-syntax stx '(doto ins (id:method args ...) ...))])
        `(let ([ins ,(m 'ins)])
           ,@(map
               (lambda (method args)
                   `(,method ins ,@args))
               (m 'id:method)
               (m 'args))
           ins))))

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
(define-syntax (.. stx)
  (datum->syntax
    #'here
    (letrec-values ([(m) (match-syntax stx '(.. ins-or-cls member ...+))]
                     [(ins-or-cls) (m 'ins-or-cls)]
                     [(members) (m 'member)])
      (if (null? (cdr members))
          `(. ,ins-or-cls ,(car members))
          `(.. (. ,ins-or-cls ,(car members)) ,@(cdr members))))))

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
(define-syntax (-> stx)
  (datum->syntax
    #'here
    (letrec-values ([(m) (match-syntax stx '(-> x (fst sec ...) ...+))]
                     [(x) (m 'x)]
                     [(fsts) (m 'fst)]
                     [(secss) (m 'sec)])
      (if (null? (cdr fsts))
          `(,(car fsts) ,x ,@(car secss))
          `(-> (,(car fsts) ,x ,@(car secss))
             ,@(map cons (cdr fsts) (cdr secss)))))))

;; ------------------------------------------------
;; (->> x forms+)
;; ------------------------------------------------
;(define-syntax ->>
;  (syntax-rules ()
;    ((->> x (fst sec ...))
;      (fst sec ... x))
;    ((->> x (fst sec ...) (fst-rest sec-rest ...) ...)
;      (->> (fst sec ... x) (fst-rest sec-rest ...) ...))))
(define-syntax (->> stx)
  (datum->syntax
    #'here
    (letrec-values ([(m) (match-syntax stx '(->> x (fst sec ...) ...+))]
                     [(x) (m 'x)]
                     [(fsts) (m 'fst)]
                     [(secss) (m 'sec)])
      (if (null? (cdr fsts))
          `(,(car fsts) ,@(car secss) ,x)
          `(->> (,(car fsts) ,@(car secss) ,x)
             ,@(map cons (cdr fsts) (cdr secss)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------------------------------------------
;; (instance? class instance)
;; ------------------------------------------------
;(define-syntax-rule (instance? class instance)
;  (.isInstance
;    (java.lang.Class/forName (symbol->string 'class))
;    instance))
(define-syntax (instance? stx)
  (datum->syntax
    #'here
    (let-values ([(m) (match-syntax stx '(instance? id:class instance))])
        `(.isInstance
           ;; .toString 应该是 symbol->string, 这里不能引用 core.ss 的过程
           ;; (string->class (.toString ',(m 'id:class)))
           (. java.lang.Class (forName (.toString ',(m 'id:class))))
           ,(m 'instance)))))

;; ------------------------------------------------
;; (hashmap (k v) ...)
;; ------------------------------------------------
;(define-syntax-rule (hashmap (k v) ...)
;  (doto
;    (java.util.HashMap.)
;    (.put k v) ...))
(define-syntax (hashmap stx)
  (datum->syntax
    #'here
    (letrec-values ([(m) (match-syntax stx '(hashmap (k v) ...))])
        `(doto
           (java.util.HashMap.)
           ,@(map (lambda (k v) `(.put ,k ,v)) (m 'k) (m 'v))))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
;; ------------------------------------------------
;; (do ([id init-expr step-expr-maybe] ...)
;;  (stop?-expr finish-expr ...)
;;  expr ...)
;; step-expr-maybe	 	=
;;                      | step-expr
;; ------------------------------------------------
; 只能写 0 或者 1 个 step
; 没写 step-expr 默认是 id
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
;        ; but 下面的写法会引入奇怪的 step, id 默认值
;        ($loop (begin id step ...) ...)))))
(define-syntax (do stx)
  (datum->syntax
    #'here
    (letrec-values ([(m)
                      (match-syntax stx
                          '(do ([id init step ...] ...) (stop? finish ...) expr ...))])
      ; 没有检查 0|1 个 step
        `(let loop
           ,(map list (m 'id) (m 'init))
           (if ,(m 'stop?)
             ; 默认 void
             (begin (void) ,@(m 'finish))
             (begin
               ,@(m 'expr)
               (loop
                 ,@(map
                     (lambda (id step)
                       ; 没有 step 默认 id
                         `(begin ,id ,@step))
                     (m 'id)
                     (m 'step)))))))))

;; ------------------------------------------------
;; racket:
;; (cond cond-clause ...)
;;  cond-clause	= [test-expr then-body ...+]
;;              |	[else then-body ...+]
;;              |	[test-expr => proc-expr]
;;              |	[test-expr]
;; A cond-clause that starts with else must be the last cond-clause.
;; If no cond-clauses are present, the result is #<void>.
;;
;; If only a [else then-body ...+] is present, then the then-bodys are evaluated.
;; The results from all but the last then-body are ignored.
;; The results of the last then-body, which is in tail position with respect to the cond form,
;; are the results for the whole cond form.
;;
;; Otherwise, the first test-expr is evaluated.
;; If it produces #f, then the result is the same as a cond form with the remaining cond-clauses,
;; in tail position with respect to the original cond form.
;; Otherwise, evaluation depends on the form of the cond-clause:
;;    [test-expr then-body ...+]
;;    The then-bodys are evaluated in order, and the results from all but the last then-body are ignored.
;;    The results of the last then-body, which is in tail position with respect to the cond form,
;;    provides the result for the whole cond form.
;;
;;    [test-expr => proc-expr]
;;    The proc-expr is evaluated, and it must produce a procedure that accepts one argument,
;;    otherwise the exn:fail:contract exception is raised.
;;    The procedure is applied to the result of test-expr in tail position with respect to the cond expression.
;;
;;    [test-expr]
;;    The result of the test-expr is returned as the result of the cond form. The test-expr is not in tail position.
;; ------------------------------------------------
;(define-syntax cond
;  (syntax-rules (else =>)
;    ((cond) (void))
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
;      (let ([$test test])
;        (if $test
;          $test
;          (cond clause ...))))))
;; step1: 处理 [cond]
;; step2: 处理 [else then-body ...+]
;; step3: 处理 [test-expr => proc-expr]
;; step4: 处理 [test-expr then-body ...+]
;; step5: 处理 [test-expr]
(define-syntax (cond stx)
  (datum->syntax
    #'here
    (let-values ([(m) (try-match-syntax stx '(cond))])
      (if m
          `(void)
        (let-values ([(m) (try-match-syntax stx '(cond [else then-body ...+]) '(else))])
          (if m
              `(let () ,@(m 'then-body))
            (letrec-values ([(m) (try-match-syntax stx '(cond [test-expr => proc-expr] clause ...) '(=>))]
                             [(=>?) (if m
                                      (if
                                        (.equals 'lexical (identifier-binding (datum->syntax stx '=>)))
                                        #f
                                        #t)
                                      #f)])
              (if =>?
                  `(let ([test ,(m 'test-expr)])
                     (if test
                       (,(m 'proc-expr) test)
                       (cond ,@(m 'clause))))
                (let-values ([(m) (try-match-syntax stx '(cond [test-expr then-body ...+] clause ...))])
                  (if m
                      `(let ([test ,(m 'test-expr)])
                         (if test
                           (let () ,@(m 'then-body))
                           (cond ,@(m 'clause))))
                    (let-values ([(m) (match-syntax stx '(cond [test-expr] clause ...))])
                        `(let ([test ,(m 'test-expr)])
                           (if test test (cond ,@(m 'clause)))))))))))))))

;; ------------------------------------------------
;; racket:
;; (case val-expr case-clause ...)
;;    case-clause = [(datum ...) then-body ...+]
;;                | [else then-body ...+]
;;
;; Evaluates val-expr and uses the result to select a case-clause.
;; The selected clause is the first one with a datum whose quoted form is equal? to the result of val-expr.
;; If no such datum is present, the else case-clause is selected;
;; if no else case-clause is present, either, then the result of the case form is #<void>.
;;
;; For the selected case-clause, the results of the last then-body, which is in tail position with respect to the case form, are the results for the whole case form.
;;
;; A case-clause that starts with else must be the last case-clause.
;; ------------------------------------------------
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
(define-syntax (case stx)
  (datum->syntax
    #'here
    (let-values ([(m) (try-match-syntax stx '(case val-expr))])
      (if m
          `(let () ,(m 'val-expr) (void))
        ; 支持 r7rs [else => (lambda (w) ... )]
        (let-values ([(m) (try-match-syntax stx '(case val-expr [else => then-body]) '(else =>))])
          (if m
              `(let ([val ,(m 'val-expr)])
                   (case val [else (,(m 'then-body) val)]))
            ; 支持 r7rs [(datum ...) => (lambda (w) ... )]
            (let-values ([(m) (try-match-syntax stx '(case val-expr [(datum ...) => then-body] clause ...) '(=>))])
              (if m
                  `(let ([val ,(m 'val-expr)])
                       (case val [,(m 'datum) (,(m 'then-body) val)] ,@(m 'clause)))
                (let-values ([(m) (try-match-syntax stx '(case val-expr [else then-body ...+]) '(else))])
                  (if m
                      `(let () ,(m 'val-expr) ,@(m 'then-body))
                    (let-values ([(m) (match-syntax stx '(case val-expr [(datum ...) then-body ...+] clause ...))])
                        `(let ([val ,(m 'val-expr)])
                           (if (member val '(,@(m 'datum)))
                             (begin ,@(m 'then-body))
                             (case val ,@(m 'clause)))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (delay body ...+)
(define-syntax (delay stx)
  (datum->syntax
    #'here
    (let-values ([(m) (match-syntax stx '(delay body ...+))])
        `(let ([forced #f]
                 [memo #f])
           (lambda ()
             (if forced
               memo
               (begin
                 (set! memo (begin ,@(m 'body)))
                 (set! forced #t)
                 memo)))))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
