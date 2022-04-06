; 可以 重新 define let 的 rhs a, 因为 let body 被展开成 letrec-values
(let ([a 1])
  (define a 2)
  (assert-equals 2 a))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

;; (lambda (stx{a-lam})
;;  (let ([id{a-lam, b-let} #'x{a-lam}])
;;     #`(let ([x{a-lam, b-let} 1])
;;         #,id{a-lam, b-let})))
(define-syntaxes (test)
  (lambda (stx)
    ;ScopeSet {...  #<intdef: 3139>, #<macro: 3214>}
    (println (.scopes stx))
    ;ScopeSet {...  #<intdef: 3139>}
    (println (.scopes (quote-syntax here)))
    ;ScopeSet {...  #<intdef: 3139>, #<local-lambda: 3140>, #<intdef: 3141>}
    (println (.scopes (quote-syntax here 'local)))

    (let-values ([(id) #'x])
      (println (.scopes id))
      (datum->syntax
        #'here
          `(let ([x 1])
             ,id)))))
(assert-equals 1 (test))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀


(let ()
  (define x 12)
  (define-syntax (m stx)
    (datum->syntax #'here
      ; 这里的 x 有 macro scope
        `(let ([x 10])
           ; x: 这里有 use-site scope
           ; 这俩 x 不是 bound-identifier=?
           ,(cadr (syntax-e stx)))))
  ; 这里的 x 有 use-site scope
  (assert-equals 12 (m x)))

(let ()
  ; definition 与 use-site scope 被加入时的 context 相同时,
  ; 指的是 展开过程的 ExpandContext 相同, 这里是 expandBody 循环中 没有创建新的 context
  ; definition 中绑定标识符的 use-site scope 会被忽略
  ; 这种 对 use-site scopes 的特殊处理是为了宏中的定义对外可见
  (define-syntax (m stx)
    (datum->syntax #'here
        `(define ,(cadr (syntax-e stx)) 5)))
  (m x)
  (assert-equals 5 x))


(let ()
  (define-syntax (m stx)
    (datum->syntax #'here
        `(let ([x 4])
           ; x: 这里有 use-site scope, scope 不是最后 x 的子集
           ; 这个场景 use-site scope 不会被忽略；
           ; 因为这里 let 的绑定不是 (m x) 展开时的 definition contxt 不相同
           ; 因为 let 在 expandBody 时 partial expand, 延迟展开整个嵌套的 let
           ; 延迟到最后用 finishBody context 来展开
           ; finishBody contex 不是原来的 context, 不相同,
           ; 通过 ExpandContext 的 useSiteScopes 属性记录清空来实现, fin ctx 没有记录 use-site scope 用来 remove
           (let ([,(cadr (syntax-e stx)) 5])
             x))))
  ; 这里的 x 有 use-site scope
  (assert-equals 4 (m x)))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
"hygienic vs unhygienic"
(let-syntax ([while
               (lambda (stx)
                 (letrec-values ([(m) (match-syntax stx '(while test body ...))]
                                  [(test bodys) (values (m 'test) (m 'body))]
                                  [(here) #'here])
                   (datum->syntax here
                       `(let loop ()
                          (when ,test
                            ,@bodys
                            (loop))))))])
  (define x 2)
  (let ([let 5])
    (while (< x 10)
;      (printf "x = %s\n" x)
      (set! x (add1 x))))
  (assert-equals 10 x))


(let-syntax ([while
               (lambda (stx)
                 (letrec-values ([(m) (match-syntax stx '(while test body ...))]
                                  [(test bodys) (values (m 'test) (m 'body))]
                                  [(here) #'here]
                                  ; 与 stx 相同的 scope, 所以when 的 body 中才能直接使用
                                  [(unhygienic-it) (datum->syntax stx 'it)])
                   (datum->syntax here
                       `(let loop ()
                          (let ([,unhygienic-it ,test])
                            (when ,unhygienic-it
                              ,@bodys
                              (loop)))))))])
  (define x 2)
  (let ([let 5])
    (while (< x 10)
;      (println it)
      (assert-equals #t it)
;      (printf "x = %s\n" x)
      (set! x (add1 x))))
  (assert-equals 10 x))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

"注意覆盖"
(letrec-syntaxes+values
  ([(if1) (lambda (stx)
            (define-values (e) (syntax-e stx))
            (datum->syntax
              #'here
              ; 这里 if 引用到下面定义的 lambda, letrec 语义
                `(if ,(cadr e) ,(caddr e) ,(cadddr e))))])
  ([(if) (lambda (x y z) "HELLO")])
  (assert-equals "HELLO" (if1 1 2 3)))

(let ()
  (define-syntax (if1 stx)
    (define-values (e) (syntax-e stx))
    (datum->syntax
      #'here
      ; 这里 if 引用到下面定义的 lambda
      ; let 内部 是 letrec 语义, 所以一样影响
        `(if ,(cadr e) ,(caddr e) ,(cadddr e))))
  (define if (lambda (x y z) "HELLO"))
  (assert-equals "HELLO" (if1 1 2 3)))

(let ()
  (define-syntax (if1 stx)
    (define-values (e) (syntax-e stx))
    (datum->syntax
      #'here
      ; if 引用的是 coreForm
        `(if ,(cadr e) ,(caddr e) ,(cadddr e))))
  (let ()
    (define if (lambda (x y z) "HELLO"))
    (assert-equals 2 (if1 1 2 3))))

; racket 测试代码, 语义一样, 报错
;(let ()
;  (define let 42)
;  (define-syntax (test stx)
;    (syntax-case stx ()
;      ((_ a)
;        #'(let ([t a]) t))))
;  (test 1))
(let ()
  (define let 42)
  (define-syntaxes (test)
    (lambda (stx)
      (let-values ([(m) (match-syntax stx '(test a))])
        (datum->syntax
          #'here
            `(
;               let ;; 报错
               ,(core-syntax 'let)
               ([t ,(m 'a)]) t)))))
  ;OR
  ;(define let 42)
  (assert-equals 1 (test 1))
  (assert-equals let 42))

; racket 测试代码, 语义一样
;(let ()
;  (define-syntax (test stx)
;    (syntax-case stx ()
;      ((_ a)
;        #'(let ([t a]) t))))
;  (let ()
;    (define let 42)
;    (test 1)))
(let ()
  (define-syntaxes (test)
    (lambda (stx)
      (let-values ([(m) (match-syntax stx '(test a))])
        (datum->syntax
          #'here
            `(let ([t ,(m 'a)]) t)))))
  (let ()
    (define let 42)
    (test 1)))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

"注意哪里的 x"
(let ()
  (define-syntaxes (define-five)
    (lambda (stx)
      (let-values ([(here) #'here])
        (datum->syntax
          here
            `(begin
               ; 这里用的定义处的作用域, 所以 define 对外可见
               (define ,(cadr (syntax-e stx)) 5)
               x)))))

  (define-syntaxes (define-other-five)
    (lambda (stx)
      (let-values ([(here) #'here])
        (if (.equals (.scopes here) (.scopes #'here))
          (void)
          (raise "ERR"))
        (datum->syntax
          #'here
            `(begin
               ; 这里 x 的最后宏展开后会保留 macro 作用域, x 定义对外不可见
               (define x 5)
               ; 这里访问的不是 (define x 5), 而是使用处的 x
               ; 这里会访问到 (define-five x) 内部定义的 x
               ,(cadr (syntax-e stx)))))))

  ; 定义 x = 5
  (define-five x)
  ; 返回 外部 x = 5
  (define-other-five x)
  ; define-five 定义的 5
  (assert-equals 5 x))
;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀


(let ()
  (define-syntaxes (x) (lambda (stx) (quote-syntax 7)))
  (define-syntaxes (y) (lambda (stx) (quote-syntax x)))
  (assert-equals 7 y))

; let-syntax
(let-syntax ([a (lambda (stx) (quote-syntax 42))])
  (assert-equals 42 a))

; letrec-syntaxes+values
(letrec-syntaxes+values
  ([(swap) (lambda (stx)
             (let-values ([(x) (car (cdr (syntax-e stx)))]
                           [(y) (car (cdr (cdr (syntax-e stx))))])
               (datum->syntax
                 #'here
                   `(let ([tmp ,x])
                      (set! ,x ,y)
                      (set! ,y tmp)))))]
  [(if1) (lambda (stx)
           (let-values ([(m) (match-syntax stx '(if test then else))])
             (datum->syntax
               #'here
                 `(if ,(m 'test) ,(m 'then) ,(m 'else)))))])
  ()
  (define-values (a b) (values 1 2))
  (swap a b)
  (assert-equals a 2)
  (assert-equals b 1)

  (assert-equals "TRUE" (if1 #t "TRUE" (raise "FALSE")))
  (assert-equals "FALSE" (if1 #f (raise "TRUE") "FALSE")))


;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
"健康: 手动控制每个符号的 scope-set"
  '(let-values
     (((t1) #t)
       ((if1)
         (λ (x y z) "oops")))
     (let-values
       (((t2) #f))
       (if t2 t2 t1)))
;(println
  (xiao.lang.PrettyPrint/pp
    (expand '(let-values ()
               (define-syntaxes (my-or)
                 (lambda (stx)
                   (let-values ([(a) (car (cdr (syntax-e stx)))]
                                 [(b) (car (cdr (cdr (syntax-e stx))))]
                                 [(t) (quote-syntax t)])
                     ; 手动把 let-values\t\if 都设置成当前 scope-set
                     (datum->syntax
                       #f
                         `(,(quote-syntax let-values)
                            ([(,t) ,a])
                            (,(quote-syntax if) ,t ,t ,b))))))
               (let-values
                 ([(t) #t]
                   [(if) (λ (x y z) "oops")])
                 (my-or #f t)))))
;)

(letrec-syntaxes+values
  ([(my-or)
     (lambda (stx)
       (let-values ([(a) (car (cdr (syntax-e stx)))]
                     [(b) (car (cdr (cdr (syntax-e stx))))]
                     [(t) (quote-syntax t)])
         (datum->syntax
           #f
             `(,(quote-syntax let-values)
                ([(,t) ,a])
                (,(quote-syntax if) ,t ,t ,b)))))])
  ()
  (assert-equals #t (my-or #t #f))
  (assert-equals #t (my-or #f #t))
  (assert-equals #t
    (let-values ([(t) #t])
      (my-or #f t)))
  (assert-equals #t
    (let-values
      ([(t) #t]
        [(if) (λ (x y z) "oops")])
      (my-or #f t))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
"健康: quote-syntax here 全局处理"
  '(let-values
     (((t1) #t)
       ((if1) (λ (x y z) "oops")))
     (let-values
       (((t2) #f))
       (if t2 t2 t1)))
;(println
  (xiao.lang.PrettyPrint/pp
    (expand '(let-values ()
               (define-syntaxes (my-or)
                 (lambda (stx)
                   (let-values ([(a) (car (cdr (syntax-e stx)))]
                                 [(b) (car (cdr (cdr (syntax-e stx))))])
                     ; 自动把除 a、b 外其他符号的 scope-set 都设置成当前 scope-set
                     ; a、b 已经是 syntax 不受 datum->syntax 影响
                     (datum->syntax
                       #'here
                         `(let-values ([(t) ,a])
                            (if t t ,b))))))
               (let-values
                 ([(t) #t]
                   [(if) (λ (x y z) "oops")])
                 (my-or #f t)))))
;)

(letrec-syntaxes+values
  ([(my-or)
     (lambda (stx)
       (let-values ([(a) (car (cdr (syntax-e stx)))]
                     [(b) (car (cdr (cdr (syntax-e stx))))])
         (datum->syntax
           #'here
             `(let-values ([(t) ,a])
                (if t t ,b)))))])
  ()
  (assert-equals #t (my-or #t #f))
  (assert-equals #t (my-or #f #t))
  (assert-equals #t
    (let-values ([(t) #t])
      (my-or #f t)))
  (assert-equals #t
    (let-values
      ([(t) #t]
        [(if) (λ (x y z) "oops")])
      (my-or #f t))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
"非健康: 第二个参数动态作用域, 外部传入宏的符号t 捕获宏引入的局部绑定 t"
  '(let-values
     (((t) #t)
       ((if1) (λ (x y z) "oops")))
     (let-values
       (((t1) #f))
       (if t1 t1 t1)))
;(println
  (xiao.lang.PrettyPrint/pp
    (expand '(let-values ()
               (define-syntaxes (my-or)
                 (lambda (stx)

                   ; 相同
                   (println (.scopes (car (cdr (cdr (syntax-e stx))))))
                   (println (.scopes stx))

                   ; 相同
                   (println (.scopes #'here))
                   (println (.-scopes
                              (datum->syntax
                                #'here
                                (syntax-e (car (cdr (cdr (syntax-e stx))))))))


                   (let-values ([(a) (car (cdr (syntax-e stx)))]
                                 [(b) (car (cdr (cdr (syntax-e stx))))])
                     (datum->syntax
                       #'here
                       ; todo quote-syntax 处理成去掉用来 scope-set
                       ; 保留符号, 去掉 b 原来的 scope-set, 替换成跟 t 一样的 scope-set
                       ; 逻辑上 (my-or t1 t1 t2) --> (my-or #f #f #f)
                         `(let-values ([(t) ,a])
                            (if t t ,(syntax-e b)))))))
               (let-values
                 ([(t) #t]
                   [(if) (λ (x y z) "oops")])
                 (my-or #f t)))))
;)

(letrec-syntaxes+values
  ([(my-or)
     (lambda (stx)
       (let-values ([(a) (car (cdr (syntax-e stx)))]
                     [(b) (car (cdr (cdr (syntax-e stx))))])
         (datum->syntax
           #'here
             `(let-values ([(t) ,a])
                (if t t ,(syntax-e b))))))])
  ()
  (assert-equals #t (my-or #t #f))
  (assert-equals #t (my-or #f #t))
  (assert-equals #f
    (let-values ([(t) #t])
      (my-or #f t)))
  (assert-equals #f
    (let-values
      ([(t) #t]
        [(if) (λ (x y z) "oops")])
      (my-or #f t))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
"非健康: 宏内部引入的局部绑定t 遮盖了外部 t"
  '(let-values
     (((t) #t)
       ((if1) (λ (x y z) "oops")))
     (let-values
       (((t1) #f))
       (if t1 t1 t1)))
;(println
  (xiao.lang.PrettyPrint/pp
    (expand '(let-values ()
               (define-syntaxes (my-or)
                 (lambda (stx)

                   ; 相同
                   (println (.scopes (datum->syntax stx 't)))
                   (println (.scopes stx))

                   (println (.scopes #'here))

                   (let-values ([(a) (car (cdr (syntax-e stx)))]
                                 [(b) (car (cdr (cdr (syntax-e stx))))]
                                 [(t) (datum->syntax stx 't)])
                     (datum->syntax
                       #'here
                         `(let-values ([(,t) ,a])
                            (if ,t ,t ,b))))))
               ; 这里 ,t 跟 stx 的 scope-set 一样
               ; 导致 ,b 的 t 查找最近的 t 找到了 ,t 也就是 ,a 的值
               ; 逻辑上 (my-or t t t) --> (my-or #f #f #f)
               (let-values
                 ([(t) #t]
                   [(if) (λ (x y z) "oops")])
                 (my-or #f t)))))
;)

(letrec-syntaxes+values
  ([(my-or)
     (lambda (stx)
       (let-values ([(a) (car (cdr (syntax-e stx)))]
                     [(b) (car (cdr (cdr (syntax-e stx))))]
                     [(t) (datum->syntax stx 't)])
         (datum->syntax
           #'here
             `(let-values ([(,t) ,a]) (if ,t ,t ,b)))))])
  ()
  (assert-equals #t (my-or #t #f))
  (assert-equals #t (my-or #f #t))
  (assert-equals #f
    (let-values ([(t) #t])
      (my-or #f t)))
  (assert-equals #f
    (let-values
      ([(t) #t]
        [(if) (λ (x y z) "oops")])
      (my-or #f t))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
"非健康: if 动态作用域"
  '(let-values
     (((t1) #t)
       ((if1) (λ (x y z) "oops")))
     (let-values
       (((t2) #f))
       (if1 t2 t2 t1)))
;(println
  (xiao.lang.PrettyPrint/pp
    (expand '(let-values ()
               (define-syntaxes (my-or)
                 (lambda (stx)
                   (let-values ([(a) (car (cdr (syntax-e stx)))]
                                 [(b) (car (cdr (cdr (syntax-e stx))))])
                     (datum->syntax
                       #'here
                         `(let-values ([(t) ,a])
                            (,(datum->syntax stx 'if) t t ,b))))))
               (let-values
                 ([(t) #t]
                   [(if) (λ (x y z) "oops")])
                 (my-or #f t)))))
;)

(letrec-syntaxes+values
  ([(my-or)
     (lambda (stx)
       (let-values ([(a) (car (cdr (syntax-e stx)))]
                     [(b) (car (cdr (cdr (syntax-e stx))))])
         (datum->syntax
           #'here
             `(let-values ([(t) ,a])
                (,(datum->syntax stx 'if) t t ,b)))))])
  ()
  (assert-equals #t (my-or #t #f))
  (assert-equals #t (my-or #f #t))
  (assert-equals #t
    (let-values ([(t) #t])
      (my-or #f t)))
  (assert-equals "x=false, y=false, z=true, oops"
    (let-values
      ([(t) #t]
        [(if) (λ (x y z) (str "x=" x ", y=" y ", z=" z ", oops"))])
      (my-or #f t))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀
"非健康: 全部新引入的符号动态作用域"
  '(let-values
     ([(t) #t]
       [(if1) (λ (x y z) "oops")])
     (let-values
       ([(t1) #f])
       (if1 t1 t1 t1)))
;(println
  (xiao.lang.PrettyPrint/pp
    (expand '(let-values ()
               (define-syntaxes (my-or)
                 (lambda (stx)
                   (let-values ([(a) (car (cdr (syntax-e stx)))]
                                 [(b) (car (cdr (cdr (syntax-e stx))))])
                     (datum->syntax
                       stx
                         `(let-values ([(t) ,a])
                            (if t t ,b))))))
               (let-values
                 ([(t) #t]
                   [(if) (λ (x y z) "oops")])
                 (my-or #f t)))))
;)

(letrec-syntaxes+values
  ([(my-or)
     (lambda (stx)
       (let-values ([(a) (car (cdr (syntax-e stx)))]
                     [(b) (car (cdr (cdr (syntax-e stx))))])
         (datum->syntax
           stx
             `(let-values ([(t) ,a])
                (if t t ,b)))))])
  ()
  (assert-equals #t (my-or #t #f))
  (assert-equals #t (my-or #f #t))
  (assert-equals #f
    (let-values ([(t) #t])
      (my-or #f t)))
  (assert-equals "x=false, y=false, z=false, oops"
    (let-values
      ([(t) #t]
        [(if) (λ (x y z) (str "x=" x ", y=" y ", z=" z ", oops"))])
      (my-or #f t))))

;; 🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀🚀

(let ()
  (define (if x y z) (str x y z))
  (letrec-syntaxes+values
    ([(if1) (lambda (stx)
              (let-values ([(m) (match-syntax stx '(if test then else))])
                (datum->syntax
                  #'here
                    `(if ,(m 'test) ,(m 'then) ,(m 'else)))))])
    ()
    (assert-equals "123" (if1 1 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; expandbody 会无脑 letrec, define if 会被提前
;(let ()
;  (letrec-syntaxes+values
;    ([(if1) (lambda (stx)
;              (let-values ([(m) (match-syntax stx '(if test then else))])
;                (datum->syntax
;                  #'here
;                    `(if ,(m 'test) ,(m 'then) ,(m 'else)))))])
;    ()
;    (println (if1 1 2 3)))
;  (define (if x y z) (str x y z))
;  (void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"if 是内置的, let 是在 syntax 中定义的, 测试 (core-syntax ...)"
(letrec-syntaxes+values
  ([(if1) (lambda (stx)
            (let-values ([(m) (match-syntax stx '(if test then else))])
              (datum->syntax
                #'here
                  `(if ,(m 'test) ,(m 'then) ,(m 'else)))))]
    [(if2) (lambda (stx)
             (let-values ([(m) (match-syntax stx '(if test then else))])
               (datum->syntax
                 #'here
                   `(,(core-syntax 'if) ,(m 'test) ,(m 'then) ,(m 'else)))))]
    [(test1) (lambda (stx)
               (let-values ([(m) (match-syntax stx '(test a))])
                 (datum->syntax
                   #'here
                     `(let ([t ,(m 'a)]) t))))]
    [(test2) (lambda (stx)
               (let-values ([(m) (match-syntax stx '(test a))])
                 (datum->syntax
                   #'here
                     `(,(core-syntax 'let) ([t ,(m 'a)]) t))))])

  ([(if) (lambda (x y z) (str x y z))]
    [(let) 42]
    [(t) 100])

  (assert-equals "123" (if1 1 2 3))
  (assert-equals 2 (if2 1 2 3))

  (assert-equals let 42)

  (assert-equals 42 (test2 42))
  (assert-equals 1 (test2 1))

  (void))
