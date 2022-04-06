"https://github.com/racket/racket/blob/master/racket/src/expander/demo.rkt"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assert-equals 2 (+ 1 1))
(let ([f (case-lambda
           [(x) (begin (set! x 5) x)]
           [(x y) (begin0 y x)])])
  (assert-equals 5 (f 5))
  (assert-equals 42 (f 0 42)))

(assert-equals 42 ((lambda (x) (define-values (y) x) y) 42))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"racket: Expands to `let-values`:"
"这里无脑 expands to letrec-values"
(assert-equals
  #t
  (.contains
    (..
      (expand '(lambda (x)
                 (define-values (z) 1)
                 (define-values (y) z)
                 y))
      (toString)
      (replaceAll "__ϟ\\d+" ""))
    (.toString '(let-values ()
                  (lambda (x)
                    (letrec-values
                      ([(z) '1]
                        [(y) z])
                      y))))))

(assert-equals 1
  ((lambda (x)
     (define-values (z) 1)
     (define-values (y) z)
     y) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"racket: Expands to two separate `letrec-values`:"
"这里无脑 expands to letrec-values"
(let ([s (.toString '(let-values ()
                     (lambda (x)
                       (letrec-values
                         ([(z) (lambda () y)]
                           [(y) '1]
                           [(q) (lambda () q)])
                         (z)))))])

  (assert-equals
    #t
    (.contains
      (..
        (expand '(lambda (x)
                   (define-values (z) (lambda () y))
                   (define-values (y) 1)
                   (define-values (q) (lambda () q))
                   (z)))
        (toString)
        (replaceAll "__ϟ\\d+" ""))
      s))

  "Same as previous:"
  (assert-equals
    #t
    (.contains
      (..
        (expand '(lambda (x)
                   (letrec-syntaxes+values
                     ()
                     ([(z) (lambda () y)]
                       [(y) 1]
                       [(q) (lambda () q)])
                     (z))))
        (toString)
        (replaceAll "__ϟ\\d+" ""))
      s)))

(assert-equals 1
  ((lambda (x)
     (define-values (z) (lambda () y))
     (define-values (y) 1)
     (define-values (q) (lambda () q))
     (z)) 0))

(assert-equals 1
  ((lambda (x)
     (letrec-syntaxes+values
       ()
       ([(z) (lambda () y)]
         [(y) 1]
         [(q) (lambda () q)])
       (z))) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"define-syntaxes"
(let ()
  ; creates a transformer binding for each id with the value of expr
  ; (define-syntaxes (id ...) expr)
  (define-syntaxes (y) (lambda (stx) (println stx) (quote-syntax 7)))
  ; 注意 !!! 遇到 transformer 就进行 call
  ; dispatch -> isTransformer
  ; y  (lambda (stx) stx is y )
  ; (y) (lambda (stx) stx is (y) )
  (assert-equals 7 y)
  (assert-equals 7 (y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"letrec-syntaxes+values"
(assert-equals 1
  (let ([z 9])
    (letrec-syntaxes+values
      ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])
      ([(x) 5]
        [(y) (lambda (z) z)])
      (let ([z 10])
        (begin z
          (if (m 10) 1 2))))))

(assert-equals 1
  (let-values ([(z) 9])
    (letrec-syntaxes+values
      ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])
      ([(x) 5] [(y) (lambda (z) z)])
      (let-values ([(z) 10])
        (begin z (if (m 10) 1 2))))))

(assert-equals 2
  (let ([z 9])
    (letrec-syntaxes+values
      ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])
      ([(x) 5] [(y) (lambda (z) z)])
      (let ([z 10])
        (define-values (x y z) (values 1 2 3))
        (begin z
          (if (m x) y z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"健康"
"expansion not captured"
(let ([x 'x-1])
  (define-syntaxes (m)
    ; 这里 quote-syntax x, x 的 scope set 距离 'x-1
    (lambda (stx) (quote-syntax x)))
  (let ([x 'x-3])
    ; (m) 执行完 transformer 得到 #<syntax:x>, 继续 expand 之后 resolve 到 'x-1
    (assert-equals 'x-1 (m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"非健康"
"non capturing expansion"
(let ([x 'x-1])
  (define-syntaxes (m)
    (lambda (stx)
      (datum->syntax
        #f
          `(,(quote-syntax let)
             ([,(quote-syntax x) ,(quote-syntax 'x-2)])
             ; (car (cdr (syntax-e stx))) 调用 m 的第一个参数 syntax 对象
             ; 是调用参数自己的 scopeset, 距离 x-3 最近
             ,(car (cdr (syntax-e stx)))))))
  (let ([x 'x-3])
    (assert-equals 'x-3 (m x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([x 'x-1])
  (define-syntaxes (m)
    (lambda (stx)
      (datum->syntax
        #f
          `(,(quote-syntax let)
             ([,(quote-syntax x) ,(quote-syntax 'x-2)])
             ,(quote-syntax x)))))
  (let ([x 'x-3])
    (assert-equals 'x-2 (m x))))

(let ([x 'x-1])
  (define-syntaxes (m)
    (lambda (stx)
      (datum->syntax
        #'here
        `(let ([x 'x-2]) x))))
  (let ([x 'x-3])
    (assert-equals 'x-2 (m x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"distinct generated variables"
(assert-equals '(2 1)
  (letrec-syntaxes+values
    ([(gen) (lambda (stx)
              (letrec-values ([(m) (match-syntax stx '(gen vals binds refs))]
                               [(vals) (syntax-e (m 'vals))]
                               [(binds) (syntax-e (m 'binds))]
                               [(refs) (syntax-e (m 'refs))]
                               ;; 📢 这里不能绑定为 bind 和 gen 会遮盖外层定义, expand 时导致 resolve 问题
                               [(bind1 gen1) (values (quote-syntax bind) (quote-syntax gen))]
                               [(x) (quote-syntax x)])
                ; (println (.-scopes x))
                (println (datum->syntax #f (if (null? vals)
                                               `(,bind1 ,binds ,refs)
                                               `(,gen1
                                                  ,(cdr vals)
                                                  (((,x) ,(car vals)) ,@binds)
                                                  (,x ,@refs)))))

                ; 📢 每次 x 都会 gensym 成不同的 localBinding key
                (datum->syntax
                  #f
                  (if (null? vals)
                      `(,(quote-syntax bind) ,binds ,refs)
                      `(,(quote-syntax gen)
                         ,(cdr vals)
                         (((,(quote-syntax x)) ,(car vals)) ,@binds)
                         (,(quote-syntax x) ,@refs))))))]
      [(bind) (lambda (stx)
                (letrec-values ([(m) (match-syntax stx '(bind binds refs))]
                                 [(binds) (m 'binds)]
                                 [(refs) (syntax-e (m 'refs))]
                                 ;; 📢 这里不能绑定为 let-values 会遮盖外层定义, expand 时导致 resolve 问题
                                 [(let-values1 list1) (values (quote-syntax let-values) (quote-syntax list))])

                  (println (datum->syntax #f `(,let-values1 ,binds (,list1 ,@refs))))
;                  (println (.-scopes let-values1))
;                  (println (.-scopes (quote-syntax let-values)))

                  (datum->syntax
                    #'here
                      `(,let-values1 ,binds (,list1 ,@refs)))))])
    ()
    (gen (1 2) () ())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"todo: use-site scope 的文档..."
"use-site scopes (so not ambiguous)"
(assert-equals 'ok
  ((let-values ()
     (define-syntaxes (identity)
       (lambda (stx)
         (let-values ([(misc-id) (car (cdr (syntax-e stx)))])
           (datum->syntax
             #'here
               `(lambda (x)
                  (let-values ([(,misc-id) 'other]) x))))))
     (identity x))
      'ok))

(println (expand '((let-values ()
                     (define-syntaxes (identity)
                       (lambda (stx)
                         (let-values ([(misc-id) (car (cdr (syntax-e stx)))])
                           (datum->syntax
                             #'here
                               `(lambda (x)
                                  (let-values ([(,misc-id) 'other]) x))))))
                     (identity x))
                      'ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"todo: use-site scope 的文档..."
"use-site scope remove from binding position"
(assert-equals 'still-ok
  (let-values ()
    (define-syntaxes (define-identity)
      (lambda (stx)
        (let-values ([(id) (car (cdr (syntax-e stx)))])
          (datum->syntax
            #'here
              `(define-values (,id) (lambda (x) x))))))
    (define-identity f)
    (f 'still-ok)))


; "compile-time scopes pruned by `quote-syntax`"
;(namespace-require '(for-meta 2 '#%kernel) demo-ns)
;(eval-expression
; #:check 'bound
; '(letrec-syntaxes+values
;   ([(m)
;     (lambda (stx)
;       (let-values ([(id1) (let-values ([(x) 1])
;                             (define-syntaxes (wrap) ; to provoke a use-site scope
;                               (lambda (stx) (car (cdr (syntax-e stx)))))
;                             (wrap (quote-syntax x)))]
;                    [(id2) (let-values ([(x) 1])
;                             (define-syntaxes (wrap)
;                               (lambda (stx) (car (cdr (syntax-e stx)))))
;                             (wrap (quote-syntax x)))])
;         (datum->syntax
;          #'here
;          (list 'let-values (list (list (list id1) ''bound))
;                id2))))])
;   ()
;   (m)))
;
;"`(quote-syntax .... #:local)` doesn't prune"
;(eval-expression
; #:check 'bound-2
; '(letrec-syntaxes+values
;   ([(m)
;     (lambda (stx)
;       (let-values ([(id1) (let-values ([(x) 1])
;                             (quote-syntax x #:local))]
;                    [(id2) (let-values ([(x) 1])
;                             (define-syntaxes (wrap)
;                               (lambda (stx) (car (cdr (syntax-e stx)))))
;                             (quote-syntax x #:local))])
;         (datum->syntax
;          #'here
;          (list 'let-values (list (list (list id1) ''bound-1)
;                                  (list (list id2) ''bound-2))
;                id2))))])
;   ()
;   (m)))