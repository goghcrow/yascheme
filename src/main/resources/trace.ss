; trace 语义不是幂等的, 必须 untrace 之后才能再次 trace
(define-syntaxes (trace)
  (lambda (stx)
    (letrec-values ([(f) (car (cdr (syntax-e stx)))]
                     [(f-name) (symbol->string (syntax-e f))]
                     [(f-cnt) (string->symbol (str "$cnt_" f-name))]
                     ;; 非健康
                     ;; !!! 注意这里, 用 stx 的 scope 是要把 ori-f 声明到调用 trace 的作用域
                     ;; 这样才能在同样的作用域 untrace  set!
                     [(ori-f) (datum->syntax stx (string->symbol (str "$ori_" f-name)))]
                     ;; 不管有没有 untrace, 如果是第二次 trace
                     ;; $ori_xxx 应该已经存在, 所以是 set! 不是 define
                     [(define-or-set!) (if
                                         (equal? 'lexical (identifier-binding ori-f))
                                           'set!
                                           'define)])
      (datum->syntax
        #'here
        (if (identifier? f)
            `(begin
               (,define-or-set! ,ori-f ,f)
               (define ,f-cnt 0)
               (set! ,f (lambda args
                          (display "> ")
                          (display (string-repeat " · " ,f-cnt))
                          (display "> ")
                          (display "(")
                          (display ,f-name)
                          (display " ")

                          ; (for-each display args)
                          (display
                            (let ([s (.toString args)])
                              (.substring s 1 (sub1 (.length s)))))

                          (display ")")
                          (newline)

                          (set! ,f-cnt (add1 ,f-cnt))
                          (define r (apply ,ori-f args))

                          (display "< ")
                          (display (string-repeat " · " (sub1 ,f-cnt)))
                          (display "< ")
                          (display r)
                          (newline)

                          (set! ,f-cnt (sub1 ,f-cnt))
                          r)))
          (raise "期望 id"))))))

; untrace 语义是幂等的
(define-syntaxes (untrace)
  (lambda (stx)
    (letrec-values ([(f) (car (cdr (syntax-e stx)))]
                     [(f-name) (symbol->string (syntax-e f))]
                     [(ori-f) (datum->syntax stx (string->symbol (str "$ori_" f-name)))])
      (if (equal? 'lexical (identifier-binding ori-f))
        (datum->syntax
          #'here
          (if (identifier? f)
              `(set! ,f ,ori-f)
            (raise "期望 id")))
        (raise "没有被trace")))))