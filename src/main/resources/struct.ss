(define-syntax (struct stx)
  (letrec-values ([(m) (match-syntax stx '(struct id (id:field ...)))]
                   [(id) (m 'id)]
                   [(fields) (m 'id:field)]
                   [(id-str) (symbol->string (syntax-e id))]
                   [(pred-id) (datum->syntax id (string->symbol (str id-str "?")))]
                   [(ix) 0])
    (datum->syntax #'here
        `(begin
           ;; 定义 ctor
           (define (,id ,@fields)
             (apply vector (cons ',id (list ,@fields))))
           ;; 定义 predicate
           (define (,pred-id v)
             (and (vector? v)
               (eq? (vector-ref v 0) ',id)))
           ;; 定义字段 accessor
           ,@(map
               (lambda (field)
                 (letrec-values ([(field-str)
                                   (symbol->string
                                     (syntax-e field))]
                                  [(acc-id)
                                    (datum->syntax id
                                      (string->symbol
                                        (str id-str "-" field-str)))])
                   (set! ix (add1 ix))
                     `(define (,acc-id v)
                        (unless (,pred-id v)
                          (raise (str v " is not a " ,id-str " struct")))
                        (vector-ref v ,ix))))
               fields)))))