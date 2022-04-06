(define-syntax (assert stx)
  (letrec-values ([(m) (match-syntax stx '(assert test))]
                   [(test) (m 'test)])
    (datum->syntax
      #'here
        `(if ,test
           (void)
           (raise (str "assert fail: " ',test))))))

(define-syntax (assert-equals stx)
  (letrec-values ([(m) (match-syntax stx '(assert-equals a b))]
                   [(a) (m 'a)]
                   [(b) (m 'b)])
    (datum->syntax
      #'here
        `(assert (equal? ,a ,b)))))
