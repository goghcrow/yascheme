(define-syntax (test-begin stx)
  (let-values ([(m) (match-syntax stx '(test-begin name expr ...))])
    (datum->syntax
      stx ; for *tests-run*, *tests-passed*
        `(let ([*tests-run* 0]
                [*tests-passed* 0])
           (begin
             (displayln "================================================================================")
             (displayln (str "「" ,(m 'name) "」"))
             (displayln "--------------------------------------------------------------------------------")
             ,@(m 'expr)
             (display *tests-passed*)
             (display " out of ")
             (display *tests-run*)
             (unless (= *tests-run* 0)
               (display " passed (")
               (display (* (/ *tests-passed* *tests-run*) 100))
               (display "%)"))
             (newline))))))

; test-equal?
(define-syntax (check-equal? stx)
  (letrec-values ([(m) (match-syntax stx '(test expect expr))]
                   [(expect) (m 'expect)]
                   [(expr) (m 'expr)]
                   ; 非健康
                   [(*tests-passed*) (datum->syntax stx '*tests-passed*)]
                   [(*tests-run*) (datum->syntax stx '*tests-run*)])
    (datum->syntax
      #'here
        `(begin
           (set! ,*tests-run* (+ ,*tests-run* 1))
           (let ([s (str ,*tests-run* ". " ',expr)]
                  [res ,expr])
             (display s)
             (display " ")
             (display (string-repeat "." (max 0 (- 72 (.length s)))))
             (cond
               ((equal? res ,expect)
                 (set! ,*tests-passed* (+ ,*tests-passed* 1))
                 (displayln " [PASS]"))
               (else
                 (displayln " [FAIL]")
                 (display "    expected ") (display ,expect)
                 (display " but got ") (displayln res)
                 (raise "CHECK FAIL"))))))))
