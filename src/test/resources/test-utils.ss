(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax (test stx)
  (letrec-values ([(m) (match-syntax stx '(test expect expr))]
                [(expect) (m 'expect)]
                [(expr) (m 'expr)])
    (datum->syntax
      #'here
      `(begin
         (set! *tests-run* (+ *tests-run* 1))
         (let ([s (str *tests-run* ". " ',expr)]
               [res ,expr])
           (display s)
           (display " ")
           (display (string-repeat "." (max 0 (- 72 (.length s)))))
           (cond
             ((equal? res ,expect)
               (set! *tests-passed* (+ *tests-passed* 1))
               (displayln " [PASS]"))
             (else
               (displayln " [FAIL]")
               (display "    expected ") (display ,expect)
               (display " but got ") (displayln res))))))))


(define-syntax (test-assert stx)
  (letrec-values ([(m) (match-syntax stx '(test-assert expr))])
   (datum->syntax
      #'here
      `(test #t ,(m 'expr)))))

(define (test-begin . name)
  (set! *tests-run* 0)
  (set! *tests-passed* 0)
  #f)

(define (test-end)
  (display *tests-passed*)
  (display " out of ")
  (display *tests-run*)
  (display " passed (")
  (display (* (/ *tests-passed* *tests-run*) 100))
  (display "%)")
  (newline))