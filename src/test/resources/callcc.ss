;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; callcc


;; ((call/cc call/cc) (call/cc call/cc))

"https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/"
"https://zhuanlan.zhihu.com/p/180473795"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let + call/cc
"https://www.zhihu.com/question/297207095/answer/509225101"
(assert-equals true
  (let ((x (call/cc list)))
    (if (pair? x)
      ((car x) (lambda () x))
      (pair? (x)))))

; define + set!
(assert-equals false
  (letrec ((x (call/cc list)))
    (if (pair? x)
      ((car x) (lambda () x))
      (pair? (x)))
  ))



(assert-equals "#<continuation>"
  (.toString (call/cc call/cc)))


; (let ([x (call/cc (lambda(k) k))]) (x f)) 等价于 (f f)
(assert-equals "hi" (let ([x (call/cc (lambda (k) k))])
                      (x (lambda (ignore) "hi"))))

(let ([f (lambda (return) (return 2) 3)]
       [id (lambda (x) x)])
  (assert-equals 3 (f id))
  (assert-equals 2 (call-with-current-continuation f)))


(let ()
  (define a '())
  (define (append it) (set! a `(,@a ,it)))
  (append
    (call-with-current-continuation
      (λ (return)
        (append 1)
        (return 2)
        (append 3))))
  (assert-equals '(1 2) a)

  (set! a '())
  (call-with-current-continuation
    (λ (return)
      (append 1)
      (append 2)
      (append 3)))
  (assert-equals `(1 2 3) a))

; wiki 的例子
(let ()
  ; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
  (define (generate-one-element-at-a-time lst)
    ;; Both internal functions are closures over lst

    ;; Internal variable/Function which passes the current element in a list
    ;; to its return argument (which is a continuation), or passes an end-of-list marker
    ;; if no more elements are left. On each step the function name is
    ;; rebound to a continuation which points back into the function body,
    ;; while return is rebound to whatever continuation the caller specifies.
    (define (control-state return)
      (for-each
        (lambda (element)
          (set! return (call-with-current-continuation
                         (lambda (resume-here)
                           ;; Grab the current continuation
                           (set! control-state resume-here)
                           (return element))))) ;; (return element) evaluates to next return
        lst)
      (return 'you-fell-off-the-end))

    ;; (-> X u 'you-fell-off-the-end)
    ;; This is the actual generator, producing one item from a-list at a time.
    (define (generator)
      (call-with-current-continuation control-state))

    ;; Return the generator
    generator)

  (define generate-digit
    (generate-one-element-at-a-time (quote (0 1 2))))

  (assert-equals 0 (generate-digit))
  (assert-equals 1 (generate-digit))
  (assert-equals 2 (generate-digit))
  (assert-equals 'you-fell-off-the-end (generate-digit)))