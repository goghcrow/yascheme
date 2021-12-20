;; ------------------------------------------------
;; (do ([id init-expr step-expr-maybe] ...)
;;  (stop?-expr finish-expr ...)
;;  expr ...)
;; step-expr-maybe	 	=
;;                      | step-expr
;; ------------------------------------------------
; 只能写 0 或者 1 个 step
; 没写 step-expr 默认是 id
(define-syntax-rule (do ((id init step ...) ...)
                      (stop? finish ...)
                      expr ...)
  (let $loop ((id init) ...)
    (if stop?
      (begin finish ...)
      (begin
        expr ...
        ; 处理 0 或者 1 个 step
        ; 这么写有个问题, 如果传了多个 step, 会默认使用最后一个 step
        ; but 下面的写法会引入奇怪的 step
        ($loop (begin id step ...) ...)))))


;; ------------------------------------------------
;; (cond cond-clause ...)
;;        cond-clause	 	=
;;          [test-expr then-body ...+]
;;      |	 	[else then-body ...+]
;;      |	 	[test-expr => proc-expr]
;;      |	 	[test-expr]
;; ------------------------------------------------
(define-syntax cond
  (syntax-rules (else =>)
    ((cond) (void))  ; 无任何匹配返回啥?? void ? false
    ((cond [else then then-rest ...]) ; else body 至少一个 form
      (begin then then-rest ...))
    ((cond [test => proc] clause ...)
      (let ([$test test])
        (if $test
          (proc $test)
          (cond clause ...))))
    ((cond [test then then-rest ...] clause ...) ; then body 至少一个 form
      (if test
        (begin then then-rest ...)
        (cond clause ...)))
    ((cond [test] clause ...) ; == 0
      test)))

;; ------------------------------------------------
;; (case val-expr case-clause ...)
;;    case-clause	 	=	 	[(datum ...) then-body ...+]
;;                  |	 	[else then-body ...+]
;; ------------------------------------------------
(define-syntax case
  (syntax-rules (else)
    ((case val)
      (begin
        val
        (void))) ; void ? false
    ((case val [else then then-rest ...])
      (begin
        val
        then then-rest ...))
    ((case val
       [(datum ...) then then-rest ...]
       clause ...)
      (let ([$val val])
        ; 注意这里 quote 了一次
        (if (member $val '(datum ...))
          (begin then then-rest ...)
          (case $val clause ...))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (delay expr)
  (let (($forced #f)
         ($memo #f))
    (lambda ()
      (if $forced
        $memo
        (begin
          (set! $memo expr)
          (set! $forced #t)
          $memo)))))

(define (force promise) (promise))