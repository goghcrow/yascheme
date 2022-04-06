"https://docs.huihoo.com/homepage/shredderyin/wiki/SchemeAmb.html"
"http://community.schemewiki.org/?amb"
"https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/"
"https://en.wikipedia.org/wiki/Nondeterministic_programming"

; amb-fail 是最近一个失败的分支设置的函数。如果执行没有参数的 (amb) 就会转到这个 amb-fail.
(define amb-fail #f)
(define (init-amb-fail)
  (set! amb-fail
    (lambda ()
      (error "amb tree exhausted"))))
(init-amb-fail)

(define-syntaxes (amb)
  (lambda (stx)
    (let-values ([(m) (match-syntax stx '(amb alt ...))])
      (datum->syntax
        #'here
        ; 把 amb-fail 的值保存在局部变量 prev-amb-fail 里，最近一个失败函数
        ; 当整个 amb 表达式失败时，可以通过 prev-amb-fail 通知上一个 amb 表达式改变它的值
          `(let ([prev-amb-fail amb-fail])
             ; 整个 amb 表达式的 continuation 存放在 k-success 里
             (call/cc
               (lambda (k-success)
                 ,@(map
                     (lambda (alt)
                       ; 对于每一个参数，使用了一个 call/cc 得到它的 continuation. 并且保存在 k-failure 里
                       ; 当某一个分支得到一个值，它就通过整个 amb 的 continuation(k-success) 把这个值返回出去
                       ; 这样 amb 就返回一个值
                         `(call/cc
                            (lambda (k-failure)
                              ; 每一个分支在第一次执行时，有两项工作
                              ; 第一，把当前的 amb-fail 设置为一个函数
                              ; 这个内部函数 的作用就是把 amb-fail 的值恢复到进入 amb 以前的值
                              (set! amb-fail
                                (lambda ()
                                  ; 失败会触发 amb-fail, 恢复进入 amb 之前的值
                                  (set! amb-fail prev-amb-fail)
                                  ; k-failure 返回分支的 continuation, 继续尝试下一个分支
                                  (k-failure 'fail)))
                              ; 第二，立即通过 amb 表达式的 continuation(k-success) 返回自己的分支 的值。从而引起 amb 表达式中途返回。
                              ; 注意，每一个分支执行时都会引起 amb 立即返回。后面的分支都还没有执行！
                              (k-success ,alt))))
                     (m 'alt))
                 ; 没有参数的 amb 被定义为是一个失败
                 (prev-amb-fail))))))))

;;; As an auxiliary example, AMB-POSSIBILITY-LIST is a special form
;;; that returns a list of all values its input expression may return.

(define-syntaxes (amb-possibility-list)
  (lambda (stx)
    (let-values ([(expr) (car (cdr (syntax-e stx)))])
      (datum->syntax
        #'here
      `(let ([val-lst '()])
         ;; This requires that AMB try its sub-forms left-to-right.
         (amb (let ([val ,expr])
                (set! val-lst (cons val val-lst))
                (amb-fail))
           (reverse val-lst)))))))


(define (append x y)
  (if (null? x)
    y
    (cons (first x) (append (rest x) y))))

(define (reverse l)
  (if (null? l)
    null
    (append (reverse (rest l)) (cons (first l) null))))







;(define-syntax amb-possibility-list
;  (syntax-rules ()
;    ((AMB-POSSIBILITY-LIST expression)
;      (LET ((VALUE-LIST '()))
;        ;; This requires that AMB try its sub-forms left-to-right.
;        (AMB (let ((VALUE expression))
;               (SET! VALUE-LIST (CONS VALUE VALUE-LIST))
;               (FAIL))
;          (REVERSE VALUE-LIST))))))   ; Order it nicely.

(define (amb-number-between lo hi)
  (let loop ([i lo])
    (if (> i hi)
      (amb)
      (amb i (loop (add1 i))))))

(define (amb-require pred)
  (if pred (void) (amb)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; http://community.schemewiki.org/?amb

;;; FAIL is called to backtrack when a condition fails.
;;; At the top level, however, there is no more to backtrack, so we signal an error.
;(define (fail)
;  (error "Amb tree exhausted"))
;
;(define (require condition)
;  (if condition (void) (fail)))
;
;(define-syntaxes (amb)
;  (lambda (stx)
;    (datum->syntax
;      #'here
;      ; Two shortcuts. (amb) (amb expr)
;      (let-values ([(m) (try-match-syntax stx '(amb))])
;        (if m `(fail)
;          (let-values ([(m) (try-match-syntax stx '(amb expr))])
;            (if m (m 'expr)
;              (let-values ([(m) (match-syntax stx '(amb expr ...))])
;                  `(let ([fail-save fail])
;                     ((call/cc
;                        ;; Capture a continuation to which we return possibles.
;                        (lambda (k-success)
;                          ,@(map (lambda (expr)
;                                     `(call/cc
;                                        ;; K-FAILURE will try the next possible expression.
;                                        (lambda (k-failure)
;                                          (set! fail (lambda () (k-failure #f)))
;                                          ;; Note that the expression is evaluated in tail position with respect to AMB.
;                                          (k-success (lambda () ,expr)))))
;                              (m 'expr))
;                          ;; Finally, if this is reached, we restore the saved FAIL.
;                          (set! fail fail-save)
;                          (fail-save)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(println (amb-possibility-list (list (amb 1 2) (amb 3 4))))
(println (amb-possibility-list (+ (amb 1 2) (amb 3 4))))

(let ([a (amb 1 2 3 4 5 6 7)]
       [b (amb 1 2 3 4 5 6 7)]
       [c (amb 1 2 3 4 5 6 7)])
  (amb-require (= (* c c) (+ (* a a) (* b b)))) ; 勾股定理: 直角三角形
  (amb-require (< b a)) ; 并且第二个边比第一个边短, 这里是为了去重

  (println (list a b c)))

(let ((a (amb 1 2))
       (b (amb 1 2)))
  (amb-require (< a b))
  (list a b))

(assert-equals 1
  (if (amb #f #t)
    1
    (amb)))