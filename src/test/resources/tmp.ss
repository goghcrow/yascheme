; fixme 实现成 illegal use
(println (list 1 '. 2))




(define-syntax (new1 stx)
    (let-values ([(m) (match-syntax stx '(new id:class args ...))])
        (datum->syntax #'here
                `(.
                     (. xiao.lang.Interop$CallSite
                         (constructor
                             (symbol->class ',(syntax-e (m 'id:class)))))
                     (newInstance
                         (. (list ,@(m 'args)) (toArray)))))))
(println (new1 java.lang.String "HELLO"))


;(letrec ((sc (java.util.Scanner. java.lang.System/in))
;          (a (.next sc))
;          (b (.next sc))
;          (c (.next sc))
;          (r (str a b c)))
;  (println r))
; 验证执行顺讯
;(letrec* ((sc (java.util.Scanner. java.lang.System/in))
;          (a (.next sc))
;          (b (.next sc))
;          (c (.next sc))
;          (r (str a b c)))
;  (println r))


;(println (free-identifier=?4
;           (let-values ([(x) 1]) (quote-syntax x))
;           (quote-syntax x)))

(let ()
    (namespace-set-variable-value! 'a 42)
    (current-namespace)
    (println (namespace-variable-value 'a))
    ;; todo todo !!!!!
    (assert-equals ''42 (namespace-variable-value 'a)))

(assert-equals 1 (let () (define void 1) void))


(println (expand '(let ((unquote 1)) `(,foo))))
(println (let ((unquote 1)) `(,foo)))
(println '(,foo))
(assert-equals '(,foo) (let ((unquote 1)) `(,foo)))
; (let ((unquote-splicing 1)) `(,@foo))

(assert-equals '(,@foo) (let ((unquote-splicing 1)) `(,@foo)))


(println (expand '(let ((=> 1)) (cond (#t => 'ok)))))
(let ((=> 1)) (cond (#t => 'ok)))