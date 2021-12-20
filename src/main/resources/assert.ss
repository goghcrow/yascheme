(define-syntax-rule (assert condtional-form)
  (if condtional-form (void) (raise (str "assert fail: " (quote condtional-form)))))

(define-syntax-rule (assert-equals a b)
  ; 这里要用 equals 不能用.equals 否则 NPE
  (assert (equals? a b)))


; todo
;(error message-sym) → any
;message-sym : symbol?
;(error message-str v ...) → any
;message-str : string?
;v : any/c
;(error who-sym format-str v ...) → any
;who-sym : symbol?
;format-str : string?
;v : any/c
; 这里实现有问题, format 有问题
;(define-syntax error
;  (syntax-rules ()
;    ((error msg-sym)
;      (raise (str "error: " (symbol->string msg-sym))))
;    ;((error msg-str v ...) ())
;    ((error who-sym fmt-str v ...)
;      ; 可以把 v... 变成一个数组
;      (raise (str (symbol->string who-sym) ": " (java.lang.String/format fmt-str v ...))))))