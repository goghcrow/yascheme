; syntax-rules 里头只要没在 matches 中, 且是 name 都会被闭包
; (lambda (id ...) ...), (let ((id expr) ...) ...) (define id expr) 等
; 中的 id 是标识符 是在宏中定义新的变量, 不能被闭包

; bug
(let ()
  (define a #f)

  ((syntax-rules ()
     ; a 被处理成文法闭包
     ((_) (lambda a 1))))

  ((syntax-rules ()
     ((_)
       ; a 被处理成文法闭包
       (let ((a 1))
         ; 这里 a 是 #f, 应该是 a
         (assert-equals a 1)))))

  ; redefine
  ;  ((syntax-rules ()
  ;     ; 这里被 a 被处理成文法闭包
  ;     ((_) (define a 1))))

  ((syntax-rules ()
     ; 这里被 a 被处理成文法闭包
     ((_) (set! a 1)))))