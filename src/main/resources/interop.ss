;; ------------------------------------------------
;; 注意这里 new 不能改名, (class. ) 语法会被改写成 (new class)
;; (new class args ...)
;; (new (interfaces ...+) map-string-to-lambda-expr)
;; ------------------------------------------------
;(define-syntax-rule (new class args ...)
;  (let (($klass (java.lang.Class/forName (symbol->string 'class))))
;    (if (.isInterface $klass)
;      (let (($methods (car (list args ...))))
;        (xiao.lang2.JavaInterop$Experimental/proxy
;          $klass
;          $methods
;          (current-environment)))
;      (.newInstance
;        (xiao.lang2.JavaInterop$CallSite/constructor $klass)
;        (.toArray (list args ...))))))

;; ------------------------------------------------
;; (doto instance-expr
;;   (instanceMethodName-symbol args ...) ...)
;; (doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
;; ------------------------------------------------
(define-syntax-rule (doto instance-expr
                      (instance-method args ...) ...)
  (let ([$instance instance-expr])
    (instance-method $instance args ...) ...
    $instance))

;; ------------------------------------------------
;; (.. instance-expr member+)
;; (.. Classname-symbol member+)
;; ------------------------------------------------
(define-syntax ..
  (syntax-rules ()
    ((.. ins-or-name member)
      (. ins-or-name member))
    ((.. ins-or-name member member-rest ...)
      (.. (. ins-or-name member) member-rest ...))))

;; ------------------------------------------------
;; (-> x forms+)
;; Clojure: convert nested function calls into a linear flow of function calls
;; improving readability.
;; ------------------------------------------------
(define-syntax ->
  (syntax-rules ()
    ((-> x (fst sec ...))
      (fst x sec ...))
    ((-> x (fst sec ...) (fst-rest sec-rest ...) ...)
      (-> (fst x sec ...) (fst-rest sec-rest ...) ...))))

;; ------------------------------------------------
;; (->> x forms+)
;; ------------------------------------------------
(define-syntax ->>
  (syntax-rules ()
    ((->> x (fst sec ...))
      (fst sec ... x))
    ((->> x (fst sec ...) (fst-rest sec-rest ...) ...)
      (->> (fst sec ... x) (fst-rest sec-rest ...) ...))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------
;; (class obj)
;; ------------------------------------------------
(define (class x) (.getClass x))


;; ------------------------------------------------
;; (instance? class instance)
;; ------------------------------------------------
(define-syntax-rule (instance? class instance)
  (.isInstance
    (java.lang.Class/forName (symbol->string 'class))
    instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ------------------------------------------------
;; (arraylist a ...)
;; ------------------------------------------------
(define-syntax-rule (arraylist el ...)
  (doto
    (java.util.ArrayList.)
    (.add el) ...))

;; ------------------------------------------------
;; (hashmap (k v) ...)
;; ------------------------------------------------
(define-syntax-rule (hashmap (k v) ...)
  (doto
    (java.util.HashMap.)
    (.put k v) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------
;; (set! (. instance-expr instance-field-name) expr)
;; (set! (. className static-field-name) expr)
;; (set! id expr)
;; ------------------------------------------------
;(define primitive-set! set!)
;(define-syntax set!
;  (syntax-rules (.)
;    ((set! (. class-or-instance field) expr)
;      (begin
;        ; xiao.lang2.Syntaxes$MemberAccessorExpansion/isJavaClassName
;        (if (and
;              (symbol? 'class-or-instance)
;              (.contains (symbol->string 'class-or-instance) "."))
;          ; 静态属性赋值
;          (.field
;            (xiao.lang2.JavaInterop$CallSite/field
;              (java.lang.Class/forName (symbol->string 'class-or-instance))
;              (symbol->string 'field))
;            NULL
;            expr)
;          ; 实例属性赋值
;          (let ([instance class-or-instance])
;            (.field
;              (xiao.lang2.JavaInterop$CallSite/field
;                (class instance)
;                (symbol->string 'field))
;              instance
;              expr)))
;        (void)))
;;    ((set! id expr)
;;      (let* ( [$env (current-environment)]
;;              [$str-id (symbol->string 'id)]
;;              [$defined-scope (.findDefinedScope $env $str-id)])
;;        (if (equals? NULL $defined-scope)
;;          (raise (str $str-id " 未定义, 在: " '(set! id expr)))
;;          (.put $defined-scope $str-id expr))
;;        (void)))
;    ((set! id expr)
;      (primitive-set! id expr))))