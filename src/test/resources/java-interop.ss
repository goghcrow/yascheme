; java interop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-equals "Byte" (.getSimpleName (class 42b)))
(assert-equals "Short" (.getSimpleName (class 42s)))
(assert-equals "Integer" (.getSimpleName (class 42)))
(assert-equals "Long" (.getSimpleName (class 42L)))
(assert-equals "Double" (.getSimpleName (class 3.14)))
(assert-equals "Float" (.getSimpleName (class 3.14f)))
(assert-equals "Double" (.getSimpleName (class 3.14d)))
(assert-equals "BigInteger" (.getSimpleName (class 42n)))
(assert-equals "BigDecimal" (.getSimpleName (class 3.13m)))


(assert-equals "Byte" (.getSimpleName (class +42b)))
(assert-equals "Short" (.getSimpleName (class +42s)))
(assert-equals "Integer" (.getSimpleName (class +42)))
(assert-equals "Long" (.getSimpleName (class +42L)))
(assert-equals "Double" (.getSimpleName (class +3.14)))
(assert-equals "Float" (.getSimpleName (class +3.14f)))
(assert-equals "Double" (.getSimpleName (class +3.14d)))
(assert-equals "BigInteger" (.getSimpleName (class +42n)))
(assert-equals "BigDecimal" (.getSimpleName (class +3.13m)))

(assert-equals "Byte" (.getSimpleName (class -42b)))
(assert-equals "Short" (.getSimpleName (class -42s)))
(assert-equals "Integer" (.getSimpleName (class -42)))
(assert-equals "Long" (.getSimpleName (class -42L)))
(assert-equals "Double" (.getSimpleName (class -3.14)))
(assert-equals "Float" (.getSimpleName (class -3.14f)))
(assert-equals "Double" (.getSimpleName (class -3.14d)))
(assert-equals "BigInteger" (.getSimpleName (class -42n)))
(assert-equals "BigDecimal" (.getSimpleName (class -3.13m)))

(assert (symbol? (string->symbol "Hello")))
(assert (string? (symbol->string 'Hello)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (array-equals? a1 a2)
  (. java.util.Arrays equals a1 a2))

; 实例属性访问
(let ([hello-chars (. "Hello" toCharArray)])
  (assert (array-equals? hello-chars (. "Hello" value)))
  (assert (array-equals? hello-chars (. "Hello" -value))))

; 静态属性访问
(assert-equals (void) (. java.lang.Void TYPE))

; 调用实例方法
(assert-equals 5 (. "Hello" length))
(assert-equals 5 (. "Hello" (length)))

; 调用实例方法
(assert-equals "el" (. "Hello" substring 1 3))
(assert-equals "el" (. "Hello" (substring 1 3)))

; 调用静态方法
(assert-equals 42 (. java.lang.Integer parseInt "42"))
(assert-equals 42 (. java.lang.Integer (parseInt "42")))

;; 实例属性赋值
(let ([a "Hello"])
  (set! (. a value) (. "World" value))
  (assert-equals "World" a))

;; 静态属性赋值
(set! (. java.lang.Void TYPE) (void))

; new
(assert-equals 42 (new java.lang.Integer 42))
(assert-equals "s" (new java.lang.String "s"))

; instance?
(assert (instance? java.lang.String "s"))
(assert-equals #f (instance? java.lang.Integer "s"))
(assert-equals #f (instance? java.lang.String 42))
(assert (instance? java.lang.Integer 42))

; get class
(assert-equals "java.lang.Integer" (.getName (class 1)))
(assert-equals "java.lang.String" (.getName (class "s")))
(assert-equals "java.lang.Boolean" (.getName (class #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; -> ->>
(assert-equals "ABC"
  (-> (str "A") (str "B") (str "C")))

(assert-equals "CBA"
  (->> (str "A") (str "B") (str "C")))

(assert-equals 5
  (-> 2 (+ 3)))
(assert-equals -2
  (-> 2 (+ 3) (- 7)))
(assert-equals 42
  (-> 4 (* 4 3) (- 6)))
(assert-equals '(1 2 3)
  (->> '(1 2 3) (map add1) (map sub1)))


(assert-equals
    (java.util.Arrays/toString
      (.split
        (.replace
          (.toUpperCase "a b c d")
          "A" "X")
        " "))
    (-> "a b c d"
      (.toUpperCase)
      (.replace "A" "X")
      (.split " ")
      (java.util.Arrays/toString)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ..
(assert-equals
  (java.lang.System/getProperty "java.vm.version")
  (.. java.lang.System (getProperties) (get "java.vm.version")))

(assert-equals
  (. (. java.lang.System (getProperties)) (get "os.name"))
  (.. java.lang.System (getProperties) (get "os.name")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; doto
(let ((m (new java.util.HashMap)))
  (.put m "a" 1) (.put m "b" 2)
  (assert-equals
    m
    (doto
      (java.util.HashMap.)
      (.put "a" 1)
      (.put "b" 2))))

(assert-equals
  (let ([lst (java.util.ArrayList.)])
    (.add lst 1)
    (.add lst 2)
    (.add lst 3)
    lst)
  (doto
    (java.util.ArrayList.)
    (.add 1)
    (.add 2)
    (.add 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 简单的代理
;
(let* (
       [a #f]
       [proxy (java.lang.Runnable.
               (hashmap
                 ("run" (lambda ()
                          (set! a 1)
                          NULL))
                 ("toString" (lambda () "run-impl"))))])
  (assert-equals (.toString proxy) "run-impl")
  (assert (.startsWith (.toString (class proxy)) "class com.sun.proxy"))
  (assert (instance? java.lang.Runnable proxy))
  (assert-equals a #f)
  (.run proxy)
  (assert-equals a 1))


;; 配合 interface. 使用
;(define-syntax-rule (methods
;                      (name (args ...) body ...) ...)
;  (hashmap
;    ((symbol->string 'name)
;      (lambda (args ...) body ...)) ...))

(let* (
       [a #f]
       [proxy (java.lang.Runnable.

;                (methods
;                 (run ()
;                   (set! a 1)
;                   NULL)
;                 (toString () "run-impl"))

                (hashmap
                  ("run" (lambda () (set! a 1) NULL))
                  ("toString" (lambda () "run-impl"))
                )

              )])
  (assert-equals (.toString proxy) "run-impl")
  (assert (.startsWith (.toString (class proxy)) "class com.sun.proxy"))
  (assert (instance? java.lang.Runnable proxy))
  (assert-equals a #f)
  (.run proxy)
  (assert-equals a 1))