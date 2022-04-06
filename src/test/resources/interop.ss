;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "demo"
  (check-equal? 5 (.length "Hello"))
  (check-equal? "He" (.substring "Hello" 0 2))
  (check-equal? "llo" (.substring "Hello" 2 5))

  (let ([lst (java.util.ArrayList.)] [sum 0])
    (define (for f iter)
      (if (.hasNext iter)
        (begin
          (f (.next iter))
          (for f iter))
        (void)))
    (.add lst 1)
    (.add lst 2)
    (.add lst 3)
    ; (for println (.iterator lst))
    (for (λ (it) (set! sum (+ sum it))) (.iterator lst))
    (check-equal? 6 sum))

  (call-with-output-string
    (lambda ()
      (println (let ([rnd (java.util.Random.)])
                 (. rnd (nextInt 10))))
      (println java.lang.Math/PI))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "实例属性访问"
  (let ([hello-chars (. "Hello" toCharArray)])
    (check-equal? #t (equal? hello-chars (. "Hello" value)))
    (check-equal? #t (equal? hello-chars (. "Hello" -value)))
    (check-equal? #t (equal? hello-chars (.value "Hello")))
    (check-equal? #t (equal? hello-chars (.-value "Hello")))))

(test-begin "静态属性访问"
  (check-equal? (void) (. java.lang.Void TYPE))
  (check-equal? (void) java.lang.Void/TYPE))

(test-begin "调用实例方法"
  (check-equal? 5 (. "Hello" length))
  (check-equal? 5 (. "Hello" (length)))
  (check-equal? 5 (.length "Hello")))

(test-begin "调用实例方法"
  (check-equal? "el" (. "Hello" substring 1 3))
  (check-equal? "el" (. "Hello" (substring 1 3)))
  (check-equal? "el" (.substring "Hello" 1 3)))

(test-begin "调用静态方法"
  (check-equal? 42 (. java.lang.Integer parseInt "42"))
  (check-equal? 42 (. java.lang.Integer (parseInt "42")))
  (check-equal? 42 (java.lang.Integer/parseInt "42"))
  (check-equal? 42 (.parseInt java.lang.Integer "42")))

(test-begin "实例属性赋值"
  (let ([a "Hello"])
    (set! (. a value) (. "World" value))
    (check-equal? "World" a)))

(test-begin "静态属性赋值"
  (set! (. java.lang.Void TYPE) (void)))

(test-begin "new instance?"
  (check-equal? 42 (new java.lang.Integer 42))
  (check-equal? "s" (new java.lang.String "s"))

  (check-equal? #t (instance? java.lang.String "s"))
  (check-equal? #f (instance? java.lang.Integer "s"))
  (check-equal? #f (instance? java.lang.String 42))
  (check-equal? #t (instance? java.lang.Integer 42)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "typed-array 调用不定参数方法"
  (check-equal? "A 42" (java.lang.String/format "%s %d" (array  "A" 42)))
  ;; static int[] varArgsTest(int ...a) { return a; }
  (check-equal? (typed-array 'int 1 2 3) (xiao.lang.Test/varArgsTest (typed-array 'int 1 2 3)))
  ;; static long[] varArgsTest(long ...a) { return a; }
  (check-equal? (typed-array 'long 1 2 3) (xiao.lang.Test/varArgsTest (typed-array 'long 1l 2l 3l)))
  ;; static int[] varArgsTest(String s, int ...a) { return a; }
  (check-equal? (typed-array 'int 1 2 3) (xiao.lang.Test/varArgsTest "HELLO" (typed-array 'int 1 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "class"
  (check-equal? "java.lang.Integer" (.getName (class 1)))
  (check-equal? "java.lang.String" (.getName (class "s")))
  (check-equal? "java.lang.Boolean" (.getName (class #t)))

  (check-equal? NULL (class NULL))

  (check-equal? "Byte" (.getSimpleName (class 42b)))
  (check-equal? "Short" (.getSimpleName (class 42s)))
  (check-equal? "Integer" (.getSimpleName (class 42)))
  (check-equal? "Long" (.getSimpleName (class 42L)))
  (check-equal? "Double" (.getSimpleName (class 3.14)))
  (check-equal? "Float" (.getSimpleName (class 3.14f)))
  (check-equal? "Double" (.getSimpleName (class 3.14d)))
  (check-equal? "BigInteger" (.getSimpleName (class 42n)))
  (check-equal? "BigDecimal" (.getSimpleName (class 3.13m)))

  (check-equal? "Byte" (.getSimpleName (class +42b)))
  (check-equal? "Short" (.getSimpleName (class +42s)))
  (check-equal? "Integer" (.getSimpleName (class +42)))
  (check-equal? "Long" (.getSimpleName (class +42L)))
  (check-equal? "Double" (.getSimpleName (class +3.14)))
  (check-equal? "Float" (.getSimpleName (class +3.14f)))
  (check-equal? "Double" (.getSimpleName (class +3.14d)))
  (check-equal? "BigInteger" (.getSimpleName (class +42n)))
  (check-equal? "BigDecimal" (.getSimpleName (class +3.13m)))

  (check-equal? "Byte" (.getSimpleName (class -42b)))
  (check-equal? "Short" (.getSimpleName (class -42s)))
  (check-equal? "Integer" (.getSimpleName (class -42)))
  (check-equal? "Long" (.getSimpleName (class -42L)))
  (check-equal? "Double" (.getSimpleName (class -3.14)))
  (check-equal? "Float" (.getSimpleName (class -3.14f)))
  (check-equal? "Double" (.getSimpleName (class -3.14d)))
  (check-equal? "BigInteger" (.getSimpleName (class -42n)))
  (check-equal? "BigDecimal" (.getSimpleName (class -3.13m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "string->class"
  (check-equal? #t (.isPrimitive (string->class "boolean")))
  (check-equal? #t (.isPrimitive (string->class "byte")))
  (check-equal? #t (.isPrimitive (string->class "short")))
  (check-equal? #t (.isPrimitive (string->class "int")))
  (check-equal? #t (.isPrimitive (string->class "long")))
  (check-equal? #t (.isPrimitive (string->class "float")))
  (check-equal? #t (.isPrimitive (string->class "double")))
  (check-equal? #t (.isPrimitive (string->class "char")))
  (check-equal? #t (.isPrimitive (string->class "void")))
  (check-equal? #t (.isArray (string->class "[Ljava.lang.Integer;")))
  (check-equal?
    (string->class "java.lang.System")
    (symbol->class 'java.lang.System)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "coerce"
  (check-equal? (.charAt "A" 0) (char 65))
  (check-equal? (byte "1") (byte 1))
  (check-equal? (byte "1") (byte 1.1))
  (check-equal? (short "42") (short 42))
  (check-equal? (short 3) (short 3.14))
  (check-equal? (.longValue (int "42")) (long 42))
  (check-equal? (.intValue (bigint 42)) (.intValue (bigdec 42)))
  (check-equal? #t (< (- (double 3.14) (.doubleValue (float "3.14"))) 0.01)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "array"
  (check-equal?
    (string->class "[I")
    (.getClass (typed-array 'int 1 2 3)))

  (check-equal?
    (string->class "[Ljava.lang.Object;")
    (.getClass (array 1 2 3)))

  (check-equal?
    (typed-array 'int 1 2 3)
    (typed-array 'int 1 2L 3.14))

  (let ((empty-obj-arr (array))
         (obj-arr (array 42 "HELLO"))
         (typed-empty-int-arr (typed-array 'java.lang.Integer))
         (str-arr0 (typed-array "java.lang.String" "a" "b" "c"))
         (str-arr (typed-array 'java.lang.String "a" "b" "c"))
         (int-arr (typed-array 'java.lang.Integer 1 2 3)))

    (check-equal? 0 (array-size empty-obj-arr))

    (check-equal? 2 (array-size obj-arr))
    (check-equal? 42 (array-ref obj-arr 0))
    (check-equal? "HELLO" (array-ref obj-arr 1))
    (check-equal? (void) (array-set! obj-arr 1 "WORLD"))
    (check-equal? "WORLD" (array-ref obj-arr 1))

    (check-equal? 3 (array-size int-arr))
    (check-equal? 1 (array-ref int-arr 0))
    (check-equal? 2 (array-ref int-arr 1))
    (check-equal? 3 (array-ref int-arr 2))

    (check-equal? 3 (array-size str-arr))
    (check-equal? "a" (array-ref str-arr 0))
    (check-equal? "b" (array-ref str-arr 1))
    (check-equal? "c" (array-ref str-arr 2)))

  (check-equal? 0 (length '()))
  (check-equal? 1 (length '(1)))
  (check-equal? 2 (length '(1 2)))

  (check-equal? 0 (array-size (array)))
  (check-equal? 1 (array-size (array 1)))
  (check-equal? 2 (array-size (array 1 2)))

  (let ()
    (define Integer 'java.lang.Integer)
    (check-equal? 0 (array-size (typed-array Integer)))
    (check-equal? 1 (array-size (typed-array Integer 1)))
    (check-equal? 2 (array-size (typed-array Integer 1 2))))

  (check-equal? #t (array? (array)))
  (check-equal? #t (array? (typed-array 'java.lang.Integer)))
  (check-equal? #f (array? 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "constructor->procedure method->procedure"
  (let ([array-list (constructor->procedure 'java.util.ArrayList)]
         [substr (method->procedure 'java.lang.String 'substring 'int 'int)]
         ; '[Ljava.lang.Object 存在[ 不能用 sym
         [fmt (method->procedure 'java.lang.String 'format 'java.lang.String "[Ljava.lang.Object;")]
         [exit (method->procedure 'java.lang.System 'exit 'int)])
    (check-equal? (arraylist) (array-list))
    (check-equal? "HELLO" (substr "HELLO WORLD" 0 5))
    ; java 方法 转换成 procedure 之后就可以 apply 了
    (check-equal? "HELLO WORLD!"
      (apply fmt null "%s %s!" (list (array "HELLO" "WORLD"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "-> ->>"
  (check-equal? "ABC"
    (-> (str "A") (str "B") (str "C")))

  (check-equal? "CBA"
    (->> (str "A") (str "B") (str "C")))

  (check-equal? 5
    (-> 2 (+ 3)))
  (check-equal? -2
    (-> 2 (+ 3) (- 7)))
  (check-equal? 42
    (-> 4 (* 4 3) (- 6)))
  (check-equal? '(1 2 3)
    (->> '(1 2 3) (map add1) (map sub1)))

  (check-equal?
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
      (java.util.Arrays/toString))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin ".."
  (check-equal?
    (java.lang.System/getProperty "java.vm.version")
    (.. java.lang.System (getProperties) (get "java.vm.version")))
  (check-equal?
    (. (. java.lang.System (getProperties)) (get "os.name"))
    (.. java.lang.System (getProperties) (get "os.name"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "doto"
  (let ((m (new java.util.HashMap)))
    (.put m 'a 1)
    (.put m 'b 2)
    (check-equal?
      m
      (doto
        (java.util.HashMap.)
        (.put 'a 1)
        (.put 'b 2))))

  (check-equal?
    (hashmap ('a 1) ('b 2))
    (doto
      (java.util.HashMap.)
      (.put 'a 1)
      (.put 'b 2)))

  (check-equal?
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

  (check-equal?
    (arraylist 1 2 3)
    (doto
      (java.util.ArrayList.)
      (.add 1)
      (.add 2)
      (.add 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 简单的代理
;(let* ([a #f]
;       [proxy (java.lang.Runnable.
;               (hashmap
;                 ("run" (lambda () (set! a 1) NULL))
;                 ("toString" (lambda () "run-impl"))))])
;  (check-equal? (.toString proxy) "run-impl")
;  (assert (.startsWith (.toString (class proxy)) "class com.sun.proxy"))
;  (assert (instance? java.lang.Runnable proxy))
;  (check-equal? a #f)
;  (.run proxy)
;  (check-equal? a 1))