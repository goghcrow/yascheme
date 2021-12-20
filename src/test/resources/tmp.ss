;(println (load "/Users/chuxiaofeng/Library/Mobile Documents/com~apple~CloudDocs/project/xiao/src/test/resources/pure.ss"))






(println (apply + `(,(+ 1 2) 3)))
(println "￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥")
(println (list 1 '. 2))
(println
  (let-values (((zϟ12) '9))
    (letrec-values (((xϟ14) '5) ((yϟ15) (lambda (zϟ17) zϟ17)))
      (let-values (((zϟ18) '10))
        (letrec-values (
                         (
                           (xϟ19 yϟ20 zϟ21)
                           (values '1 '2 '3)
                         )
                       )
          (begin zϟ21 (if xϟ19 yϟ20 zϟ21)))))))
(println (letrec-values (
                          (
                            (a b c)
                            (values '1 '2 '3)
                          )
                        )
           (begin a (if a b c))))
(println "￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥￥")

(println (gensym))
(println (gensym))
(println (gensym "HELLO"))

(println (values))
(println (values 1))
(println (values 1 2))

(call/cc (lambda (cc1) (call/cc (lambda (cc2) (println (eq? cc1 cc2))))))




; System.out.println(Class.forName("[Ljava.lang.Object;"));
;; todo 特殊处理 print 打印格式化数组
;; todo 判断是否是数组
(println (.isInstance
           (java.lang.Class/forName "[Ljava.lang.Object;")
           (java.lang.reflect.Array/newInstance
             (java.lang.Class/forName "java.lang.Object")
             0)))
;; 调用 Arrays.toString()

(define array-object
  (lambda args
    (begin
      (define arr
        (java.lang.reflect.Array/newInstance
          (java.lang.Class/forName "java.lang.Object")
          (.size args)))
      (define i 0)
      (for-each
        (lambda (arg)
          (begin
            (java.lang.reflect.Array/set arr i arg)
            (set! i (+ i 1)))) args)
      arr)))

(println (array-object 1 2 "Hello"))


(println "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")



(xiao.lang2.JavaInterop$CallSite/field
  (java.lang.Class/forName "java.lang.Void")
  ;  "..."
  "TYPE")

;Executable[] constructors(Class<?> cls) {
;            expect(!cls.isInterface(), "assert false");
;            List<Executable> lst = new ArrayList<>();
;            while (cls != Object.class) {
;                Collections.addAll(lst, cls.getDeclaredConstructors());
;                cls = cls.getSuperclass();
;            }
;            return lst.toArray(new Executable[0]);
;        }
;
;        Executable dispatch(
;                Executable[] executables,
;                String method,
;                Object[] args,
;                int modifiers // 暂时用作区分静态和实例方法
;        ) {
;            List<Executable> matches = new ArrayList<>();
;
;            for (Executable m : executables) {
;                if (
;
;                        m.getName().equals(method)
;                        && m.getParameterCount() == args.length
;                ) {
;                    Class<?>[] pTypes = m.getParameterTypes();
;                    boolean matched = true;
;                    for (int i = 0; i < args.length; i++) {
;                        Object arg = args[i];
;                        Class<?> pType = pTypes[i];
;                        if (arg == null) {
;                            if (pType.isPrimitive()) {
;                                matched = false;
;                                break;
;                            }
;                        } else {
;                            // 解释器里的类型都是包装类型, 否则需要对 arg 也进行包装
;                            if (!boxed(pType).isInstance(arg)) {
;                                matched = false;
;                                break;
;                            }
;                        }
;                    }
;                    if (matched) {
;  return m;
;                    }
;                }
;            }
;
;                throw new InterpError("NoSuchMethod: " + fmtMethodCall(klass, method, args));
;        }


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define my-if
;  (macro (test then else)
;    `(let ((a 42)) (if ,test ,then ,else))))
;(define a 100)
;(my-if true (println a) (raise "ERROR"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; todo 测试不定参数的 macro !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; test coerce
(assert-equals (.charAt "A" 0) (char 65))

; from string
;(byte a)
;(short a)
;(int a)
;(long a)
;(float a)
;(double a)
;(char a)
;(bigint a)
;(bigdec a)





;(define (fib n)
;  (cond
;    ([< n 0] (raise 'err))
;    ([= n 0] 1)
;    ([= n 1] 1)
;    (else (+ (fib (- n 2)) (fib (- n 1))))))
;
;(println (fib 5))


;(define-syntax argcount
;  (syntax-rules ()
;    ((argcount) 0)
;    ((argcount x) 1)
;    ((argcount x y) 2)
;    ((argcount x y rest ...)
;      (+ (argcount x y) (.size '(rest ...))))))
;
;(assert-equals 0 (argcount))
;(assert-equals 1 (argcount 1))
;(assert-equals 2 (argcount 1 2))
;(assert-equals 5 (argcount 1 2 3 4 5))



;(println (arraylist 1 3 "HELLO"))
;(println (hashmap (1 2) ('hello 3.14)))

; (call/cc call/cc)




;(java.lang.Runnable. )

(println (let ([rnd (java.util.Random.)])
           (. rnd (nextInt 10))))
(println java.lang.Math/PI)




;(define r1 #f)
;(define r2 #f)
;(define (somefunc x y)  (+ x y))
;(set! value
;  (somefunc (call/cc
;              (lambda (c1)
;                (set! r1 c1)
;                (c1 1)))
;    (call/cc
;      (lambda (c2)
;        (set! r2 c2)
;        (c2 1)))))
;(r1 5)
;(r2 5)




;(define a (cons 1 2))
;(eq? a a)


;(define values (lambda (& things)
;          (call-with-current-continuation
;            (lambda (cont) (apply cont things)))))

;(values 1 2 3)
;
;(define-values (a b c) '(1 2 3))
;`(,a ,b, c)









;;; ------------------------------------------------
;;; 注意这里 new 不能改名, (class. ) 语法会被改写成 (new class)
;;; (new class args ...)
;;; (new (interfaces +) map-string-to-lambda-expr)
;;; ------------------------------------------------
;(define let
;  (syntax-rules ()
;    ((let ([id val] ...) body ...)
;      ((lambda (id ...) body ...) val ...))
;    ((let proc ([id init] ...) body ...)
;      ; 这里 letrec 因为 body 必须能引用到 proc
;      (letrec ([proc (lambda (id ...) body ...)])
;        (proc init ...)))))
;(define new
;  (syntax-rules ()
;    ((new class args ...)
;      (let ((tmp-klass (java.lang.Class/forName (symbol->string 'class))))
;        (if (.isInterface tmp-klass)
;          (let ((tmp-methods (car (list args ...))))
;            (xiao.lang.JavaInterop/proxy
;              tmp-klass
;              tmp-methods
;              (current-environment)))
;          (.newInstance
;            (xiao.lang.JavaInterop$CallSite/constructor tmp-klass)
;            (.toArray (list args ...))))))))
;
;
;(new java.util.ArrayList)






;;;;;;;;; BUGBUGBUG
;(define displayln (lambda (a) (.println java.lang.System/out a)))
;
;(define let
;  (syntax-rules ()
;    ((let ([id val] ...) body ...)
;      ((lambda (id ...) body ...) val ...))))
;
;(define and
;  (syntax-rules ()
;    ((and test) test)
;    ((and test rest ...)
;      (let ([$test test])
;        (if $test
;          (and rest ...)
;          $test)))))
;
;(define =
;  (syntax-rules ()
;    ((= a) (xiao.lang2.Generated/eq a a)) ; 类型检查
;    ((= a b rest ...)
;      (let ([$b b])
;        (and (xiao.lang2.Generated/eq a $b) (= $b rest ...)))
;    )))
;
;; a = 1 b = 1 rest = [2] , a == $b 1 == 1
;; $b == rest
;; 1 == 1 &&
;; 1 $b2
;
;(displayln (= 1 1 2))
;
;(displayln "syntax-rules BUG")
;
;
;
;(define begin (lambda xs
;  (if (.isEmpty xs)
;    (void)
;    (.last xs))))
;
;(let ((a 2))
;  (begin
;    (define macro-test
;      (syntax-rules ()
;        ((_)
;          (let ((a 1))
;            (displayln a))))) ; BUG
;    (macro-test)))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; todo 去掉卫生宏的逻辑, 全都做声非卫生好了
;; 做一个 gensym #1 #2 #3
;; todo defmacro
;; todo 非卫生宏
;; parser -> reader
;; 或者 reader wrap parser ...
;; 静态属性那个 逻辑放到 reader
;; syntax-rules 做成先展开
;
;
;
;;clojure
;;(defmacro test1 [] '(def a 1))
;;(test1)
;;(println a) ; 1
;
;; racket
;; (define-syntax test
;;  (syntax-rules ()
;;    ((_) (define a 1))))
;;(test)
;;(display a) ; 报错
;
;
;
;;(let ((a 1))
;;  ((macro ()
;;       '(let ((a 2))
;;          (displayln a)))))
;;(displayln "~~~~~~~~~~~")
;
;
;
;
;(define and
;  (syntax-rules ()
;    ((and test) test)
;    ((and test rest ...)
;      (let ([tmp test])
;        (if tmp
;          (and rest ...)
;          tmp)))))
;
;
;(let ((t 1))
;  (begin
;    (define =
;      (syntax-rules ()
;        ((= a) (xiao.lang2.Generated/eq a a)) ; 类型检查
;        ((= a b rest ...)
;          (let ([t b])
;            (and (xiao.lang2.Generated/eq a t) (= t rest ...))))))
;    (displayln (= 1 1 2))
;  ))
;
;(displayln "------------------")
;
;
;(let ((t 100))
;  (begin
;    (define =
;      (syntax-rules ()
;        ((= a) (xiao.lang2.Generated/eq a a)) ; 类型检查
;        ((= a b rest ...)
;          (let ([t b])
;            (and (xiao.lang2.Generated/eq a t) (= t rest ...))))))
;    (displayln (= 1 1 2))
;  ))
;
;; 11 22 22
;; 这里的问题是, t 处理成 直接返回, 第二次 = 当前环境的 t 直接就是 2, 所以 2 = 2
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;;(define displayln (lambda (a) (.println java.lang.System/out a)))
;;(define begin (lambda (& xs)
;;                (if (.isEmpty xs)
;;                  (void)
;;                  (.last xs))))
;;(define let
;;  (syntax-rules ()
;;    ((let ([id val] ...) body ...)
;;      ((lambda (id ...) body ...) val ...))))
;;
;;(define =
;;  (syntax-rules ()
;;    ((= a) (displayln a)) ; 类型检查
;;    ((= a b rest ...)
;;      (let ([$b b])
;;        (begin
;;          (displayln "------------")
;;          (displayln a)
;;          (displayln b)
;;          (displayln $b)
;;          (displayln `(= ,$b rest ...))
;;          (displayln "------------")
;;          (= $b rest ...)
;;        )
;;      )
;;    )))
;;
;;;(= 1)
;;;(= 1 2)
;;(= 1 2 3)
;;
;;
;;
;;
;;;(define test
;;;  (syntax-rules ()
;;;    ((_) (displayln 'end))
;;;    ((_ arg args ...)
;;;      (begin
;;;        (displayln arg)
;;;        ; (displayln args) todo 正确处理 indexoutofbound 报错
;;;        (displayln '(args ...))
;;;        (test args ...)))))
;;
;;;(test 1 2 3)
;
;
;
;
;
;
;
;
;
;;; 用定义的作用域也不行
;;(define let
;;  (syntax-rules ()
;;    ((let ([id val] ...) body ...)
;;      ((lambda (id ...) body ...) val ...))))
;;
;;(define new
;;  (syntax-rules ()
;;    ((_)
;;      (let ((a 1))
;;        ; 这里 (debugger a) 会变成文法闭包, 出错了, body 作用域应该是 lambda 定义的子作用域 BUG
;;        (debugger a)
;;      ))))
;;
;;(debugger)
;;(new)
