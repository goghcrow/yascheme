;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(assert-equals "\u4e2d\t\u56fd" "中\t国")
(println "---------------------")
(print "\r\n\b\f")
(println "\"")
(println "\\")
(println '"\"")
(println "---------------------")

; parser bug
(println (+ -0xb 0xf +0xd))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; define & lambda

; lambda body 自动加 begin
(assert-equals 5
  ((lambda (a) (+ 1 1) (+ 2 3)) 0))
; λ alias
(assert-equals 5
  ((λ (a) (+ 1 1) (+ 2 3)) 0))

; ((lambda (a a) a) 1 2) ; 参数重名
; (let ((a 1) (a 1)) a) ; 参数重名

(let ()
  (define (add a b) (+ a b))
  (assert-equals 5 (add 1 4))

  (define (f0) 0)
  (define (f1 a) a)
  (define (f2 a b) `(,a ,b))
  (define (f3 a . rest) `(,a ,@rest))

  (assert-equals (f0) 0)
  (assert-equals (f1 1) 1)
  (assert-equals (f2 1 2) '(1 2))
  (assert-equals (f3 1 2 3) '(1 2 3))

  (assert-equals ((lambda xs xs)) '())
  (assert-equals ((lambda xs xs) 1) '(1))
  (assert-equals ((lambda xs xs) 1 2) '(1 2))

  (assert-equals ((lambda () 0)) 0)
  (assert-equals ((lambda (a) a) 1) 1)
  (assert-equals ((lambda (a b) `(,a ,b)) 1 2) '(1 2))
  (assert-equals ((lambda (a . rest) `(,a ,@rest)) 1) '(1))
  (assert-equals ((lambda (a . rest) `(,a ,@rest)) 1 2) '(1 2))
  (assert-equals ((lambda (a . rest) `(,a ,@rest)) 1 2 3) '(1 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; case-lambda

(assert-equals
  ((case-lambda
     [() 0]
     [() 1]))
  0)
(assert-equals
  ((case-lambda
     [() 1]
     [() 0]))
  1)
(assert-equals
  ((case-lambda
     [(a) 0]
     [(a) 1]) #f)
  0)
(assert-equals
  ((case-lambda
     [(a) 1]
     [(a) 0]) #f)
  1)

(let ([argcount (case-lambda
                  (() 0)
                  ((x) 1)
                  ((x y) 2)
                  (rest (.size rest)))])
  (assert-equals 0 (argcount))
  (assert-equals 1 (argcount 1))
  (assert-equals 2 (argcount 1 2))
  (assert-equals 5 (argcount 1 2 3 4 5)))

(let ()
  (define case-f1
    (case-lambda
      [() 0]
      [(x) x]
      [(x y) `(,y ,x)]
      [r r]))

  (assert-equals (case-f1) 0)
  (assert-equals (case-f1 1) 1)
  (assert-equals (case-f1 1 2) '(2 1))
  (assert-equals (case-f1 1 2 3) '(1 2 3))

  (define case-f2
    (case-lambda
      [() 0]
      [(x) x]
      [(x y) `(,y ,x)]
      [(x . rest) `(,x ,@rest)]
      [r #f]))

  (assert-equals (case-f2) 0)
  (assert-equals (case-f2 1) 1)
  (assert-equals (case-f2 1 2) '(2 1))
  (assert-equals (case-f2 1 2 3) '(1 2 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; eq?
(assert-equals (eq? 1 1) #t)
(assert-equals (eq? '(1) '(1)) #f)
(assert-equals (eq? "s" "s") #f)
(let ()
  (define a "s")
  (assert-equals (eq? a a) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; define set!
(let ()
  (define id 42)
  (assert-equals id 42)
  (set! id 1)
  (assert-equals id 1))

; define-values
(let ()
  (define-values (a) 1)
  (assert-equals a 1))

(let ()
  (define-values (a b) (values 1 2))
  (assert-equals a 1)
  (assert-equals b 2))

(let ()
  (define-values (a b c) (values 1 2 3))
  (assert-equals (+ a b c) 6)
  (assert-equals a 1)
  (assert-equals b 2)
  (assert-equals c 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; let
; Parallel Binding: let
; The fact that an id’s expr does not see its own binding is often useful
; for wrappers that must refer back to the old value:
(assert-equals
  (let ([+ (lambda (x y)
             (if (string? x)
               (string-append x y)
               (+ x y)))]) ; use original +
    (list (+ 1 2)
      (+ "see" "saw")))
  '(3 "seesaw"))

; Occasionally, the parallel nature of let bindings is convenient
; for swapping or rearranging a set of bindings:
(assert-equals
  (let ([me "Tarzan"]
         [you "Jane"])
    (let ([me you]
           [you me])
      (list me you)))
    '("Jane" "Tarzan"))

(assert-equals 3628800
  (let fac ([n 10])
    (if (zero? n)
      1
      (* n (fac (sub1 n))))))

(let () (void))
(assert-equals 1 (let () (void) 1))
(assert-equals 3 (let ([a 1] [b 2]) (+ a b)))
(assert-equals '(5 2)
  (let ([x 5])
    (let ([x 2]
           [y x])
      (list y x))))

; Named let
;(assert-equals
;  (let ()
;    (define (duplicate pos lst)
;      (let dup ([i 0]
;                 [lst lst])
;        (cond
;          [(= i pos) (cons (car lst) lst)]
;          [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))
;    (duplicate 1 (list "apple" "cheese burger!" "banana")))
;    '("apple" "cheese burger!" "cheese burger!" "banana"))
(assert-equals
  (let ()
    (define (duplicate pos lst)
      (let dup ([i 0]
                 [lst lst])
        (if
          (= i pos)
          (cons (car lst) lst)
          (cons (car lst) (dup (+ i 1) (cdr lst))))))
    (duplicate 1 (list "apple" "cheese burger!" "banana")))
    '("apple" "cheese burger!" "cheese burger!" "banana"))

; Sequential Binding: let*
(assert-equals
  (let* ([x (list "Burroughs")]
          [y (cons "Rice" x)]
          [z (cons "Edgar" y)])
    (list x y z))
    '(("Burroughs") ("Rice" "Burroughs") ("Edgar" "Rice" "Burroughs")))
(assert-equals
  (let* ([name (list "Burroughs")]
          [name (cons "Rice" name)]
          [name (cons "Edgar" name)])
    name)
    '("Edgar" "Rice" "Burroughs"))

; In other words, a let* form is equivalent to nested let forms, each with a single binding:
(assert-equals
  (let ([name (list "Burroughs")])
    (let ([name (cons "Rice" name)])
      (let ([name (cons "Edgar" name)])
        name)))
    '("Edgar" "Rice" "Burroughs"))

(assert-equals (let* () (void)) (void))
(let* ([a 1])
  (assert-equals a 1))
(let* ([a 1] [b a])
  (assert-equals b 1))
(let* ([a 1] [b a] [c b])
  (assert-equals c 1))
(let* ([a 1] [a 2])
  (assert-equals a 2))
(assert-equals '(2 1)
  (let* ([x 1]
          [y (+ x 1)])
    (list y x)))

; Recursive Binding: letrec
(assert-equals (letrec () (void)) (void))
(assert-equals
  (letrec ([swing
             (lambda (t)
               (if (eq? (car t) 'tarzan)
                 (cons 'vine
                   (cons 'tarzan (cdr (cdr t))))
                 (cons (car t)
                   (swing (cdr t)))))])
    (swing '(vine tarzan vine vine)))
    '(vine vine tarzan vine))

; (letrec ([a b] [b a]) (println a) (println b))
(letrec ([is-even? (lambda (n)
                     (or (zero? n)
                       (is-odd? (sub1 n))))]
          [is-odd? (lambda (n)
                     (and (not (zero? n))
                       (is-even? (sub1 n))))])
  (assert-equals true (is-odd? 11))
  (assert-equals true (is-even? 12)))


; Multiple Values: let-values, let*-values, letrec-values
(assert-equals (let-values () (void)) (void))
(assert-equals (let-values (((a) 1)) a) 1)
(assert-equals (let-values (((a b) (values 1 2))) (+ a b)) 3)
(assert-equals (call-with-values (lambda () (values 1 2)) +) 3)
(assert-equals (call-with-values (lambda () 1) (lambda (x) x)) 1)

(assert-equals (let*-values () (void)) (void))
(let*-values (((a) 1))
  (assert-equals a 1))
(let*-values (((a b) (values 1 2)) ((a b) (values 2 3)))
  (assert-equals a 2)
  (assert-equals b 3))


(assert-equals (letrec-values () (void)) (void))
(letrec-values (((a) 1))
  (assert-equals a 1))
(letrec-values (
                 [(is-even? is-odd?)
                   (values
                     (lambda (n)
                       (or (zero? n)
                         (is-odd1? (sub1 n))))
                     (lambda (n)
                       (and (not (zero? n))
                         (is-even1? (sub1 n))))
                   )]
                 [(is-even1? is-odd1?)
                   (values
                     (lambda (n)
                       (or (zero? n)
                         (is-odd? (sub1 n))))
                     (lambda (n)
                       (and (not (zero? n))
                         (is-even? (sub1 n))))
                   )]
               )
  (assert-equals true (is-odd? 11))
  (assert-equals true (is-even? 12))
  (assert-equals true (is-odd1? 11))
  (assert-equals true (is-even1? 12)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; member
(assert-equals '(2 3 4) (member 2 (list 1 2 3 4)))
(assert-equals #f (member 9 (list 1 2 3 4)))
(assert-equals (cons 'b 'etc) (member 'b (cons 'a (cons 'b 'etc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; callcc

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; list, list*
(assert-equals (quote ()) (list))
(assert-equals (quote (1)) (list 1))
(assert-equals (quote (1 2)) (list 1 2))
(assert-equals (quote (1 2 3)) (list 1 2 3))

(assert-equals (list* 1) 1)
(assert-equals (list* 1 2) (cons 1 2))
(assert-equals (list* 1 2) '(1 . 2))
(assert-equals (list* 1 2) `(1 . 2))
(assert-equals (list* 1 '(2)) '(1 2))
(assert-equals (list* 1 2 '(3 4)) '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; +-*/
(assert-equals 0 (+))
(assert-equals 1 (+ 1))
(assert-equals 3 (+ 1 2))
(assert-equals 6 (+ 1 2 3))

(assert-equals -1 (- 1))
(assert-equals 1 (- -1))
(assert-equals 10 (- 12 2))
(assert-equals 5 (- 12 2 5))

(assert-equals 1 (*))
(assert-equals 42 (* 42))
(assert-equals 6 (* 2 3))
(assert-equals 24 (* 2 3 4))

(assert-equals 0 (/ 42))
(assert-equals 6 (/ 12 2))
(assert-equals 2 (/ 12 2 3))

(assert (= 42))
(assert (= 1 1))
(assert-equals #f (= 1 2))
(assert (= 1 1 1))
(assert-equals #f (= 1 1 2))
(assert-equals #f (= 1 2 1))

(assert (< 42))
(assert (< 1 2))
(assert-equals #f (< 2 1))
(assert (< 1 2 3))
(assert-equals #f (< 1 2 1))

(assert (> 42))
(assert-equals #f (> 1 2))
(assert (> 2 1))
(assert (> 3 2 1))
(assert-equals #f (> 3 2 3))
(assert-equals #f (> 3 3 2))

(assert (<= 42))
(assert (<= 1 2))
(assert (<= 1 1))
(assert (<= 1 1 1))
(assert-equals #f (<= 2 1))
(assert (<= 1 2 3))
(assert (<= 1 1 2))
(assert-equals #f (<= 1 2 1))

(assert (>= 42))
(assert-equals #f (>= 1 2))
(assert (>= 1 1))
(assert (>= 1 1 1))
(assert (>= 2 1))
(assert-equals #f (>= 1 2 3))
(assert-equals #f (>= 2 2 3))
(assert (>= 2 2 1))
(assert (>= 3 2 1))


(assert-equals (+ 1 1.1) 2.1)
(assert-equals (+ 1 1.1f) 2.1f)
(assert-equals (+ 1l 1.1f) 2.1f)
(assert (<= (java.lang.Math/abs (- (+ 1.1d 1.1f) 2.2d)) 0.000001))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; if
(assert-equals 1 (if #t 1 (raise "ERR")))
(assert-equals 2 (if #f (raise "ERR") 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; and or

(assert-equals #t (and))
(assert-equals 1 (and 1))
(assert-equals false (and false))
(assert-equals true (and true))
(assert-equals #f (and #f 2 3))
(assert-equals #f (and 1 #f 3))
(assert-equals 3 (and 1 2 3))
(assert-equals #f (and #f (raise "not here")))

(assert-equals #f (or))
(assert-equals 1 (or 1))
(assert-equals false (or false))
(assert-equals true (or true))
(assert-equals 1 (or 1 2 3))
(assert-equals 2 (or #f 2 3))
(assert-equals 3 (or #f #f 3))
(assert-equals 1 (or 1 (raise "not here")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; predication
(assert (void? (void)))
(assert (not (void? 1)))

(assert (number? 1))
(assert (not (number? "")))

(assert (null? null))
(assert (not (pair? null)))
(assert (list? null))

(assert (list? (cons 1 null)))
(assert (pair? (cons 1 null)))
(assert (cons? (cons 1 null)))

(assert (not (list? (cons 1 2))))
(assert (pair? (cons 1 2)))
(assert (cons? (cons 1 2)))

(assert (list? (quote (1 2))))
(assert (list? (cons 1 (cons 2 null))))

(assert (procedure? list))
(assert (procedure? cons))
(assert (procedure? car))
(assert (procedure? cdr))
(assert-equals (procedure? 1) false)
(assert-equals (procedure? #t) false)
(assert-equals (procedure? "") false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cons car cdr
(assert-equals null (quote ()))
(assert-equals null '())
(assert-equals (cons 1 null) '(1))
(assert-equals (quote (1)) (cons 1 null))
(assert-equals (cons 1 (cons 2 null)) '(1 2))
(assert-equals (quote (1 2)) (cons 1 (cons 2 null)))
(assert-equals (car '(1 2)) 1)
(assert-equals (cdr '(1 2)) (cons 2 null))

; first rest
(assert-equals (first '(1 2 3)) 1)
(assert-equals (rest '(1 2 3)) '(2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; str
(assert-equals (str "Hello" " World " "🍺") "Hello World 🍺")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map
(assert-equals '(2 3 4) (map (lambda (a) (+ a 1)) '(1 2 3)))
(assert-equals null (map (lambda (a) a) '()))

(assert-equals
  (map (lambda (number1 number2)
         (+ number1 number2))
      '(1 2 3 4)
      '(10 100 1000 10000))
    '(11 102 1003 10004))

(assert-equals
  (map (lambda (a b c)
         (str a b c))
      '("Hello" "a")
      '("World" "b")
      '("!" "c"))
    '("HelloWorld!" "abc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; java method
(assert-equals 5 (.length "Hello"))
(assert-equals "He" (.substring "Hello" 0 2))
(assert-equals "llo" (.substring "Hello" 2 5))

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
  (assert-equals 6 sum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; quote

(assert-equals
  (quote (1 2 7 5))
  (quasiquote (1 2 (unquote (+ 3 4)) 5)))
(assert-equals
  '(1 2 7 5)
  `(1 2 ,(+ 3 4) 5))

(let ([a 42])
  (assert-equals
    (quote (1 2 42 5))
    (quasiquote (1 2 (unquote a) 5))))
(let ([a 42])
  (assert-equals
    '(1 2 42 5)
    `(1 2 ,a 5)))

(assert-equals
  (cons 1 (cons null null))
  (quasiquote (1 (unquote null))))
(assert-equals
  (list 1 '())
  `(1 ,'()))

(assert-equals
  (quote (1 2 3 4 5 6 7))
  (quasiquote (1 2 (unquote-splicing (quote (3 4))) 5 6 (unquote 7))))
(assert-equals
  '(1 2 3 4 5 6 7)
  `(1 2 ,@'(3 4) 5 6 7))

(assert-equals
  (quote (1 2 (unquote-splicing (quote (3 4))) 5 6 (unquote 7)))
  (quote (1 2 (unquote-splicing (quote (3 4))) 5 6 (unquote 7))))
(assert-equals
  '(1 2 ,@'(3 4) 5 6 ,7)
  '(1 2 (unquote-splicing (quote (3 4))) 5 6 (unquote 7)))

; dot notation
(assert-equals (quote (1 . 2)) (cons 1 2))
(assert-equals '(1 . 2) (cons 1 2))
(assert-equals '(0 . (1 . 2)) (cons 0 (cons 1 2)))
(assert-equals '(0 . (1 . (2 . ()))) '(0 1 2))

(assert-equals (quasiquote (1 . 2)) (cons 1 2))
(assert-equals `(1 . 2) (cons 1 2))
(assert-equals `(0 . (1 . 2)) (cons 0 (cons 1 2)))
(assert-equals `(0 . (1 . (2 . ()))) '(0 1 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; eval
;; TODOTODOTODOTODOTODOTODOTODOTODOTODO
;(assert-equals 1 (eval '1))
;(assert-equals 3 (eval '(+ 1 2)))

; (.equals null (eval (quote null))) ; NPE, 要用 equals
;(assert-equals null (eval (quote null)))
;(assert-equals '() (eval ''()))


;(let* ( [a 42]
;        [env (current-environment)])
;  (let ( [a 1]
;         [env1 (make-environment)])
;    (.put env1 "a" 100)
;    (assert-equals 1 (eval 'a))
;    (assert-equals 42 (eval 'a env))
;    (assert-equals 100 (eval 'a env1))))

; apply
(assert-equals (apply + '()) 0)
(assert-equals (apply + '(1 2 3)) 6)
(assert-equals (apply + 1 2 '(3)) 6)
(assert-equals (apply + 1 2 '(3 4)) 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO
;; do
;(let ([a 0])
;  (assert-equals 6
;    (do ([i 1 (+ i 1)])
;      ((> i 5) i)
;      (set! a (+ a 1))))
;  (assert-equals a 5))
;
;; 写两个会用后面的 step
;;(do ([i 1 (+ i 1) (+ i 2)])
;;  ((> i 5) (println 'end))
;;  (println i))
;
;(let ([a 0])
;  (assert-equals 10
;    (do ([i 1 (+ i 1)]
;          [j 1 (+ j 1)])
;      ((> i 5) a)
;      (set! a (+ i j)))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cond
; TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO
;(assert-equals (void) (cond))
;(assert-equals 5 (cond [else 5]))
;(assert-equals 6 (cond [else 5 6]))
;(assert-equals 5 (cond [true 5]))
;(assert-equals 6 (cond [true 5 6]))
;(assert-equals 'here (cond
;           [(positive? -5) (error "doesn't get here")]
;           [(zero? -5) (error "doesn't get here, either")]
;           [(positive? 5) 'here]))
;(assert-equals '(-1 -2 -3) (cond ['(1 2 3) => (lambda (l) (map - l))]))
;(assert-equals (void) (cond [#f => (error "err")]))
;(assert-equals 1 (cond [1]))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case
; TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO
;(let ([a 0])
;  (assert-equals (void) (case (set! a 1)))
;  (assert-equals 1 a))
;(let ([a 0])
;  (assert-equals 1 (case (set! a 1) [else 1]))
;  (assert-equals 1 a))
;
;(assert-equals 'big
;  (case (+ 7 5)
;    [(1 2 3) 'small]
;    [(10 11 12) 'big]))
;(assert-equals 'small
;  (case (- 7 5)
;    [(1 2 3) 'small]
;    [(10 11 12) 'big]))
;
;(assert-equals "animal"
;  (case (string-append "do" "g")
;    [("cat" "dog" "mouse") "animal"]
;    [else "mineral or vegetable"]))
;
;(assert-equals 'backwards
;  (case (list 'y 'x)
;    [((a b) (x y)) 'forwards]
;    [((b a) (y x)) 'backwards]))
;
;(assert-equals "ex"
;  (case 'x
;    [(x) "ex"]
;    [('x) "quoted ex"]))
;
;(assert-equals "quoted ex"
;  ; '(quote x)
;  ; ''x
;  (case (list 'quote 'x)
;    [(x) "ex"]
;    [('x) "quoted ex"]))
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODOTODOTODOTODOTODOTODOTODOTODOTODOTODOTODO
;; 宏
;
;(let ()
;  (define-syntax-rule (swap x y)
;    (let ([tmp x])
;      (set! x y)
;      (set! y tmp)))
;  (define-values (a b) (values 1 2))
;  (swap a b)
;  (assert-equals a 2)
;  (assert-equals b 1))
;
;(define-syntax-rule (if1 cond then else)
;  (if cond then else))
;(assert-equals "TRUE" (if1 #t "TRUE" (raise "FALSE")))
;(assert-equals "FALSE" (if1 #f (raise "TRUE") "FALSE"))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;匿名宏
;(assert-equals ((syntax-rules ()
;                  ((_ a b)
;                    (cons b a))) 1 2) (cons 2 1))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 递归宏
;(let ([a 1] [b 2] [c 3] [d 4])
;  (define-syntax swap
;    (syntax-rules ()
;      ([_] (void))
;      ([_ a] (void))
;      ([swap a b]
;        (let ([tmp b])
;          (set! b a)
;          (set! a tmp)))
;      ([swap a b rest ...]
;        (begin
;          (swap a b)
;          (swap b rest ...)))))
;
;  (swap)
;  (swap a)
;
;  (assert-equals a 1)
;  (assert-equals b 2)
;  (swap a b)
;  (assert-equals a 2)
;  (assert-equals b 1)
;
;  (set! a 1)
;  (set! b 2)
;
;  (assert-equals a 1)
;  (assert-equals b 2)
;  (assert-equals c 3)
;  (assert-equals d 4)
;  (swap a b c d)
;  ; 1234 -> 2134 -> 2314 -> 2341
;  (assert-equals 2 a)
;  (assert-equals 3 b)
;  (assert-equals 4 c)
;  (assert-equals 1 d))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; 卫生宏 vs 非卫生宏
;
;;(let ()
;;  (define my-or
;;    (macro (e1 e2)
;;        `(let ([t ,e1]) (if t t ,e2))))
;;
;;  (assert-equals true (my-or #t #f))
;;  (assert-equals true (my-or #f #t))
;;
;;  (assert-equals true
;;    (let ([t #t])
;;      (my-or #f t)))
;;
;;  (assert-equals false
;;    (let
;;      ([if (λ (x y z) "oops")])
;;      (my-or #f #f)))
;;)
;
;; 卫生宏 vs 非卫生宏
;
;(define-syntax my-or
;  (syntax-rules ()
;    ([_ e1 e2]
;      (let ([t e1]) (if t t e2)))))
;
;;(define-syntax non-hygienic-my-or
;;  (non-hygienic-syntax-rules ()
;;    ([_ e1 e2]
;;      (let ([t e1]) (if t t e2)))))
;
;(assert-equals true (my-or #t #f))
;(assert-equals true (my-or #f #t))
;
;(assert-equals true
;  (let ([t #t])
;    (my-or #f t)))
;
;;(assert-equals false
;;  (let ([t #t])
;;    (non-hygienic-my-or #f t)))
;
;(assert-equals false
;  (let
;    ([if (λ (x y z) "oops")])
;    (my-or #f #f)))
;
;;(assert-equals "oops"
;;  (let
;;    ([if (λ (x y z) "oops")])
;;    (non-hygienic-my-or #f #f)))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delay force
;(let* ([a 0] [b (delay (set! a 1))])
;  (assert-equals 0 a)
;  (force b)
;  (assert-equals 1 a)
;  (set! a 0)
;  (force b)
;  (assert-equals 0 a))