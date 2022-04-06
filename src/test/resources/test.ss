(test-begin "parser bug"
  (check-equal? 17 (+ -0xb 0xf +0xd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "struct"
  (struct foo (a b))
  (define s (foo 1 2))
  ;; furble is not a foo struct
  ;; (foo-a "furble")
  (check-equal? #t (foo? s))
  (check-equal? #f (foo? 1))
  (check-equal? (foo-a s) 1)
  (check-equal? (foo-b s) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "eq? equal?"
  (check-equal? #t (eq? '() '()))
  (check-equal? #t (eq? '() null))
  (check-equal? #t (eq? '() (list)))
  (check-equal? #t (eq? '() (cdr '(1))))

  (check-equal? #t (eq? 1 1))
  (check-equal? #f (eq? '(1) '(1)))
  (check-equal? #f (eq? "s" "s"))
  (let ()
    (define a "s")
    (check-equal? #t (eq? a a)))

  (check-equal? #f (equal? '(1) '(1.0)))

  (check-equal? #t (equal? "s" "s"))
  (check-equal? #t (equal? #(1 2) #(1 2)))
  (check-equal? #t (equal? #(#(1)) #(#(1))))
  (check-equal? #t (equal? (cons #(1) 1) (cons #(1) 1)))
  (check-equal? #t (equal? (list #(1)) (list #(1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "define"
  (define a 1)
  (check-equal? 1 a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "Function Shorthand"
  (define (add a b) (+ a b))
  (check-equal? 5 (add 1 4))

  (define (f . args) args)
  (check-equal? '(1 2 3) (f 1 2 3))

  (define (test a . rest) (cons a rest))
  (check-equal? '(1 2 3) (test 1 2 3))

  (define (f0) 0)
  (define (f1 a) a)
  (define (f2 a b) `(,a ,b))
  (define (f3 a . rest) `(,a ,@rest))

  (check-equal? 0 (f0))
  (check-equal? 1 (f1 1))
  (check-equal? '(1 2) (f2 1 2))
  (check-equal? '(1 2 3) (f3 1 2 3))

  (check-equal? '() ((lambda xs xs)))
  (check-equal? '(1) ((lambda xs xs) 1))
  (check-equal? '(1 2) ((lambda xs xs) 1 2))

  (check-equal? 0 ((lambda () 0)))
  (check-equal? 1 ((lambda (a) a) 1))
  (check-equal? '(1 2) ((lambda (a b) `(,a ,b)) 1 2))
  (check-equal? '(1) ((lambda (a . rest) `(,a ,@rest)) 1))
  (check-equal? '(1 2) ((lambda (a . rest) `(,a ,@rest)) 1 2))
  (check-equal? '(1 2 3) ((lambda (a . rest) `(,a ,@rest)) 1 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "Curried Function Shorthand"
  (define ((make-add-suffix s2) s)
    (string-append s s2))
  (define less-sure (make-add-suffix "?"))
  (define louder (make-add-suffix "!"))
  (check-equal? "hello!" ((make-add-suffix "!") "hello"))
  (check-equal? "really!" (louder "really"))

  (define ((f a) . rest) (cons a rest))
  (check-equal? '(1 2 3) ((f 1) 2 3))

  (define (((cf a) b) c) (list a b c))
  (check-equal? '(1 2 3) (((cf 1) 2) 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "define-values"
  (let ()
    (define-values (a) 1)
    (check-equal? 1 a))

  (let ()
    (define-values (a b) (values 1 2))
    (check-equal? 1 a)
    (check-equal? 2 b))

  (let ()
    (define-values (a b c) (values 1 2 3))
    (check-equal? 6 (+ a b c))
    (check-equal? 1 a)
    (check-equal? 2 b)
    (check-equal? 3 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "set!"
  (define id 42)
  (check-equal? 42 id)
  (set! id 1)
  (check-equal? 1 id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "lambda"
  ; ((lambda (a a) a) 1 2) ; 参数重名
  ; (let ((a 1) (a 1)) a) ; 参数重名

  "lambda body 自动加 begin"
  (check-equal? 5
    ((lambda (a) (+ 1 1) (+ 2 3)) 0))

  "λ alias"
  (check-equal? 5
    ((λ (a) (+ 1 1) (+ 2 3)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "case-lambda"
  (check-equal? 0
    ((case-lambda
       [() 0]
       [() 1])))
  (check-equal? 1
    ((case-lambda
       [() 1]
       [() 0])))
  (check-equal? 0
    ((case-lambda
       [(a) 0]
       [(a) 1]) #f))
  (check-equal? 1
    ((case-lambda
       [(a) 1]
       [(a) 0]) #f))

  (let ([argcount (case-lambda
                    (() 0)
                    ((x) 1)
                    ((x y) 2)
                    (rest (.size rest)))])
    (check-equal? 0 (argcount))
    (check-equal? 1 (argcount 1))
    (check-equal? 2 (argcount 1 2))
    (check-equal? 5 (argcount 1 2 3 4 5)))

  (let ()
    (define case-f1
      (case-lambda
        [() 0]
        [(x) x]
        [(x y) `(,y ,x)]
        [r r]))

    (check-equal? 0 (case-f1))
    (check-equal? 1 (case-f1 1))
    (check-equal? '(2 1) (case-f1 1 2))
    (check-equal? '(1 2 3) (case-f1 1 2 3))

    (define case-f2
      (case-lambda
        [() 0]
        [(x) x]
        [(x y) `(,y ,x)]
        [(x . rest) `(,x ,@rest)]
        [r #f]))

    (check-equal? 0 (case-f2))
    (check-equal? 1 (case-f2 1))
    (check-equal? '(2 1) (case-f2 1 2))
    (check-equal? '(1 2 3) (case-f2 1 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "let"
; Parallel Binding: let
; The fact that an id’s expr does not see its own binding is often useful
; for wrappers that must refer back to the old value:

  ; no body forms
  ;(let ((a 1))
  ;  (define a 2))
  (let ((a 1))
    (define a 2)
    (check-equal? 2 a))

  (check-equal?
      '(3 "seesaw")
    (let ([+ (lambda (x y)
               (if (string? x)
                 (string-append x y)
                 (+ x y)))]) ; use original +
      (list (+ 1 2)
        (+ "see" "saw"))))

  ; Occasionally, the parallel nature of let bindings is convenient
  ; for swapping or rearranging a set of bindings:
  (check-equal?
      '("Jane" "Tarzan")
    (let ([me "Tarzan"]
           [you "Jane"])
      (let ([me you]
             [you me])
        (list me you))))

  (check-equal? 3628800
    (let fac ([n 10])
      (if (zero? n)
        1
        (* n (fac (sub1 n))))))

  (let () (void))
  (check-equal? 1 (let () (void) 1))
  (check-equal? 3 (let ([a 1] [b 2]) (+ a b)))
  (check-equal? '(5 2)
    (let ([x 5])
      (let ([x 2]
             [y x])
        (list y x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "Named let"
  (check-equal?
      '("apple" "cheese burger!" "cheese burger!" "banana")
    (let ()
      (define (duplicate pos lst)
        (let dup ([i 0]
                   [lst lst])
          (cond
            [(= i pos) (cons (car lst) lst)]
            [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))
      (duplicate 1 (list "apple" "cheese burger!" "banana"))))
  (check-equal?
      '("apple" "cheese burger!" "cheese burger!" "banana")
    (let ()
      (define (duplicate pos lst)
        (let dup ([i 0]
                   [lst lst])
          (if
            (= i pos)
            (cons (car lst) lst)
            (cons (car lst) (dup (+ i 1) (cdr lst))))))
      (duplicate 1 (list "apple" "cheese burger!" "banana")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "Sequential Binding: let*"
  (check-equal?
      '(("Burroughs") ("Rice" "Burroughs") ("Edgar" "Rice" "Burroughs"))
    (let* ([x (list "Burroughs")]
            [y (cons "Rice" x)]
            [z (cons "Edgar" y)])
      (list x y z)))
  (check-equal?
      '("Edgar" "Rice" "Burroughs")
    (let* ([name (list "Burroughs")]
            [name (cons "Rice" name)]
            [name (cons "Edgar" name)])
      name))

  ; In other words, a let* form is equivalent to nested let forms, each with a single binding:
  (check-equal?
      '("Edgar" "Rice" "Burroughs")
    (let ([name (list "Burroughs")])
      (let ([name (cons "Rice" name)])
        (let ([name (cons "Edgar" name)])
          name))))

  (check-equal? (void) (let* () (void)))
  (let* ([a 1])
    (check-equal? 1 a))
  (let* ([a 1] [b a])
    (check-equal? 1 b))
  (let* ([a 1] [b a] [c b])
    (check-equal? 1 c))
  (let* ([a 1] [a 2])
    (check-equal? 2 a))
  (check-equal? '(2 1)
    (let* ([x 1]
            [y (+ x 1)])
      (list y x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "Recursive Binding: letrec"
  (check-equal? (void) (letrec () (void)))
  (check-equal?
      '(vine vine tarzan vine)
    (letrec ([swing
               (lambda (t)
                 (if (eq? (car t) 'tarzan)
                   (cons 'vine
                     (cons 'tarzan (cdr (cdr t))))
                   (cons (car t)
                     (swing (cdr t)))))])
      (swing '(vine tarzan vine vine))))

  ; (letrec ([a b] [b a]) (println a) (println b))
  (letrec ([is-even? (lambda (n)
                       (or (zero? n)
                         (is-odd? (sub1 n))))]
            [is-odd? (lambda (n)
                       (and (not (zero? n))
                         (is-even? (sub1 n))))])
    (check-equal? true (is-odd? 11))
    (check-equal? true (is-even? 12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "Multiple Values: let-values, let*-values, letrec-values"
  (check-equal? (void) (let-values () (void)))
  (check-equal? 1 (let-values (((a) 1)) a))
  (check-equal? 3 (let-values (((a b) (values 1 2))) (+ a b)))
  (check-equal? 3 (call-with-values (lambda () (values 1 2)) +))
  (check-equal? 1 (call-with-values (lambda () 1) (lambda (x) x)))

  (check-equal? (void) (let*-values () (void)))
  (let*-values (((a) 1))
    (check-equal? 1 a))
  (let*-values (((a b) (values 1 2)) ((a b) (values 2 3)))
    (check-equal? 2 a)
    (check-equal? 3 b))


  (check-equal? (void) (letrec-values () (void)))
  (letrec-values (((a) 1))
    (check-equal? 1 a))
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

    (check-equal? 42 (let ([a 42]) `,`,`,a))
    (check-equal? true (is-odd? 11))
    (check-equal? true (is-even? 12))
    (check-equal? true (is-odd1? 11))
    (check-equal? true (is-even1? 12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "cons car cdr first rest map"
  (check-equal? null (quote ()))
  (check-equal? null '())
  (check-equal? '(1) (cons 1 null))
  (check-equal? (quote (1)) (cons 1 null))
  (check-equal?  '(1 2) (cons 1 (cons 2 null)))
  (check-equal? (quote (1 2)) (cons 1 (cons 2 null)))
  (check-equal? 1 (car '(1 2)))
  (check-equal? (cdr '(1 2)) (cons 2 null))
  (check-equal? 1 (first '(1 2 3)))
  (check-equal? '(2 3) (rest '(1 2 3)))

  (check-equal? '(2 3 4) (map (lambda (a) (+ a 1)) '(1 2 3)))
  (check-equal? null (map (lambda (a) a) '()))

  (check-equal?
      '(11 102 1003 10004)
    (map (lambda (number1 number2)
           (+ number1 number2))
        '(1 2 3 4)
        '(10 100 1000 10000)))

  (check-equal?
      '("HelloWorld!" "abc")
    (map (lambda (a b c)
           (str a b c))
        '("Hello" "a")
        '("World" "b")
        '("!" "c"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "begin"
  (check-equal? '(x y) (append '(x) '(y)))
  (check-equal? '(a b c d) (append '(a) '(b c d)))
  (check-equal? '(a (b) (c)) (append '(a (b)) '((c))))
  (check-equal? '(a b c . d) (append '(a b) '(c . d)))
  (check-equal? 'a (append '() 'a))
  (check-equal? '() (append))
  (check-equal? 'a (append 'a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "list list* build-list make-list member"
  (check-equal? (quote ()) (list))
  (check-equal? (quote (1)) (list 1))
  (check-equal? (quote (1 2)) (list 1 2))
  (check-equal? (quote (1 2 3)) (list 1 2 3))

  (check-equal? 1 (list* 1))
  (check-equal? (cons 1 2) (list* 1 2))
  (check-equal? '(1 . 2) (list* 1 2))
  (check-equal? `(1 . 2) (list* 1 2))
  (check-equal? '(1 2) (list* 1 '(2)))
  (check-equal? '(1 2 3 4) (list* 1 2 '(3 4)))

  (check-equal? '(0 1 2 3 4 5 6 7 8 9) (build-list 10 values))
  (check-equal? '(0 1 4 9 16) (build-list 5 (lambda (x) (* x x))))
  (check-equal? '(foo foo foo foo foo foo foo) (make-list 7 'foo))

  (check-equal? '(2 3 4) (member 2 (list 1 2 3 4)))
  (check-equal? #f (member 9 (list 1 2 3 4)))
  (check-equal? (cons 'b 'etc) (member 'b (cons 'a (cons 'b 'etc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "vector"
  (check-equal? #() (vector))
  (check-equal? #(1 2 3) (vector 1 2 3))
  (check-equal? #(1 1 1) (make-vector 3 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "+ - * / min max"
  (check-equal? 0 (+))
  (check-equal? 1 (+ 1))
  (check-equal? 3 (+ 1 2))
  (check-equal? 6 (+ 1 2 3))

  (check-equal? -1 (- 1))
  (check-equal? 1 (- -1))
  (check-equal? 10 (- 12 2))
  (check-equal? 5 (- 12 2 5))

  (check-equal? 1 (*))
  (check-equal? 42 (* 42))
  (check-equal? 6 (* 2 3))
  (check-equal? 24 (* 2 3 4))


  ;(check-equal? 0 (/ 42))
  (check-equal? #t (< (abs (- (/ 42) 0.02380952)) 0.00001))
  (check-equal? 6 (/ 12 2))
  (check-equal? 2 (/ 12 2 3))

  (check-equal? #t (= 42))
  (check-equal? #t (= 1 1))
  (check-equal? #t (= 1 1.0))
  (check-equal? #f (= 1 2))
  (check-equal? #t (= 1 1 1))
  (check-equal? #f (= 1 1 2))
  (check-equal? #f (= 1 2 1))

  (check-equal? #t (< 42))
  (check-equal? #t (< 1 2))
  (check-equal? #f (< 2 1))
  (check-equal? #t (< 1 2 3))
  (check-equal? #f (< 1 2 1))

  (check-equal? #t (> 42))
  (check-equal? #f (> 1 2))
  (check-equal? #t (> 2 1))
  (check-equal? #t (> 3 2 1))
  (check-equal? #f (> 3 2 3))
  (check-equal? #f (> 3 3 2))

  (check-equal? #t (<= 42))
  (check-equal? #t (<= 1 2))
  (check-equal? #t (<= 1 1))
  (check-equal? #t (<= 1 1 1))
  (check-equal? #f (<= 2 1))
  (check-equal? #t (<= 1 2 3))
  (check-equal? #t (<= 1 1 2))
  (check-equal? #f (<= 1 2 1))

  (check-equal? #t (>= 42))
  (check-equal? #f (>= 1 2))
  (check-equal? #t (>= 1 1))
  (check-equal? #t (>= 1 1 1))
  (check-equal? #t (>= 2 1))
  (check-equal? #f (>= 1 2 3))
  (check-equal? #f (>= 2 2 3))
  (check-equal? #t (>= 2 2 1))
  (check-equal? #t (>= 3 2 1))


  (check-equal? 2.1 (+ 1 1.1))
  (check-equal? 2.1f (+ 1 1.1f))
  (check-equal? 2.1f (+ 1l 1.1f))
  (check-equal? #t (<= (java.lang.Math/abs (- (+ 1.1d 1.1f) 2.2d)) 0.000001))

  (check-equal? 42 (max 42))
  (check-equal? 42 (min 42))
  (check-equal? 3 (max 1 3 2))
  (check-equal? 3 (max 1 3 2.0))
  (check-equal? 1 (min 1 3 2))
  (check-equal? 1 (min 1 3 2.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "if and or predication"
  (check-equal? 1 (if #t 1 (raise "ERR")))
  (check-equal? 2 (if #f (raise "ERR") 2))

  (check-equal? #t (and))
  (check-equal? 1 (and 1))
  (check-equal? false (and false))
  (check-equal? true (and true))
  (check-equal? #f (and #f 2 3))
  (check-equal? #f (and 1 #f 3))
  (check-equal? 3 (and 1 2 3))
  (check-equal? #f (and #f (raise "not here")))

  (check-equal? #f (or))
  (check-equal? 1 (or 1))
  (check-equal? false (or false))
  (check-equal? true (or true))
  (check-equal? 1 (or 1 2 3))
  (check-equal? 2 (or #f 2 3))
  (check-equal? 3 (or #f #f 3))
  (check-equal? 1 (or 1 (raise "not here")))

  (check-equal? #t (void? (void)))
  (check-equal? #t (not (void? 1)))

  (check-equal? #t (number? 1))
  (check-equal? #t (not (number? "")))

  (check-equal? #t (null? null))
  (check-equal? #t (not (pair? null)))
  (check-equal? #t (list? null))

  (check-equal? #t (list? (cons 1 null)))
  (check-equal? #t (pair? (cons 1 null)))
  (check-equal? #t (cons? (cons 1 null)))

  (check-equal? #t (not (list? (cons 1 2))))
  (check-equal? #t (pair? (cons 1 2)))
  (check-equal? #t (cons? (cons 1 2)))

  (check-equal? #t (list? (quote (1 2))))
  (check-equal? #t (list? (cons 1 (cons 2 null))))

  (check-equal? #t (procedure? list))
  (check-equal? #t (procedure? cons))
  (check-equal? #t (procedure? car))
  (check-equal? #t (procedure? cdr))
  (check-equal? (procedure? 1) false)
  (check-equal? (procedure? #t) false)
  (check-equal? (procedure? "") false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "quote"
  ; unquote-splicing 支持 vector
  (check-equal? #(10 5 4 16 9 8)
      `#(10 5 ,(square 2) ,@(map square '(4 3)) 8))

  (define (deep n)
    (cond
      [(zero? n) 0]
      [else
          `(,n ,(deep (sub1 n)))]))
  (check-equal?
      '(8 (7 (6 (5 (4 (3 (2 (1 0))))))))
    (deep 8))

  (check-equal?
    (quote (1 2 7 5))
    (quasiquote (1 2 (unquote (+ 3 4)) 5)))
  (check-equal?
      '(1 2 7 5)
      `(1 2 ,(+ 3 4) 5))

  (let ([a 42])
    (check-equal?
      (quote (1 2 42 5))
      (quasiquote (1 2 (unquote a) 5))))
  (let ([a 42])
    (check-equal?
        '(1 2 42 5)
        `(1 2 ,a 5)))

  (check-equal?
    (cons 1 (cons null null))
    (quasiquote (1 (unquote null))))
  (check-equal?
    (list 1 '())
      `(1 ,'()))

  (check-equal?
    (quote (1 2 3 4 5 6 7))
    (quasiquote (1 2 (unquote-splicing (quote (3 4))) 5 6 (unquote 7))))
  (check-equal?
      '(1 2 3 4 5 6 7)
      `(1 2 ,@'(3 4) 5 6 7))

  (check-equal?
    (quote (1 2 (unquote-splicing (quote (3 4))) 5 6 (unquote 7)))
    (quote (1 2 (unquote-splicing (quote (3 4))) 5 6 (unquote 7))))
  (check-equal?
      '(1 2 ,@'(3 4) 5 6 ,7)
      '(1 2 (unquote-splicing (quote (3 4))) 5 6 (unquote 7))))

(test-begin "quote dot notation"
  (check-equal?
      '(1 . (2 . 3))
      `(1 . (2 . 3)))

  (check-equal? (cons 1 2) (quote (1 . 2)))
  (check-equal? (cons 1 2) '(1 . 2))
  (check-equal? (cons 0 (cons 1 2)) '(0 . (1 . 2)))
  (check-equal? '(0 1 2) '(0 . (1 . (2 . ()))))

  (check-equal? (cons 1 2) (quasiquote (1 . 2)))
  (check-equal? (cons 1 2) `(1 . 2))
  (check-equal? (cons 0 (cons 1 2)) `(0 . (1 . 2)))
  (check-equal? '(0 1 2) `(0 . (1 . (2 . ()))))

  (check-equal? '(a b c . d) (append '(a b) '(c . d)))
  (check-equal? 'd (cdr (cdr (cdr '(a b c . d)))))
  (check-equal? 'd (cdr (cdr (cdr (append '(a b) '(c . d)))))))


(test-begin "nested quasiquote"
  (check-equal? '(a `(b ,x ,'y d) e)
    (let-values (((name1) 'x)  ((name2) 'y))
        `(a `(b ,,name1 ,',name2 d) e)))
  (check-equal? 42
    (let-values ([(a) 42]) `,a))
  (check-equal? '(1 2 `,(+ 1 2))
      `(1 2 `,(+ 1 2)))
  (check-equal? '(1 2 `(,(+ 1 2) ,4))
      `(1 2 `(,(+ 1 2) ,,(- 5 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "eval apply"
  (check-equal? 1 (eval '1))
  (check-equal? 3 (eval '(+ 1 2)))
  ; (.equals null (eval (quote null))) ; NPE, 要用 equals
  (check-equal? null (eval (quote null)))
  (check-equal? '() (eval ''()))
  (check-equal? 0 (apply + '()))
  (check-equal? 6 (apply + '(1 2 3)))
  (check-equal? 6 (apply + 1 2 '(3)))
  (check-equal? 10 (apply + 1 2 '(3 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "do"
  (let ([a 0])
    (check-equal? 6
      (do ([i 1 (+ i 1)])
        ((> i 5) i)
        (set! a (+ a 1))))
    (check-equal? a 5))

  (let ([a 0])
    (check-equal? 10
      (do ([i 1 (+ i 1)]
            [j 1 (+ j 1)])
        ((> i 5) a)
        (set! a (+ i j)))))

  (check-equal? (void)
    (let ([a 0])
      (do ([i 1 (+ i 1)]) ((> i 5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "cond"
  (check-equal? (void) (cond))
  (check-equal? (void) (cond (#f)))
  (check-equal? 5 (cond [else 5]))
  (check-equal? 6 (cond [else 5 6]))
  (check-equal? 5 (cond [true 5]))
  (check-equal? 6 (cond [true 5 6]))
  (check-equal? 'here (cond
                         [(positive? -5) (error "doesn't get here")]
                         [(zero? -5) (error "doesn't get here, either")]
                         [(positive? 5) 'here]))
  (check-equal? '(2 3)
    (cond
      [(member 2 '(1 2 3))]))
  (check-equal? '(-2 -3)
    (cond
      [(member 2 '(1 2 3)) => (lambda (l) (map - l))]))
  (check-equal? '(-1 -2 -3) (cond ['(1 2 3) => (lambda (l) (map - l))]))
  (check-equal? (void) (cond [#f => (error "err")]))
  (check-equal? 1 (cond [1]))
  ;(cond (else (define a 1))) ;; 错误
  ;(cond (else (define a 1))) (println a) ; 错误
  ;(cond (else (define a 1) (println a)))  (println a) ; 错误
  (cond [true (define a 1) (check-equal? 1 a)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "case"
  (let ([a 0])
    (check-equal? (void) (case (set! a 1)))
    (check-equal? 1 a))
  (let ([a 0])
    (check-equal? 1 (case (set! a 1) [else 1]))
    (check-equal? 1 a))

  (check-equal? 'big
    (case (+ 7 5)
      [(1 2 3) 'small]
      [(10 11 12) 'big]))
  (check-equal? 'small
    (case (- 7 5)
      [(1 2 3) 'small]
      [(10 11 12) 'big]))

  (check-equal? "animal"
    (case (string-append "do" "g")
      [("cat" "dog" "mouse") "animal"]
      [else "mineral or vegetable"]))

  (check-equal? 'backwards
    (case (list 'y 'x)
      [((a b) (x y)) 'forwards]
      [((b a) (y x)) 'backwards]))

  (check-equal? "ex"
    (case 'x
      [(x) "ex"]
      [('x) "quoted ex"]))

  (check-equal? "quoted ex"
    ; '(quote x)
    ; ''x
    (case (list 'quote 'x)
      [(x) "ex"]
      [('x) "quoted ex"]))

  ; 正常情况 define 不能写在表达式, 没做, 目前放在 let 中
  (case (define a 1))
  (case (begin (define a 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "delay force"
  (let* ([a 0] [b (delay (set! a 1))])
    (check-equal? 0 a)
    (force b)
    (check-equal? 1 a)
    (set! a 0)
    (force b)
    (check-equal? 0 a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "gensym string->symbol symbol->string"
  (check-equal? #t (.startsWith (symbol->string (gensym "HELLO")) "HELLO"))
  (check-equal? #f (equal? (gensym) (gensym)))
  (check-equal? #f (eq? (gensym) (gensym)))
  (check-equal? #f (equal? (gensym "prefix") (gensym "prefix")))
  (check-equal? #f (eq? (gensym "prefix") (gensym "prefix")))

  (check-equal? #t (symbol? (string->symbol "Hello")))
  (check-equal? #t (string? (symbol->string 'Hello))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "str"
  (check-equal? "" (str))
  (check-equal? (str "Hello" " World " "🍺") "Hello World 🍺"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "字符串转义"
  (check-equal? "\u4e2d\t\u56fd" "中\t国")
  (call-with-output-string
    (lambda ()
      (print "\r\n\b\f")
      (println "\"")
      (println "\\")
      (println '"\""))))

