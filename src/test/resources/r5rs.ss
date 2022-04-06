(test-begin "r5rs"

  (check-equal? 8 ((lambda (x) (+ x x)) 4))

  (check-equal? '(3 4 5 6) ((lambda x x) 3 4 5 6))

  (check-equal? '(5 6) ((lambda (x y . z) z) 3 4 5 6))

  (check-equal? 'yes (if (> 3 2) 'yes 'no))

  (check-equal? 'no (if (> 2 3) 'yes 'no))

  (check-equal? 1 (if (> 3 2) (- 3 2) (+ 3 2)))

  (check-equal? 'greater (cond ((> 3 2) 'greater) ((< 3 2) 'less)))

  (check-equal? 'equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))

  (check-equal? 'composite (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))

  (check-equal? 'consonant
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else 'consonant)))

  (check-equal? #t (and (= 2 2) (> 2 1)))

  (check-equal? #f (and (= 2 2) (< 2 1)))

  (check-equal? '(f g) (and 1 2 'c '(f g)))

  (check-equal? #t (and))

  (check-equal? #t (or (= 2 2) (> 2 1)))

  (check-equal? #t (or (= 2 2) (< 2 1)))

  (check-equal? '(b c) (or (member 'b '(a b c)) (/ 3 0))) ; todo memq

  (check-equal? 6 (let ((x 2) (y 3)) (* x y)))

  (check-equal? 35 (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))

  (check-equal? 70 (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))

  (check-equal? -2 (let ()
                    (define x 2)
                    (define f (lambda () (- x)))
                    (f)))

  (define let*-def 1)
  (let* () (define let*-def 2) #f)
  (check-equal? 1 let*-def)

  (check-equal? '#(0 1 2 3 4)
    (do ((vec (make-vector 5))
          (i 0 (+ i 1)))
      ((= i 5) vec)
      (vector-set! vec i i)))

  (check-equal? 25
    (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
            (sum 0 (+ sum (car x))))
        ((null? x)
          sum))))

  (check-equal? '((6 1 3) (-5 -2))
    (let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '()))
      (cond
        ((null? numbers)
          (list nonneg neg))
        ((>= (car numbers) 0)
          (loop (cdr numbers) (cons (car numbers) nonneg) neg))
        ((< (car numbers) 0)
          (loop (cdr numbers) nonneg (cons (car numbers) neg))))))

  (check-equal? '(list 3 4) `(list ,(+ 1 2) 4))

  (check-equal? '(list a 'a) (let ((name 'a)) `(list ,name ',name)))

  (check-equal? '(a 3 4 5 6 b)
      `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))

  (check-equal? '(10 5 4 16 9 8)
      `(10 5 ,(expt 2 2) ,@(map (lambda (n) (expt n 2)) '(4 3)) 8))
  ; !!!
  (check-equal? '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
      `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
  ; !!!
  (check-equal? '(a `(b ,x ,'y d) e)
    (let ((name1 'x)
           (name2 'y))
        `(a `(b ,,name1 ,',name2 d) e)))

  (check-equal? '(list 3 4)
    (quasiquote (list (unquote (+ 1 2)) 4)))

  (check-equal? #t (eqv? 'a 'a))

  (check-equal? #f (eqv? 'a 'b))

  (check-equal? #t (eqv? '() '()))

  (check-equal? #f (eqv? (cons 1 2) (cons 1 2)))

  (check-equal? #f (eqv? (lambda () 1) (lambda () 2)))

  (check-equal? #t (let ((p (lambda (x) x))) (eqv? p p)))

  (check-equal? #t (eq? 'a 'a))

  (check-equal? #f (eq? (list 'a) (list 'a)))

  (check-equal? #t (eq? '() '()))

  (check-equal? #t (eq? car car))

  (check-equal? #t (let ((x '(a))) (eq? x x)))

  (check-equal? #t (let ((p (lambda (x) x))) (eq? p p)))

  (check-equal? #t (equal? 'a 'a))

  (check-equal? #t (equal? '(a) '(a)))

  (check-equal? #t (equal? '(a (b) c) '(a (b) c)))

  (check-equal? #t (equal? "abc" "abc"))

  (check-equal? #f (equal? "abc" "abcd"))

  (check-equal? #f (equal? "a" "b"))

  (check-equal? #t (equal? 2 2))

  (check-equal? #f (eqv? 2 2.0))

  (check-equal? #f (equal? 2.0 2))

  (check-equal? #t (equal? (make-vector 5 'a) (make-vector 5 'a)))

  (check-equal? 4 (max 3 4))

  (check-equal? 4 (max 3.9 4))

  (check-equal? 7 (+ 3 4))

  (check-equal? 3 (+ 3))

  (check-equal? 0 (+))

  (check-equal? 4 (* 4))

  (check-equal? 1 (*))

  (check-equal? -1 (- 3 4))

  (check-equal? -6 (- 3 4 5))

  (check-equal? -3 (- 3))

  (check-equal? -1.0 (- 3.0 4))

  (check-equal? 7 (abs -7))

  (check-equal? 1 (modulo 13 4))

  (check-equal? 1 (remainder 13 4))

  (check-equal? 3 (modulo -13 4))

  (check-equal? -1 (remainder -13 4))

  (check-equal? -3 (modulo 13 -4))

  (check-equal? 1 (remainder 13 -4))

  (check-equal? -1 (modulo -13 -4))

  (check-equal? -1 (remainder -13 -4))

  (check-equal? 4 (gcd 32 -36))

  (check-equal? 288 (lcm 32 -36))

  (check-equal? 100 (string->number "100"))

  (check-equal? 256 (string->number "100" 16))

  (check-equal? 127 (string->number "177" 8))

  (check-equal? 5 (string->number "101" 2))

  (check-equal? 100.0 (string->number "1e2"))

  (check-equal? "100" (number->string 100))

  (check-equal? "100" (number->string 256 16))

  (check-equal? "ff" (number->string 255 16))

  (check-equal? "177" (number->string 127 8))

  (check-equal? "101" (number->string 5 2))

  (check-equal? #f (not 3))

  (check-equal? #f (not (list 3)))

  (check-equal? #f (not '()))

  (check-equal? #f (not (list)))

  (check-equal? #f (not '()))

  (check-equal? #f (boolean? 0))

  (check-equal? #f (boolean? '()))

  (check-equal? #t (pair? '(a . b)))

  (check-equal? #t (pair? '(a b c)))

  (check-equal? '(a) (cons 'a '()))

  (check-equal? '((a) b c d) (cons '(a) '(b c d)))

  (check-equal? '("a" b c) (cons "a" '(b c)))

  (check-equal? '(a . 3) (cons 'a 3))

  (check-equal? '((a b) . c) (cons '(a b) 'c))

  (check-equal? 'a (car '(a b c)))

  (check-equal? '(a) (car '((a) b c d)))

  (check-equal? 1 (car '(1 . 2)))

  (check-equal? '(b c d) (cdr '((a) b c d)))

  (check-equal? 2 (cdr '(1 . 2)))

  (check-equal? #t (list? '(a b c)))

  (check-equal? #t (list? '()))

  (check-equal? #f (list? '(a . b)))

  ;(check-equal? #f
  ;  (let ((x (list 'a)))
  ;    (set-cdr! x x)
  ;    (list? x)))

  (check-equal? '(a 7 c) (list 'a (+ 3 4) 'c))

  (check-equal? '() (list))

  (check-equal? 3 (length '(a b c)))

  (check-equal? 3 (length '(a (b) (c d e))))

  (check-equal? 0 (length '()))

  (check-equal? '(x y) (append '(x) '(y)))

  (check-equal? '(a b c d) (append '(a) '(b c d)))

  (check-equal? '(a (b) (c)) (append '(a (b)) '((c))))

  (check-equal? '(a b c . d) (append '(a b) '(c . d)))

  (check-equal? 'a (append '() 'a))

  (check-equal? '(c b a) (reverse '(a b c)))

  (check-equal? '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

  (check-equal? 'c (list-ref '(a b c d) 2))

  (check-equal? '(a b c) (memq 'a '(a b c)))

  (check-equal? '(b c) (memq 'b '(a b c)))

  (check-equal? #f (memq 'a '(b c d)))

  (check-equal? #f (memq (list 'a) '(b (a) c)))

  (check-equal? '((a) c) (member (list 'a) '(b (a) c)))

  (check-equal? '(101 102) (memv 101 '(100 101 102)))

  (check-equal? #f (assq (list 'a) '(((a)) ((b)) ((c)))))

  (check-equal? '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

  (check-equal? '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

  (check-equal? #t (symbol? 'foo))

  (check-equal? #t (symbol? (car '(a b))))

  (check-equal? #f (symbol? "bar"))

  (check-equal? #t (symbol? 'nil))

  (check-equal? #f (symbol? '()))

  (check-equal? "flying-fish" (symbol->string 'flying-fish))

  (check-equal? "Martin" (symbol->string 'Martin))

  (check-equal? "Malvina" (symbol->string (string->symbol "Malvina")))

  (check-equal? #t (string? "a"))

  (check-equal? #f (string? 'a))

  (check-equal? 0 (string-length ""))

  (check-equal? 3 (string-length "abc"))

  (check-equal? #\a (string-ref "abc" 0))

  (check-equal? #\c (string-ref "abc" 2))

  (check-equal? #t (string=? "a" (string #\a)))

  (check-equal? #f (string=? "a" (string #\b)))

  (check-equal? #t (string<? "a" "aa"))

  (check-equal? #f (string<? "aa" "a"))

  (check-equal? #f (string<? "a" "a"))

  (check-equal? #t (string<=? "a" "aa"))

  (check-equal? #t (string<=? "a" "a"))

  (check-equal? #t (string=? "a" (make-string 1 #\a)))

  (check-equal? #f (string=? "a" (make-string 1 #\b)))

  (check-equal? "" (substring "abc" 0 0))

  (check-equal? "a" (substring "abc" 0 1))

  (check-equal? "bc" (substring "abc" 1 3))

  (check-equal? "abc" (string-append "abc" ""))

  (check-equal? "abc" (string-append "" "abc"))

  (check-equal? "abc" (string-append "a" "bc"))

  (check-equal? '#(0 ("Sue" "Sue") "Anna")
    (let ((vec (vector 0 '(2 2 2 2) "Anna")))
      (vector-set! vec 1 '("Sue" "Sue"))
      vec))

  (check-equal? '(dah dah didah) (vector->list '#(dah dah didah)))

  (check-equal? '#(dididit dah) (list->vector '(dididit dah)))

  (check-equal? #t (procedure? car))

  (check-equal? #f (procedure? 'car))

  (check-equal? #t (procedure? (lambda (x) (* x x))))

  (check-equal? #f (procedure? '(lambda (x) (* x x))))

  (check-equal? #t (call-with-current-continuation procedure?))

  (check-equal? 7 (call-with-current-continuation (lambda (k) (+ 2 5))))

  (check-equal? 3 (call-with-current-continuation (lambda (k) (+ 2 5 (k 3)))))

  (check-equal? 7 (apply + (list 3 4)))

  (check-equal? '(b e h) (map cadr '((a b) (d e) (g h))))

  (check-equal? '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))

  (check-equal? '(5 7 9) (map + '(1 2 3) '(4 5 6)))

  (check-equal? '#(0 1 4 9 16)
    (let ((v (make-vector 5)))
      (for-each
        (lambda (i) (vector-set! v i (* i i)))
          '(0 1 2 3 4))
      v))

  (check-equal? 3 (force (delay (+ 1 2))))

  (check-equal? '(3 3) (let ((p (delay (+ 1 2)))) (list (force p) (force p))))
  ; !!!
  (check-equal? 'ok (let ((else 1)) (cond (else 'ok) (#t 'bad))))
  ; !!!
  (check-equal? 'ok (let ((=> 1)) (cond (#t => 'ok))))
  ; !!!
  (check-equal? '(,foo) (let ((unquote 1)) `(,foo)))
  ; !!!
  (check-equal? '(,@foo) (let ((unquote-splicing 1)) `(,@foo)))

  ;(check-equal? 'ok
  ;  (let ((... 2))
  ;    (let-syntax ((s (syntax-rules ()
  ;                      ((_ x ...) 'bad)
  ;                      ((_ . r) 'ok))))
  ;      (s a b c))))
  ; 这个没有按用例处理
  (check-equal? 'bad
    (let ((... 2))
      (let-syntax ((s (lambda (stx)
                        (let-values ([(m) (try-match-syntax stx '(s x ...))])
                          (if m
                            (quote-syntax 'bad)
                            (begin
                              (match-syntax stx '(s . r))
                              (quote-syntax 'ok)))))))
        (s a b c))))

  (check-equal? '(2 1)
    ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (list x y))))))

  (check-equal? '(2 2)
    ((lambda () (let ((x 1)) (set! x 2) (let ((y x)) (list x y))))))

  (check-equal? '(1 2)
    ((lambda () (let ((x 1)) (let ((y x)) (set! y 2) (list x y))))))

  (check-equal? '(2 3)
    ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (set! y 3) (list x y))))))

  ;(check-equal? '(a b c)
  ;  (let* ((path '())
  ;          (add (lambda (s) (set! path (cons s path)))))
  ;    (dynamic-wind (lambda () (add 'a)) (lambda () (add 'b)) (lambda () (add 'c)))
  ;    (reverse path)))
  ;
  ;(check-equal? '(connect talk1 disconnect connect talk2 disconnect)
  ;  (let ((path '())
  ;         (c #f))
  ;    (let ((add (lambda (s)
  ;                 (set! path (cons s path)))))
  ;      (dynamic-wind
  ;        (lambda () (add 'connect))
  ;        (lambda ()
  ;          (add (call-with-current-continuation
  ;                 (lambda (c0)
  ;                   (set! c c0)
  ;                     'talk1))))
  ;        (lambda () (add 'disconnect)))
  ;      (if (< (length path) 4)
  ;        (c 'talk2)
  ;        (reverse path)))))
  ;
  ;(check-equal? 2 (let-syntax
  ;          ((foo (syntax-rules ::: ()
  ;                  ((foo ... args :::)
  ;                    (args ::: ...)))))
  ;          (foo 3 - 5)))
  ;
  ;(check-equal? '(5 4 1 2 3)
  ;  (let-syntax
  ;    ((foo (syntax-rules ()
  ;            ((foo args ... penultimate ultimate)
  ;              (list ultimate penultimate args ...)))))
  ;    (foo 1 2 3 4 5)))
)