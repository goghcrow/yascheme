;;; R7RS test suite.  Covers all procedures and syntax in the small
;;; language except `delete-file'.  Currently assumes full-unicode
;;; support, the full numeric tower and all standard libraries
;;; provided.

(test-begin
  "4.1 Primitive expression types"
  (let ()
    (define x 28)
    (check-equal? 28 x))

  (check-equal? 'a (quote a))
  (check-equal? #(a b c) (quote #(a b c)))
  (check-equal? '(+ 1 2) (quote (+ 1 2)))

  (check-equal? 'a 'a)
  (check-equal? #(a b c) '#(a b c))
  (check-equal? '() '())
  (check-equal? '(+ 1 2) '(+ 1 2))
  (check-equal? '(quote a) '(quote a))
  (check-equal? '(quote a) ''a)

  (check-equal? "abc" '"abc")
  (check-equal? "abc" "abc")
  (check-equal? 145932 '145932)
  (check-equal? 145932 145932)
  (check-equal? #t '#t)
  (check-equal? #t #t)

  (check-equal? 7 (+ 3 4))
  (check-equal? 12 ((if #f + *) 3 4))

  (check-equal? 8 ((lambda (x) (+ x x)) 4))
  (define reverse-subtract
    (lambda (x y) (- y x)))
  (check-equal? 3 (reverse-subtract 7 10))
  (define add4
    (let ((x 4))
      (lambda (y) (+ x y))))
  (check-equal? 10 (add4 6))

  (check-equal? '(3 4 5 6) ((lambda x x) 3 4 5 6))
  (check-equal? '(5 6) ((lambda (x y . z) z)
                 3 4 5 6))

  (check-equal? 'yes (if (> 3 2) 'yes 'no))
  (check-equal? 'no (if (> 2 3) 'yes 'no))
  (check-equal? 1 (if (> 3 2)
            (- 3 2)
            (+ 3 2)))
  (let ()
    (define x 2)
    (check-equal? 3 (+ x 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "4.2 Derived expression types"
  (check-equal? 'greater
    (cond ((> 3 2) 'greater)
      ((< 3 2) 'less)))

  (check-equal? 'equal
    (cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal)))

  (check-equal? 2
    (cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else #f)))

  (check-equal? 'composite
    (case (* 2 3)
      ((2 3 5 7) 'prime)
      ((1 4 6 8 9) 'composite)))

  (check-equal? 'c
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else => (lambda (x) x))))

  (check-equal? '((other . z) (semivowel . y) (other . x)
           (semivowel . w) (vowel . u))
    (map (lambda (x)
           (case x
             ((a e i o u) => (lambda (w) (cons 'vowel w)))
             ((w y) (cons 'semivowel x))
             (else => (lambda (w) (cons 'other w)))))
        '(z y x w u)))

  (check-equal? #t (and (= 2 2) (> 2 1)))
  (check-equal? #f (and (= 2 2) (< 2 1)))
  (check-equal? '(f g) (and 1 2 'c '(f g)))
  (check-equal? #t (and))

  (check-equal? #t (or (= 2 2) (> 2 1)))
  (check-equal? #t (or (= 2 2) (< 2 1)))
  (check-equal? #f (or #f #f #f))
  (check-equal? '(b c) (or (memq 'b '(a b c))
                 (/ 3 0)))

  (check-equal? 6 (let ((x 2) (y 3))
            (* x y)))

  (check-equal? 35 (let ((x 2) (y 3))
             (let ((x 7)
                    (z (+ x y)))
               (* z x))))

  (check-equal? 70 (let ((x 2) (y 3))
             (let* ((x 7)
                     (z (+ x y)))
               (* z x))))

  (check-equal? #t
    (letrec ((even?
               (lambda (n)
                 (if (zero? n)
                   #t
                   (odd? (- n 1)))))
              (odd?
                (lambda (n)
                  (if (zero? n)
                    #f
                    (even? (- n 1))))))
      (even? 88)))

  (check-equal? 5
    (letrec* ((p
                (lambda (x)
                  (+ 1 (q (- x 1)))))
               (q
                 (lambda (y)
                   (if (zero? y)
                     0
                     (+ 1 (p (- y 1))))))
               (x (p 5))
               (y x))
      y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "4.2 Derived expression types"

  ;; By Jussi Piitulainen <jpiitula@ling.helsinki.fi>
  ;; and John Cowan <cowan@mercury.ccil.org>:
  ;; http://lists.scheme-reports.org/pipermail/scheme-reports/2013-December/003876.html
  (define (means ton)
    (letrec*
      ((mean
         (lambda (f g)
           (f (/ (sum g ton) n))))
        (sum
          (lambda (g ton)
            (if (null? ton)
              (+)
              (if (number? ton)
                (g ton)
                (+ (sum g (car ton))
                  (sum g (cdr ton)))))))
        (n (sum (lambda (x) 1) ton)))
      (values (mean values values)
        (mean exp log)
        (mean / /))))
  (let*-values (((a b c) (means '(8 5 99 1 22))))
    (check-equal? 27 a)
    ;(check-equal? 9.728 b)
    (check-equal? #t (< (abs (- b 9.728)) 0.00001))
    ;(check-equal? 1800/497 c)
    (check-equal? (/ 1800 497) c))

  (let*-values (((root rem) (exact-integer-sqrt 32)))
    (check-equal? 35 (* root rem)))

  ;(check-equal? '(1073741824 0)
  ;  (let*-values (((root rem) (exact-integer-sqrt (expt 2 60))))
  ;    (list root rem)))
  ;
  ;(check-equal? '(1518500249 3000631951)
  ;  (let*-values (((root rem) (exact-integer-sqrt (expt 2 61))))
  ;    (list root rem)))
  ;
  ;(check-equal? '(815238614083298888 443242361398135744)
  ;  (let*-values (((root rem) (exact-integer-sqrt (expt 2 119))))
  ;    (list root rem)))
  ;
  ;(check-equal? '(1152921504606846976 0)
  ;  (let*-values (((root rem) (exact-integer-sqrt (expt 2 120))))
  ;    (list root rem)))
  ;
  ;(check-equal? '(1630477228166597776 1772969445592542976)
  ;  (let*-values (((root rem) (exact-integer-sqrt (expt 2 121))))
  ;    (list root rem)))
  ;
  ;(check-equal? '(31622776601683793319 62545769258890964239)
  ;  (let*-values (((root rem) (exact-integer-sqrt (expt 10 39))))
  ;    (list root rem)))
  ;
  ;(let*-values (((root rem) (exact-integer-sqrt (expt 2 140))))
  ;  (check-equal? 0 rem)
  ;  (check-equal? (expt 2 140) (square root)))
  ;
  (check-equal? '(x y x y) (let ((a 'a) (b 'b) (x 'x) (y 'y))
                             (let*-values (((a b) (values x y))
                                            ((x y) (values a b)))
                               (list a b x y))))

  (check-equal? 'ok (let-values () 'ok))

  (check-equal? 1 (let ((x 1))
                    (let*-values ()
                      (define x 2)
                      #f)
                    x))

  (let ()
    (define x 0)
    (set! x 5)
    (check-equal? 6 (+ x 1)))

  (check-equal? #(0 1 2 3 4) (do ((vec (make-vector 5))
                                   (i 0 (+ i 1)))
                               ((= i 5) vec)
                               (vector-set! vec i i)))

  (check-equal? 25 (let ((x '(1 3 5 7 9)))
                     (do ((x x (cdr x))
                           (sum 0 (+ sum (car x))))
                       ((null? x) sum))))

  (check-equal? '((6 1 3) (-5 -2))
    (let loop ((numbers '(3 -2 1 6 -5))
                (nonneg '())
                (neg '()))
      (cond ((null? numbers) (list nonneg neg))
        ((>= (car numbers) 0)
          (loop (cdr numbers)
            (cons (car numbers) nonneg)
            neg))
        ((< (car numbers) 0)
          (loop (cdr numbers)
            nonneg
            (cons (car numbers) neg))))))

  (check-equal? 3 (force (delay (+ 1 2))))

  (check-equal? '(3 3)
    (let ((p (delay (+ 1 2))))
      (list (force p) (force p))))

  (define integers
    (letrec ((next
               (lambda (n)
                 (delay (cons n (next (+ n 1)))))))
      (next 0)))
  (define head
    (lambda (stream) (car (force stream))))
  (define tail
    (lambda (stream) (cdr (force stream))))

  (check-equal? 2 (head (tail (tail integers))))

  (define (stream-filter p? s)
    (delay-force
      (if (null? (force s))
        (delay '())
        (let ((h (car (force s)))
               (t (cdr (force s))))
          (if (p? h)
            (delay (cons h (stream-filter p? t)))
            (stream-filter p? t))))))

  (check-equal? 5 (head (tail (tail (stream-filter odd? integers)))))

  (let ()
    (define x 5)
    (define count 0)
    (define p
      (delay (begin (set! count (+ count 1))
               (if (> count x)
                 count
                 (force p)))))
    (check-equal? 6 (force p))
    (check-equal? 6 (begin (set! x 10) (force p))))

  ;(check-equal? #t (promise? (delay (+ 2 2))))
  ;(check-equal? #t (promise? (make-promise (+ 2 2))))
  ;(check-equal? #t
  ;  (let ((x (delay (+ 2 2))))
  ;    (force x)
  ;    (promise? x)))
  ;(check-equal? #t
  ;  (let ((x (make-promise (+ 2 2))))
  ;    (force x)
  ;    (promise? x)))
  ;(check-equal? 4 (force (make-promise (+ 2 2))))
  ;(check-equal? 4 (force (make-promise (make-promise (+ 2 2)))))

  ;(define radix
  ;  (make-parameter
  ;    10
  ;    (lambda (x)
  ;      (if (and (integer? x) (<= 2 x 16))
  ;        x
  ;        (error "invalid radix")))))
  ;(define (f n) (number->string n (radix)))
  ;(check-equal? "12" (f 12))
  ;(check-equal? "1100" (parameterize ((radix 2))
  ;               (f 12)))
  ;(check-equal? "12" (f 12))

  (check-equal? '(list 3 4) `(list ,(+ 1 2) 4))
  (let ((name 'a)) (check-equal? '(list a (quote a)) `(list ,name ',name)))
  (check-equal? '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
  (check-equal? #(10 5 4 16 9 8)
      `#(10 5 ,(square 2) ,@(map square '(4 3)) 8))
  (check-equal? '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
      `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) )
  (let ((name1 'x)
         (name2 'y))
    (check-equal? '(a `(b ,x ,'y d) e) `(a `(b ,,name1 ,',name2 d) e)))
  (check-equal? '(list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)) )
  (check-equal? `(list ,(+ 1 2) 4) (quasiquote (list (unquote (+ 1 2)) 4)))

  (define plus
    (case-lambda
      (() 0)
      ((x) x)
      ((x y) (+ x y))
      ((x y z) (+ (+ x y) z))
      (args (apply + args))))

  (check-equal? 0 (plus))
  (check-equal? 1 (plus 1))
  (check-equal? 3 (plus 1 2))
  (check-equal? 6 (plus 1 2 3))
  (check-equal? 10 (plus 1 2 3 4))

  (define mult
    (case-lambda
      (() 1)
      ((x) x)
      ((x y) (* x y))
      ((x y . z) (apply mult (* x y) z))))

  (check-equal? 1 (mult))
  (check-equal? 1 (mult 1))
  (check-equal? 2 (mult 1 2))
  (check-equal? 6 (mult 1 2 3))
  (check-equal? 24 (mult 1 2 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "4.3 Macros"

  (check-equal? 'now (let-syntax
                       ((when
                          (lambda (stx)
                            (let-values ([(m) (match-syntax stx '(when test stmt1 stmt2 ...))])
                              (datum->syntax #'here
                                `(if ,(m 'test)
                                  (begin ,(m 'stmt1)
                                    ,@(m 'stmt2))
                                   (void)
                                 ))))))
                       (let ((if #t))
                         (when if (set! if 'now))
                         if)))

(check-equal? 'outer (let ((x 'outer))
               (let-syntax ((m (lambda (stx) #'x)))
                 (let ((x 'inner))
                   (m)))))
;
;(check-equal? 7 (letrec-syntax
;          ((my-or (syntax-rules ()
;                    ((my-or) #f)
;                    ((my-or e) e)
;                    ((my-or e1 e2 ...)
;                      (let ((temp e1))
;                        (if temp
;                          temp
;                          (my-or e2 ...)))))))
;          (let ((x #f)
;                 (y 7)
;                 (temp 8)
;                 (let odd?)
;                 (if even?))
;            (my-or x
;              (let temp)
;              (if y)
;              y))))
;
;(define-syntax be-like-begin1
;  (syntax-rules ()
;    ((be-like-begin1 name)
;      (define-syntax name
;        (syntax-rules ()
;          ((name expr (... ...))
;            (begin expr (... ...))))))))
;(be-like-begin1 sequence1)
;(check-equal? 3 (sequence1 0 1 2 3))
;
;(define-syntax be-like-begin2
;  (syntax-rules ()
;    ((be-like-begin2 name)
;      (define-syntax name
;        (... (syntax-rules ()
;               ((name expr ...)
;                 (begin expr ...))))))))
;(be-like-begin2 sequence2)
;(check-equal? 4 (sequence2 1 2 3 4))
;
;(define-syntax be-like-begin3
;  (syntax-rules ()
;    ((be-like-begin3 name)
;      (define-syntax name
;        (syntax-rules dots ()
;          ((name expr dots)
;            (begin expr dots)))))))
;(be-like-begin3 sequence3)
;(check-equal? 5 (sequence3 2 3 4 5))
;
;;; ellipsis escape
;(define-syntax elli-esc-1
;  (syntax-rules ()
;    ((_)
;        '(... ...))
;    ((_ x)
;        '(... (x ...)))
;    ((_ x y)
;        '(... (... x y)))))
;
;(check-equal? '... (elli-esc-1))
;(check-equal? '(100 ...) (elli-esc-1 100))
;(check-equal? '(... 100 200) (elli-esc-1 100 200))
;
;;; Syntax pattern with ellipsis in middle of proper list.
;(define-syntax part-2
;  (syntax-rules ()
;    ((_ a b (m n) ... x y)
;      (vector (list a b) (list m ...) (list n ...) (list x y)))
;    ((_ . rest) 'error)))
;(check-equal? '#((10 43) (31 41 51) (32 42 52) (63 77))
;  (part-2 10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77))
;;; Syntax pattern with ellipsis in middle of improper list.
;(define-syntax part-2x
;  (syntax-rules ()
;    ((_ (a b (m n) ... x y . rest))
;      (vector (list a b) (list m ...) (list n ...) (list x y)
;        (cons "rest:" 'rest)))
;    ((_ . rest) 'error)))
;(check-equal? '#((10 43) (31 41 51) (32 42 52) (63 77) ("rest:"))
;  (part-2x (10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77)))
;(check-equal? '#((10 43) (31 41 51) (32 42 52) (63 77) ("rest:" . "tail"))
;  (part-2x (10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77 . "tail")))
;
;;; underscore
;(define-syntax underscore
;  (syntax-rules ()
;    ((foo _) '_)))
;(check-equal? '_ (underscore foo))
;
;(let ()
;  (define-syntax underscore2
;    (syntax-rules ()
;      ((underscore2 (a _) ...) 42)))
;  (check-equal? 42 (underscore2 (1 2))))
;
;(define-syntax count-to-2
;  (syntax-rules ()
;    ((_) 0)
;    ((_ _) 1)
;    ((_ _ _) 2)
;    ((_ . _) 'many)))
;(check-equal? '(2 0 many)
;  (list (count-to-2 a b) (count-to-2) (count-to-2 a b c d)))
;
;(define-syntax count-to-2_
;  (syntax-rules (_)
;    ((_) 0)
;    ((_ _) 1)
;    ((_ _ _) 2)
;    ((x . y) 'fail)))
;(check-equal? '(2 0 fail fail)
;  (list (count-to-2_ _ _) (count-to-2_)
;    (count-to-2_ a b) (count-to-2_ a b c d)))
;
;(define-syntax jabberwocky
;  (syntax-rules ()
;    ((_ hatter)
;      (begin
;        (define march-hare 42)
;        (define-syntax hatter
;          (syntax-rules ()
;            ((_) march-hare)))))))
;(jabberwocky mad-hatter)
;(check-equal? 42 (mad-hatter))
;
;(check-equal? 'ok (let ((=> #f)) (cond (#t => 'ok))))
;
;(let ()
;  (define x 1)
;  (let-syntax ()
;    (define x 2)
;    #f)
;  (check-equal? 1 x))
;
;(let ()
;  (define-syntax foo
;    (syntax-rules ()
;      ((foo bar y)
;        (define-syntax bar
;          (syntax-rules ()
;            ((bar x) 'y))))))
;  (foo bar x)
;  (check-equal? 'x (bar 1)))
;
;(begin
;  (define-syntax ffoo
;    (syntax-rules ()
;      ((ffoo ff)
;        (begin
;          (define (ff x)
;            (gg x))
;          (define (gg x)
;            (* x x))))))
;  (ffoo ff)
;  (check-equal? 100 (ff 10)))
;
;(let-syntax ((vector-lit
;               (syntax-rules ()
;                 ((vector-lit)
;                     '#(b)))))
;  (check-equal? '#(b) (vector-lit)))
;
;(let ()
;  ;; forward hygienic refs
;  (define-syntax foo399
;    (syntax-rules () ((foo399) (bar399))))
;  (define (quux399)
;    (foo399))
;  (define (bar399)
;    42)
;  (check-equal? 42 (quux399)))
;
;(let-syntax
;  ((m (syntax-rules ()
;        ((m x) (let-syntax
;                 ((n (syntax-rules (k)
;                       ((n x) 'bound-identifier=?)
;                       ((n y) 'free-identifier=?))))
;                 (n z))))))
;  (check-equal? 'bound-identifier=? (m k)))
;
;;; literal has priority to ellipsis (R7RS 4.3.2)
;(let ()
;  (define-syntax elli-lit-1
;    (syntax-rules ... (...)
;      ((_ x)
;          '(x ...))))
;  (check-equal? '(100 ...) (elli-lit-1 100)))
;
;;; bad ellipsis
;#|
;(check-equal? 'error
;      (guard (exn (else 'error))
;        (eval
;         '(define-syntax bad-elli-1
;            (syntax-rules ()
;              ((_ ... x)
;               '(... x))))
;         (interaction-environment))))
;
;(check-equal? 'error
;      (guard (exn (else 'error))
;        (eval
;         '(define-syntax bad-elli-2
;            (syntax-rules ()
;              ((_ (... x))
;               '(... x))))
;         (interaction-environment))))
;|#
;
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "5 Program structure"

  (define add3
    (lambda (x) (+ x 3)))
  (check-equal? 6 (add3 3))
  (define first car)
  (check-equal? 1 (first '(1 2)))

  (check-equal? 45 (let ((x 5))
                     (define foo (lambda (y) (bar x y)))
                     (define bar (lambda (a b) (+ (* a b) a)))
                     (foo (+ x 3))))

  (check-equal? 'ok
    (let ()
      (define-values () (values))
        'ok))
  (check-equal? 1
    (let ()
      (define-values (x) (values 1))
      x))
;  (check-equal? 3
;    (let ()
;      (define-values x (values 1 2))
;      (apply + x)))
  (check-equal? 3
    (let ()
      (define-values (x y) (values 1 2))
      (+ x y)))
  (check-equal? 6
    (let ()
      (define-values (x y z) (values 1 2 3))
      (+ x y z)))
;  (check-equal? 10
;    (let ()
;      (define-values (x y . z) (values 1 2 3 4))
;      (+ x y (car z) (cadr z))))

;  (check-equal? '(2 1) (let ((x 1) (y 2))
;                         (define-syntax swap!
;                           (syntax-rules ()
;                             ((swap! a b)
;                               (let ((tmp a))
;                                 (set! a b)
;                                 (set! b tmp)))))
;                         (swap! x y)
;                         (list x y)))

  ;; Records

;  (define-record-type <pare>
;    (kons x y)
;    pare?
;    (x kar set-kar!)
;    (y kdr))
;
;  (check-equal? #t (pare? (kons 1 2)))
;  (check-equal? #f (pare? (cons 1 2)))
;  (check-equal? 1 (kar (kons 1 2)))
;  (check-equal? 2 (kdr (kons 1 2)))
;  (check-equal? 3 (let ((k (kons 1 2)))
;                    (set-kar! k 3)
;                    (kar k)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6 Standard Procedures

(test-begin "6.1 Equivalence Predicates"
  (check-equal? #t (eqv? 'a 'a))
  (check-equal? #f (eqv? 'a 'b))
  (check-equal? #t (eqv? 2 2))
  (check-equal? #t (eqv? '() '()))
  (check-equal? #t (eqv? 100000000 100000000))
  (check-equal? #f (eqv? (cons 1 2) (cons 1 2)))
  (check-equal? #f (eqv? (lambda () 1)
                     (lambda () 2)))
  (check-equal? #f (eqv? #f 'nil))

  (define gen-counter
    (lambda ()
      (let ((n 0))
        (lambda () (set! n (+ n 1)) n))))
  (check-equal? #t
    (let ((g (gen-counter)))
      (eqv? g g)))
  (check-equal? #f (eqv? (gen-counter) (gen-counter)))
  (define gen-loser
    (lambda ()
      (let ((n 0))
        (lambda () (set! n (+ n 1)) 27))))
  (check-equal? #t (let ((g (gen-loser)))
                     (eqv? g g)))

  (check-equal? #f
    (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
              (g (lambda () (if (eqv? f g) 'g 'both))))
      (eqv? f g)))

  (check-equal? #t
    (let ((x '(a)))
      (eqv? x x)))

  (check-equal? #t (eq? 'a 'a))
  (check-equal? #f (eq? (list 'a) (list 'a)))
  (check-equal? #t (eq? '() '()))
  (check-equal? #t
    (let ((x '(a)))
      (eq? x x)))
  (check-equal? #t
    (let ((x '#()))
      (eq? x x)))
  (check-equal? #t
    (let ((p (lambda (x) x)))
      (eq? p p)))

  (check-equal? #t (equal? 'a 'a))
  (check-equal? #t (equal? '(a) '(a)))
  (check-equal? #t (equal? '(a (b) c)
                       '(a (b) c)))
  (check-equal? #t (equal? "abc" "abc"))
  (check-equal? #t (equal? 2 2))
  (check-equal? #t (equal? (make-vector 5 'a)
                     (make-vector 5 'a))))

(test-begin "6.2 Numbers"

;(check-equal? #t (complex? 3+4i))
;(check-equal? #t (complex? 3))
;(check-equal? #t (real? 3))
;(check-equal? #t (real? -2.5+0i))
;(check-equal? #f (real? -2.5+0.0i))
;(check-equal? #t (real? #e1e10))
;(check-equal? #t (real? +inf.0))
;(check-equal? #f (rational? -inf.0))
;(check-equal? #f (rational? +nan.0))
;(check-equal? #t (rational? 9007199254740991.0))
;(check-equal? #t (rational? 9007199254740992.0))
;(check-equal? #t (rational? 1.7976931348623157e308))
;(check-equal? #t (rational? 6/10))
;(check-equal? #t (rational? 6/3))
;(check-equal? #t (integer? 3+0i))
;(check-equal? #t (integer? 3.0))
;(check-equal? #t (integer? 8/4))
;
;(check-equal? #f (exact? 3.0))
;(check-equal? #t (exact? #e3.0))
;(check-equal? #t (inexact? 3.))
;
;(check-equal? #t (exact-integer? 32))
;(check-equal? #f (exact-integer? 32.0))
;(check-equal? #f (exact-integer? 32/5))
;
;(check-equal? #t (finite? 3))
;(check-equal? #f (finite? +inf.0))
;(check-equal? #f (finite? 3.0+inf.0i))
;
;(check-equal? #f (infinite? 3))
;(check-equal? #t (infinite? +inf.0))
;(check-equal? #f (infinite? +nan.0))
;(check-equal? #t (infinite? 3.0+inf.0i))
;
;(check-equal? #t (nan? +nan.0))
;(check-equal? #f (nan? 32))
;;; (check-equal? #t (nan? +nan.0+5.0i))
;(check-equal? #f (nan? 1+2i))
;
;(check-equal? #t (= 1 1.0 1.0+0.0i))
;(check-equal? #f (= 1.0 1.0+1.0i))
;(check-equal? #t (< 1 2 3))
;(check-equal? #f (< 1 1 2))
;(check-equal? #t (> 3.0 2.0 1.0))
;(check-equal? #f (> -3.0 2.0 1.0))
;(check-equal? #t (<= 1 1 2))
;(check-equal? #f (<= 1 2 1))
;(check-equal? #t (>= 2 1 1))
;(check-equal? #f (>= 1 2 1))
;(check-equal? #f (< +nan.0 0))
;(check-equal? #f (> +nan.0 0))
;(check-equal? #f (< +nan.0 0.0))
;(check-equal? #f (> +nan.0 0.0))
;(check-equal? '(#t #f) (list (<= 1 1 2) (<= 2 1 3)))
;
;;; From R7RS 6.2.6 Numerical operations:
;;;
;;; These predicates are required to be transitive.
;;;
;;; _Note:_ The traditional implementations of these predicates in
;;; Lisp-like languages, which involve converting all arguments to inexact
;;; numbers if any argument is inexact, are not transitive.
;
;;; Example from Alan Bawden
;(let ((a (- (expt 2 1000) 1))
;       (b (inexact (expt 2 1000))) ; assuming > single-float-epsilon
;       (c (+ (expt 2 1000) 1)))
;  (check-equal? #t (if (and (= a b) (= b c))
;             (= a c)
;             #t)))
;
;;; From CLtL 12.3. Comparisons on Numbers:
;;;
;;;  Let _a_ be the result of (/ 10.0 single-float-epsilon), and let
;;;  _j_ be the result of (floor a). ..., all of (<= a j), (< j (+ j
;;;  1)), and (<= (+ j 1) a) would be true; transitivity would then
;;;  imply that (< a a) ought to be true ...
;
;;; Transliteration from Jussi Piitulainen
;(define single-float-epsilon
;  (do ((eps 1.0 (* eps 2.0)))
;    ((= eps (+ eps 1.0)) eps)))
;
;(let* ((a (/ 10.0 single-float-epsilon))
;        (j (exact a)))
;  (check-equal? #t (if (and (<= a j) (< j (+ j 1)))
;             (not (<= (+ j 1) a))
;             #t)))

(check-equal? #t (zero? 0))
(check-equal? #t (zero? 0.0))
;(check-equal? #t (zero? 0.0+0.0i))
(check-equal? #f (zero? 1))
(check-equal? #f (zero? -1))

(check-equal? #f (positive? 0))
(check-equal? #f (positive? 0.0))
(check-equal? #t (positive? 1))
(check-equal? #t (positive? 1.0))
(check-equal? #f (positive? -1))
(check-equal? #f (positive? -1.0))
;(check-equal? #t (positive? +inf.0))
;(check-equal? #f (positive? -inf.0))
;(check-equal? #f (positive? +nan.0))

(check-equal? #f (negative? 0))
(check-equal? #f (negative? 0.0))
(check-equal? #f (negative? 1))
(check-equal? #f (negative? 1.0))
(check-equal? #t (negative? -1))
(check-equal? #t (negative? -1.0))
;(check-equal? #f (negative? +inf.0))
;(check-equal? #t (negative? -inf.0))
;(check-equal? #f (negative? +nan.0))

(check-equal? #f (odd? 0))
(check-equal? #t (odd? 1))
(check-equal? #t (odd? -1))
(check-equal? #f (odd? 102))

(check-equal? #t (even? 0))
(check-equal? #f (even? 1))
(check-equal? #t (even? -2))
(check-equal? #t (even? 102))

(check-equal? 3 (max 3))
(check-equal? 4 (max 3 4))
;(check-equal? 4.0 (max 3.9 4))
;(check-equal? 5.0 (max 5 3.9 4))
;(check-equal? +inf.0 (max 100 +inf.0))
(check-equal? 3 (min 3))
(check-equal? 3 (min 3 4))
;(check-equal? 3.0 (min 3 3.1))
;(check-equal? -inf.0 (min -inf.0 -100))

(check-equal? 7 (+ 3 4))
(check-equal? 3 (+ 3))
(check-equal? 0 (+))
(check-equal? 4 (* 4))
(check-equal? 1 (*))

(check-equal? -1 (- 3 4))
(check-equal? -6 (- 3 4 5))
(check-equal? -3 (- 3))
;(check-equal? 3/20 (/ 3 4 5))
;(check-equal? 1/3 (/ 3))

(check-equal? 7 (abs -7))
(check-equal? 7 (abs 7))

;(test-values (values 2 1) (floor/ 5 2))
;(test-values (values -3 1) (floor/ -5 2))
;(test-values (values -3 -1) (floor/ 5 -2))
;(test-values (values 2 -1) (floor/ -5 -2))
;(test-values (values 2 1) (truncate/ 5 2))
;(test-values (values -2 -1) (truncate/ -5 2))
;(test-values (values -2 1) (truncate/ 5 -2))
;(test-values (values 2 -1) (truncate/ -5 -2))
;(test-values (values 2.0 -1.0) (truncate/ -5.0 -2))

(check-equal? 1 (modulo 13 4))
(check-equal? 1 (remainder 13 4))

(check-equal? 3 (modulo -13 4))
(check-equal? -1 (remainder -13 4))

(check-equal? -3 (modulo 13 -4))
(check-equal? 1 (remainder 13 -4))

(check-equal? -1 (modulo -13 -4))
(check-equal? -1 (remainder -13 -4))

(check-equal? -1.0 (remainder -13 -4.0))

(check-equal? 4 (gcd 32 -36))
(check-equal? 0 (gcd))
(check-equal? 288 (lcm 32 -36))
;(check-equal? 288.0 (lcm 32.0 -36))
(check-equal? 1 (lcm))

;(check-equal? 3 (numerator (/ 6 4)))
;(check-equal? 2 (denominator (/ 6 4)))
;(check-equal? 2.0 (denominator (inexact (/ 6 4))))
;(check-equal? 11.0 (numerator 5.5))
;(check-equal? 2.0 (denominator 5.5))
;(check-equal? 5.0 (numerator 5.0))
;(check-equal? 1.0 (denominator 5.0))
;
;(check-equal? -5.0 (floor -4.3))
(check-equal? -4.0 (ceiling -4.3))
;(check-equal? -4.0 (truncate -4.3))
;(check-equal? -4.0 (round -4.3))
;
(check-equal? 3.0 (floor 3.5))
(check-equal? 4.0 (ceiling 3.5))
;(check-equal? 3.0 (truncate 3.5))
;(check-equal? 4.0 (round 3.5))

;(check-equal? 4 (round 7/2))
(check-equal? 7 (round 7))
;(check-equal? 1 (round 7/10))
;(check-equal? -4 (round -7/2))
(check-equal? -7 (round -7))
;(check-equal? -1 (round -7/10))

;(check-equal? 1/3 (rationalize (exact .3) 1/10))
;(check-equal? #i1/3 (rationalize .3 1/10))
;
;(check-equal? 1.0 (inexact (exp 0))) ;; may return exact number
;(check-equal? 20.0855369231877 (exp 3))
;
;(check-equal? 0.0 (inexact (log 1))) ;; may return exact number
(check-equal? 1.0 (log (exp 1)))
(check-equal? 42.0 (log (exp 42)))
(check-equal? 2.0 (log 100 10))
(check-equal? 12.0 (log 4096 2))

;(check-equal? 0.0 (inexact (sin 0))) ;; may return exact number
;(check-equal? 1.0 (sin 1.5707963267949))
;(check-equal? 1.0 (inexact (cos 0))) ;; may return exact number
;(check-equal? -1.0 (cos 3.14159265358979))
;(check-equal? 0.0 (inexact (tan 0))) ;; may return exact number
;(check-equal? 1.5574077246549 (tan 1))
;
;(check-equal? 0.0 (inexact (asin 0))) ;; may return exact number
;(check-equal? 1.5707963267949 (asin 1))
;(check-equal? 0.0 (inexact (acos 1))) ;; may return exact number
;(check-equal? 3.14159265358979 (acos -1))
;
;;; (check-equal? 0.0-0.0i (asin 0+0.0i))
;;; (check-equal? 1.5707963267948966+0.0i (acos 0+0.0i))
;
;(check-equal? 0.0 (atan 0.0 1.0))
;(check-equal? -0.0 (atan -0.0 1.0))
;(check-equal? 0.785398163397448 (atan 1.0 1.0))
;(check-equal? 1.5707963267949 (atan 1.0 0.0))
;(check-equal? 2.35619449019234 (atan 1.0 -1.0))
;(check-equal? 3.14159265358979 (atan 0.0 -1.0))
;(check-equal? -3.14159265358979 (atan -0.0 -1.0)) ;
;(check-equal? -2.35619449019234 (atan -1.0 -1.0))
;(check-equal? -1.5707963267949 (atan -1.0 0.0))
;(check-equal? -0.785398163397448 (atan -1.0 1.0))
;;; (check-equal? undefined (atan 0.0 0.0))

(check-equal? 1764 (square 42))
(check-equal? 4 (square 2))

;(check-equal? 3.0 (inexact (sqrt 9)))
;(check-equal? 1.4142135623731 (sqrt 2))
;(check-equal? 0.0+1.0i (inexact (sqrt -1)))
;(check-equal? 0.0+1.0i (sqrt -1.0-0.0i))

(check-equal? '(2 0) (call-with-values (lambda () (exact-integer-sqrt 4)) list))
(check-equal? '(2 1) (call-with-values (lambda () (exact-integer-sqrt 5)) list))

(check-equal? 27 (expt 3 3))
(check-equal? 1 (expt 0 0))
(check-equal? 0 (expt 0 1))
;(check-equal? 1.0 (expt 0.0 0))
;(check-equal? 0.0 (expt 0 1.0))

;(check-equal? 1+2i (make-rectangular 1 2))
;
;(check-equal? 0.54030230586814+0.841470984807897i (make-polar 1 1))
;
;(check-equal? 1 (real-part 1+2i))
;
;(check-equal? 2 (imag-part 1+2i))
;
;(check-equal? 2.23606797749979 (magnitude 1+2i))
;
;(check-equal? 1.10714871779409 (angle 1+2i))
;
;(check-equal? 1.0 (inexact 1))
;(check-equal? #t (inexact? (inexact 1)))
;(check-equal? 1 (exact 1.0))
;(check-equal? #t (exact? (exact 1.0)))

(check-equal? 100 (string->number "100"))
(check-equal? 256 (string->number "100" 16))
(check-equal? 100.0 (string->number "1e2"))

)

(test-begin "6.3 Booleans"

  (check-equal? #t #t)
  (check-equal? #f #f)
  (check-equal? #f '#f)

  (check-equal? #f (not #t))
  (check-equal? #f (not 3))
  (check-equal? #f (not (list 3)))
  (check-equal? #t (not #f))
  (check-equal? #f (not '()))
  (check-equal? #f (not (list)))
  (check-equal? #f (not 'nil))

  (check-equal? #t (boolean? #f))
  (check-equal? #f (boolean? 0))
  (check-equal? #f (boolean? '()))

  (check-equal? #t (boolean=? #t #t))
  (check-equal? #t (boolean=? #f #f))
  (check-equal? #f (boolean=? #t #f))
  (check-equal? #t (boolean=? #f #f #f))
  (check-equal? #f (boolean=? #t #t #f)))

(test-begin "6.4 Lists"

;  (let* ((x (list 'a 'b 'c))
;          (y x))
;    (check-equal? '(a b c) (values y))
;    (check-equal? #t (list? y))
;    (set-cdr! x 4)
;    (check-equal? '(a . 4) (values x))
;    (check-equal? #t (eqv? x y))
;    (check-equal? #f (list? y))
;    (set-cdr! x x)
;    (check-equal? #f (list? x)))

  (check-equal? #t (pair? '(a . b)))
  (check-equal? #t (pair? '(a b c)))
  (check-equal? #f (pair? '()))
  (check-equal? #f (pair? '#(a b)))

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
  (define (g) '(constant-list))

  (check-equal? #t (list? '(a b c)))
  (check-equal? #t (list? '()))
  (check-equal? #f (list? '(a . b)))
;  (check-equal? #f (let ((x (list 'a))) (set-cdr! x x) (list? x)))

  (check-equal? '(3 3) (make-list 2 3))

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

  (check-equal? '(d e) (list-tail '(a b c d e) 3))

  (check-equal? 'c (list-ref '(a b c d) 2))
;  (check-equal? 'c (list-ref '(a b c d)
;                     (exact (round 1.8))))

  (check-equal? '(0 ("Sue" "Sue") "Anna")
    (let ((lst (list 0 '(2 2 2 2) "Anna")))
      (list-set! lst 1 '("Sue" "Sue"))
      lst))

  (check-equal? '(a b c) (memq 'a '(a b c)))
  (check-equal? '(b c) (memq 'b '(a b c)))
  (check-equal? #f (memq 'a '(b c d)))
  (check-equal? #f (memq (list 'a) '(b (a) c)))
  (check-equal? '((a) c) (member (list 'a) '(b (a) c)))
;  (check-equal? '("b" "c") (member "B" '("a" "b" "c") string-ci=?))
  (check-equal? '(101 102) (memv 101 '(100 101 102)))

  (let ()
    (define e '((a 1) (b 2) (c 3)))
    (check-equal? '(a 1) (assq 'a e))
    (check-equal? '(b 2) (assq 'b e))
    (check-equal? #f (assq 'd e)))

  (check-equal? #f (assq (list 'a) '(((a)) ((b)) ((c)))))
  (check-equal? '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
  (check-equal? '(2 4) (assoc 2.0 '((1 1) (2 4) (3 9)) =))
  (check-equal? '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

  (check-equal? '(1 2 3) (list-copy '(1 2 3)))
  (check-equal? "foo" (list-copy "foo"))
  (check-equal? '() (list-copy '()))
  (check-equal? '(3 . 4) (list-copy '(3 . 4)))
  (check-equal? '(6 7 8 . 9) (list-copy '(6 7 8 . 9)))
  (let* ((l1 '((a b) (c d) e))
          (l2 (list-copy l1)))
    (check-equal? l2 '((a b) (c d) e))
    (check-equal? #t (eq? (car l1) (car l2)))
    (check-equal? #t (eq? (cadr l1) (cadr l2)))
    (check-equal? #f (eq? (cdr l1) (cdr l2)))
    (check-equal? #f (eq? (cddr l1) (cddr l2)))))

(test-begin "6.5 Symbols"

(check-equal? #t (symbol? 'foo))
(check-equal? #t (symbol? (car '(a b))))
(check-equal? #f (symbol? "bar"))
(check-equal? #t (symbol? 'nil))
(check-equal? #f (symbol? '()))
(check-equal? #f (symbol? #f))
;
(check-equal? #t (symbol=? 'a 'a))
(check-equal? #f (symbol=? 'a 'A))
(check-equal? #t (symbol=? 'a 'a 'a))
(check-equal? #f (symbol=? 'a 'a 'A))

(check-equal? "flying-fish"
  (symbol->string 'flying-fish))
(check-equal? "Martin" (symbol->string 'Martin))
(check-equal? "Malvina" (symbol->string (string->symbol "Malvina")))

(check-equal? 'mISSISSIppi (string->symbol "mISSISSIppi"))
(check-equal? #t (eq? 'bitBlt (string->symbol "bitBlt")))
(check-equal? #t (eq? 'LollyPop (string->symbol (symbol->string 'LollyPop))))
(check-equal? #t (string=? "K. Harper, M.D."
           (symbol->string (string->symbol "K. Harper, M.D.")))))
;
(test-begin "6.6 Characters"

;(check-equal? #t (char? #\a))
;(check-equal? #f (char? "a"))
;(check-equal? #f (char? 'a))
;(check-equal? #f (char? 0))
;
;(check-equal? #t (char=? #\a #\a #\a))
;(check-equal? #f (char=? #\a #\A))
;(check-equal? #t (char<? #\a #\b #\c))
;(check-equal? #f (char<? #\a #\a))
;(check-equal? #f (char<? #\b #\a))
;(check-equal? #f (char>? #\a #\b))
;(check-equal? #f (char>? #\a #\a))
;(check-equal? #t (char>? #\c #\b #\a))
;(check-equal? #t (char<=? #\a #\b #\b))
;(check-equal? #t (char<=? #\a #\a))
;(check-equal? #f (char<=? #\b #\a))
;(check-equal? #f (char>=? #\a #\b))
;(check-equal? #t (char>=? #\a #\a))
;(check-equal? #t (char>=? #\b #\b #\a))
;
;(check-equal? #t (char-ci=? #\a #\a))
;(check-equal? #t (char-ci=? #\a #\A #\a))
;(check-equal? #f (char-ci=? #\a #\b))
;(check-equal? #t (char-ci<? #\a #\B #\c))
;(check-equal? #f (char-ci<? #\A #\a))
;(check-equal? #f (char-ci<? #\b #\A))
;(check-equal? #f (char-ci>? #\A #\b))
;(check-equal? #f (char-ci>? #\a #\A))
;(check-equal? #t (char-ci>? #\c #\B #\a))
;(check-equal? #t (char-ci<=? #\a #\B #\b))
;(check-equal? #t (char-ci<=? #\A #\a))
;(check-equal? #f (char-ci<=? #\b #\A))
;(check-equal? #f (char-ci>=? #\A #\b))
;(check-equal? #t (char-ci>=? #\a #\A))
;(check-equal? #t (char-ci>=? #\b #\B #\a))
;
;(check-equal? #t (char-alphabetic? #\a))
;(check-equal? #f (char-alphabetic? #\space))
;(check-equal? #t (char-numeric? #\0))
;(check-equal? #f (char-numeric? #\.))
;(check-equal? #f (char-numeric? #\a))
;(check-equal? #t (char-whitespace? #\space))
;(check-equal? #t (char-whitespace? #\tab))
;(check-equal? #t (char-whitespace? #\newline))
;(check-equal? #f (char-whitespace? #\_))
;(check-equal? #f (char-whitespace? #\a))
;(check-equal? #t (char-upper-case? #\A))
;(check-equal? #f (char-upper-case? #\a))
;(check-equal? #f (char-upper-case? #\3))
;(check-equal? #t (char-lower-case? #\a))
;(check-equal? #f (char-lower-case? #\A))
;(check-equal? #f (char-lower-case? #\3))
;
;(check-equal? #t (char-alphabetic? #\Λ))
;(check-equal? #f (char-alphabetic? #\x0E50))
;(check-equal? #t (char-upper-case? #\Λ))
;(check-equal? #f (char-upper-case? #\λ))
;(check-equal? #f (char-lower-case? #\Λ))
;(check-equal? #t (char-lower-case? #\λ))
;(check-equal? #f (char-numeric? #\Λ))
;(check-equal? #t (char-numeric? #\x0E50))
;(check-equal? #t (char-whitespace? #\x1680))
;
;(check-equal? 0 (digit-value #\0))
;(check-equal? 3 (digit-value #\3))
;(check-equal? 9 (digit-value #\9))
;(check-equal? 4 (digit-value #\x0664))
;(check-equal? 0 (digit-value #\x0AE6))
;(check-equal? #f (digit-value #\.))
;(check-equal? #f (digit-value #\-))
;
;(check-equal? 97 (char->integer #\a))
;(check-equal? #\a (integer->char 97))
;
;(check-equal? #\A (char-upcase #\a))
;(check-equal? #\A (char-upcase #\A))
;(check-equal? #\a (char-downcase #\a))
;(check-equal? #\a (char-downcase #\A))
;(check-equal? #\a (char-foldcase #\a))
;(check-equal? #\a (char-foldcase #\A))
;
;(check-equal? #\Λ (char-upcase #\λ))
;(check-equal? #\Λ (char-upcase #\Λ))
;(check-equal? #\λ (char-downcase #\λ))
;(check-equal? #\λ (char-downcase #\Λ))
;(check-equal? #\λ (char-foldcase #\λ))
;(check-equal? #\λ (char-foldcase #\Λ))

)

(test-begin "6.7 Strings"

(check-equal? #t (string? ""))
(check-equal? #t (string? " "))
(check-equal? #f (string? 'a))
(check-equal? #f (string? #\a))

(check-equal? 3 (string-length (make-string 3)))
(check-equal? "---" (make-string 3 #\-))

(check-equal? "" (string))
(check-equal? "---" (string #\- #\- #\-))
(check-equal? "kitten" (string #\k #\i #\t #\t #\e #\n))

(check-equal? 0 (string-length ""))
(check-equal? 1 (string-length "a"))
(check-equal? 3 (string-length "abc"))

(check-equal? #\a (string-ref "abc" 0))
(check-equal? #\b (string-ref "abc" 1))
(check-equal? #\c (string-ref "abc" 2))

(check-equal? "a-c" (let ((str (string #\a #\b #\c))) (string-set! str 1 #\-) str))

(check-equal? (string #\a #\x1F700 #\c)
  (let ((s (string #\a #\b #\c)))
    (string-set! s 1 #\x1F700)
    s))

(check-equal? #t (string=? "" ""))
(check-equal? #t (string=? "abc" "abc" "abc"))
(check-equal? #f (string=? "" "abc"))
(check-equal? #f (string=? "abc" "aBc"))

(check-equal? #f (string<? "" ""))
(check-equal? #f (string<? "abc" "abc"))
(check-equal? #t (string<? "abc" "abcd" "acd"))
(check-equal? #f (string<? "abcd" "abc"))
(check-equal? #t (string<? "abc" "bbc"))

(check-equal? #f (string>? "" ""))
(check-equal? #f (string>? "abc" "abc"))
(check-equal? #f (string>? "abc" "abcd"))
(check-equal? #t (string>? "acd" "abcd" "abc"))
(check-equal? #f (string>? "abc" "bbc"))

(check-equal? #t (string<=? "" ""))
(check-equal? #t (string<=? "abc" "abc"))
(check-equal? #t (string<=? "abc" "abcd" "abcd"))
(check-equal? #f (string<=? "abcd" "abc"))
(check-equal? #t (string<=? "abc" "bbc"))

(check-equal? #t (string>=? "" ""))
(check-equal? #t (string>=? "abc" "abc"))
(check-equal? #f (string>=? "abc" "abcd"))
(check-equal? #t (string>=? "abcd" "abcd" "abc"))
(check-equal? #f (string>=? "abc" "bbc"))

(check-equal? #t (string-ci=? "" ""))
(check-equal? #t (string-ci=? "abc" "abc"))
(check-equal? #f (string-ci=? "" "abc"))
(check-equal? #t (string-ci=? "abc" "aBc"))
(check-equal? #f (string-ci=? "abc" "aBcD"))

(check-equal? #f (string-ci<? "abc" "aBc"))
(check-equal? #t (string-ci<? "abc" "aBcD"))
(check-equal? #f (string-ci<? "ABCd" "aBc"))

(check-equal? #f (string-ci>? "abc" "aBc"))
(check-equal? #f (string-ci>? "abc" "aBcD"))
(check-equal? #t (string-ci>? "ABCd" "aBc"))

(check-equal? #t (string-ci<=? "abc" "aBc"))
(check-equal? #t (string-ci<=? "abc" "aBcD"))
(check-equal? #f (string-ci<=? "ABCd" "aBc"))

(check-equal? #t (string-ci>=? "abc" "aBc"))
(check-equal? #f (string-ci>=? "abc" "aBcD"))
(check-equal? #t (string-ci>=? "ABCd" "aBc"))

(check-equal? #t (string-ci=? "ΑΒΓ" "αβγ" "αβγ"))
(check-equal? #f (string-ci<? "ΑΒΓ" "αβγ"))
(check-equal? #f (string-ci>? "ΑΒΓ" "αβγ"))
(check-equal? #t (string-ci<=? "ΑΒΓ" "αβγ"))
(check-equal? #t (string-ci>=? "ΑΒΓ" "αβγ"))

;; latin
(check-equal? "ABC" (string-upcase "abc"))
(check-equal? "ABC" (string-upcase "ABC"))
(check-equal? "abc" (string-downcase "abc"))
(check-equal? "abc" (string-downcase "ABC"))
(check-equal? "abc" (string-foldcase "abc"))
(check-equal? "abc" (string-foldcase "ABC"))

;; cyrillic
(check-equal? "ΑΒΓ" (string-upcase "αβγ"))
(check-equal? "ΑΒΓ" (string-upcase "ΑΒΓ"))
(check-equal? "αβγ" (string-downcase "αβγ"))
(check-equal? "αβγ" (string-downcase "ΑΒΓ"))
(check-equal? "αβγ" (string-foldcase "αβγ"))
(check-equal? "αβγ" (string-foldcase "ΑΒΓ"))

;; special cases
(check-equal? "SSA" (string-upcase "ßa"))
(check-equal? "ßa" (string-downcase "ßa"))
(check-equal? "ssa" (string-downcase "SSA"))
(check-equal? "maß" (string-downcase "Maß"))
(check-equal? "mass" (string-foldcase "Maß"))
(check-equal? "İ" (string-upcase "İ"))
;(check-equal? "i\x0307;" (string-downcase "İ"))
;(check-equal? "i\x0307;" (string-foldcase "İ"))
(check-equal? "J̌" (string-upcase "ǰ"))
(check-equal? "ſ" (string-downcase "ſ"))
(check-equal? "s" (string-foldcase "ſ"))

;; context-sensitive (final sigma)
(check-equal? "ΓΛΏΣΣΑ" (string-upcase "γλώσσα"))
(check-equal? "γλώσσα" (string-downcase "ΓΛΏΣΣΑ"))
(check-equal? "γλώσσα" (string-foldcase "ΓΛΏΣΣΑ"))
(check-equal? "ΜΈΛΟΣ" (string-upcase "μέλος"))
(check-equal? #t (and (member (string-downcase "ΜΈΛΟΣ") '("μέλος" "μέλοσ")) #t))
;(check-equal? "μέλοσ" (string-foldcase "ΜΈΛΟΣ"))
(check-equal? #t (and (member (string-downcase "ΜΈΛΟΣ ΕΝΌΣ")
                  '("μέλος ενός" "μέλοσ ενόσ"))
           #t))

(check-equal? "" (substring "" 0 0))
(check-equal? "" (substring "a" 0 0))
(check-equal? "" (substring "abc" 1 1))
(check-equal? "ab" (substring "abc" 0 2))
(check-equal? "bc" (substring "abc" 1 3))

(check-equal? "" (string-append ""))
(check-equal? "" (string-append "" ""))
(check-equal? "abc" (string-append "" "abc"))
(check-equal? "abc" (string-append "abc" ""))
(check-equal? "abcde" (string-append "abc" "de"))
(check-equal? "abcdef" (string-append "abc" "de" "f"))

(check-equal? '() (string->list ""))
(check-equal? '(#\a) (string->list "a"))
(check-equal? '(#\a #\b #\c) (string->list "abc"))
(check-equal? '(#\a #\b #\c) (string->list "abc" 0))
(check-equal? '(#\b #\c) (string->list "abc" 1))
(check-equal? '(#\b #\c) (string->list "abc" 1 3))

(check-equal? "" (list->string '()))
(check-equal? "abc" (list->string '(#\a #\b #\c)))

(check-equal? "" (string-copy ""))
(check-equal? "" (string-copy "" 0))
(check-equal? "" (string-copy "" 0 0))
(check-equal? "abc" (string-copy "abc"))
(check-equal? "abc" (string-copy "abc" 0))
(check-equal? "bc" (string-copy "abc" 1))
(check-equal? "b" (string-copy "abc" 1 2))
(check-equal? "bc" (string-copy "abc" 1 3))

(check-equal? "-----"
  (let ((str (make-string 5 #\x))) (string-fill! str #\-) str))
(check-equal? "xx---"
  (let ((str (make-string 5 #\x))) (string-fill! str #\- 2) str))
(check-equal? "xx-xx"
  (let ((str (make-string 5 #\x))) (string-fill! str #\- 2 3) str))

(check-equal? "a12de"
  (let ((str (string-copy "abcde"))) (string-copy! str 1 "12345" 0 2) str))
(check-equal? "-----"
  (let ((str (make-string 5 #\x))) (string-copy! str 0 "-----") str))
(check-equal? "---xx"
  (let ((str (make-string 5 #\x))) (string-copy! str 0 "-----" 2) str))
(check-equal? "xx---"
  (let ((str (make-string 5 #\x))) (string-copy! str 2 "-----" 0 3) str))
(check-equal? "xx-xx"
  (let ((str (make-string 5 #\x))) (string-copy! str 2 "-----" 2 3) str))

;; same source and dest
(check-equal? "aabde"
  (let ((str (string-copy "abcde"))) (string-copy! str 1 str 0 2) str))
(check-equal? "abcab"
  (let ((str (string-copy "abcde"))) (string-copy! str 3 str 0 2) str)))

(test-begin "6.8 Vectors"

  (check-equal? #t (vector? #()))
  (check-equal? #t (vector? #(1 2 3)))
  (check-equal? #t (vector? '#(1 2 3)))

(check-equal? 0 (vector-length (make-vector 0)))
(check-equal? 1000 (vector-length (make-vector 1000)))

(check-equal? #(0 (2 2 2 2) "Anna") '#(0 (2 2 2 2) "Anna"))

(check-equal? #(a b c) (vector 'a 'b 'c))

(check-equal? 8 (vector-ref '#(1 1 2 3 5 8 13 21) 5))
;(check-equal? 13 (vector-ref '#(1 1 2 3 5 8 13 21)
;           (let ((i (round (* 2 (acos -1)))))
;             (if (inexact? i)
;               (exact i)
;               i))))

(check-equal? #(0 ("Sue" "Sue") "Anna") (let ((vec (vector 0 '(2 2 2 2) "Anna")))
                                  (vector-set! vec 1 '("Sue" "Sue"))
                                  vec))

(check-equal? '(dah dah didah) (vector->list '#(dah dah didah)))
(check-equal? '(dah didah) (vector->list '#(dah dah didah) 1))
(check-equal? '(dah) (vector->list '#(dah dah didah) 1 2))
(check-equal? #(dididit dah) (list->vector '(dididit dah)))

(check-equal? #() (string->vector ""))
(check-equal? #(#\A #\B #\C) (string->vector "ABC"))
(check-equal? #(#\B #\C) (string->vector "ABC" 1))
(check-equal? #(#\B) (string->vector "ABC" 1 2))

(check-equal? "" (vector->string #()))
(check-equal? "123" (vector->string #(#\1 #\2 #\3)))
(check-equal? "23" (vector->string #(#\1 #\2 #\3) 1))
(check-equal? "2" (vector->string #(#\1 #\2 #\3) 1 2))

(check-equal? #() (vector-copy #()))
(check-equal? #(a b c) (vector-copy #(a b c)))
(check-equal? #(b c) (vector-copy #(a b c) 1))
(check-equal? #(b) (vector-copy #(a b c) 1 2))

(check-equal? #() (vector-append #()))
(check-equal? #() (vector-append #() #()))
(check-equal? #(a b c) (vector-append #() #(a b c)))
(check-equal? #(a b c) (vector-append #(a b c) #()))
(check-equal? #(a b c d e) (vector-append #(a b c) #(d e)))
(check-equal? #(a b c d e f) (vector-append #(a b c) #(d e) #(f)))

(check-equal? #(1 2 smash smash 5)
  (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'smash 2 4) vec))
(check-equal? #(x x x x x)
  (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x) vec))
(check-equal? #(1 2 x x x)
  (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x 2) vec))
(check-equal? #(1 2 x 4 5)
  (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x 2 3) vec))

(check-equal? #(1 a b 4 5)
  (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 1 #(a b c d e) 0 2) vec))
(check-equal? #(a b c d e)
  (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 0 #(a b c d e)) vec))
(check-equal? #(c d e 4 5)
  (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 0 #(a b c d e) 2) vec))
(check-equal? #(1 2 a b c)
  (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 2 #(a b c d e) 0 3) vec))
(check-equal? #(1 2 c 4 5)
  (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 2 #(a b c d e) 2 3) vec))

;; same source and dest
(check-equal? #(1 1 2 4 5)
  (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 1 vec 0 2) vec))
(check-equal? #(1 2 3 1 2)
  (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 3 vec 0 2) vec)))

(test-begin "6.9 Bytevectors"

;(check-equal? #t (bytevector? #u8()))
;(check-equal? #t (bytevector? #u8(0 1 2)))
;(check-equal? #f (bytevector? #()))
;(check-equal? #f (bytevector? #(0 1 2)))
;(check-equal? #f (bytevector? '()))
;(check-equal? #t (bytevector? (make-bytevector 0)))
;
;(check-equal? 0 (bytevector-length (make-bytevector 0)))
;(check-equal? 1024 (bytevector-length (make-bytevector 1024)))
;(check-equal? 1024 (bytevector-length (make-bytevector 1024 255)))
;
;(check-equal? 3 (bytevector-length (bytevector 0 1 2)))
;
;(check-equal? 0 (bytevector-u8-ref (bytevector 0 1 2) 0))
;(check-equal? 1 (bytevector-u8-ref (bytevector 0 1 2) 1))
;(check-equal? 2 (bytevector-u8-ref (bytevector 0 1 2) 2))
;
;(check-equal? #u8(0 255 2)
;  (let ((bv (bytevector 0 1 2))) (bytevector-u8-set! bv 1 255) bv))
;
;(check-equal? #u8() (bytevector-copy #u8()))
;(check-equal? #u8(0 1 2) (bytevector-copy #u8(0 1 2)))
;(check-equal? #u8(1 2) (bytevector-copy #u8(0 1 2) 1))
;(check-equal? #u8(1) (bytevector-copy #u8(0 1 2) 1 2))
;
;(check-equal? #u8(1 6 7 4 5)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (bytevector-copy! bv 1 #u8(6 7 8 9 10) 0 2)
;    bv))
;(check-equal? #u8(6 7 8 9 10)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (bytevector-copy! bv 0 #u8(6 7 8 9 10))
;    bv))
;(check-equal? #u8(8 9 10 4 5)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (bytevector-copy! bv 0 #u8(6 7 8 9 10) 2)
;    bv))
;(check-equal? #u8(1 2 6 7 8)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (bytevector-copy! bv 2 #u8(6 7 8 9 10) 0 3)
;    bv))
;(check-equal? #u8(1 2 8 4 5)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (bytevector-copy! bv 2 #u8(6 7 8 9 10) 2 3)
;    bv))
;
;;; same source and dest
;(check-equal? #u8(1 1 2 4 5)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (bytevector-copy! bv 1 bv 0 2)
;    bv))
;(check-equal? #u8(1 2 3 1 2)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (bytevector-copy! bv 3 bv 0 2)
;    bv))
;
;(check-equal? #u8() (bytevector-append #u8()))
;(check-equal? #u8() (bytevector-append #u8() #u8()))
;(check-equal? #u8(0 1 2) (bytevector-append #u8() #u8(0 1 2)))
;(check-equal? #u8(0 1 2) (bytevector-append #u8(0 1 2) #u8()))
;(check-equal? #u8(0 1 2 3 4) (bytevector-append #u8(0 1 2) #u8(3 4)))
;(check-equal? #u8(0 1 2 3 4 5) (bytevector-append #u8(0 1 2) #u8(3 4) #u8(5)))
;
;(check-equal? "ABC" (utf8->string #u8(#x41 #x42 #x43)))
;(check-equal? "ABC" (utf8->string #u8(0 #x41 #x42 #x43) 1))
;(check-equal? "ABC" (utf8->string #u8(0 #x41  #x42 #x43 0) 1 4))
;(check-equal? "λ" (utf8->string #u8(0 #xCE #xBB 0) 1 3))
;(check-equal? #u8(#x41 #x42 #x43) (string->utf8 "ABC"))
;(check-equal? #u8(#x42 #x43) (string->utf8 "ABC" 1))
;(check-equal? #u8(#x42) (string->utf8 "ABC" 1 2))
;(check-equal? #u8(#xCE #xBB) (string->utf8 "λ"))

)

(test-begin "6.10 Control Features"

  (check-equal? #t (procedure? car))
  (check-equal? #f (procedure? 'car))
  (check-equal? #t (procedure? (lambda (x) (* x x))))
  (check-equal? #f (procedure? '(lambda (x) (* x x))))
  (check-equal? #t (call-with-current-continuation procedure?))

  (check-equal? 7 (apply + (list 3 4)))
  (check-equal? 7 (apply + 3 4 (list)))
;  (test-error (apply +)) ;; not enough args
;  (test-error (apply + 3)) ;; final arg not a list
;  (test-error (apply + 3 4)) ;; final arg not a list
;  (test-error (apply + '(2 3 . 4))) ;; final arg is improper


  (define compose
    (lambda (f g)
      (lambda args
        (f (apply g args)))))
  (check-equal? '(30 0)
    (call-with-values (lambda () ((compose exact-integer-sqrt *) 12 75))
      list))

  (check-equal? '(b e h) (map cadr '((a b) (d e) (g h))))

  (check-equal? '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))

  (check-equal? '(5 7 9) (map + '(1 2 3) '(4 5 6 7)))

  (check-equal? #t
    (let ((res (let ((count 0))
                 (map (lambda (ignored)
                        (set! count (+ count 1))
                        count)
                     '(a b)))))
      (or (equal? res '(1 2))
        (equal? res '(2 1)))))

;  (check-equal? '(10 200 3000 40 500 6000)
;    (let ((ls1 (list 10 100 1000))
;           (ls2 (list 1 2 3 4 5 6)))
;      (set-cdr! (cddr ls1) ls1)
;      (map * ls1 ls2)))

;  (check-equal? "abdegh" (string-map char-foldcase "AbdEgH"))

;  (check-equal? "IBM" (string-map
;                        (lambda (c)
;                          (integer->char (+ 1 (char->integer c))))
;                        "HAL"))
;
;  (check-equal? "StUdLyCaPs"
;    (string-map
;      (lambda (c k) (if (eqv? k #\u) (char-upcase c) (char-downcase c)))
;      "studlycaps xxx"
;      "ululululul"))

;  (check-equal? #(b e h) (vector-map cadr '#((a b) (d e) (g h))))
;
;  (check-equal? #(1 4 27 256 3125)
;    (vector-map (lambda (n) (expt n n))
;        '#(1 2 3 4 5)))
;
;  (check-equal? #(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6 7)))
;
;  (check-equal? #t
;    (let ((res (let ((count 0))
;                 (vector-map
;                   (lambda (ignored)
;                     (set! count (+ count 1))
;                     count)
;                     '#(a b)))))
;      (or (equal? res #(1 2))
;        (equal? res #(2 1)))))

  (check-equal? #(0 1 4 9 16)
    (let ((v (make-vector 5)))
      (for-each (lambda (i)
                  (vector-set! v i (* i i)))
          '(0 1 2 3 4))
      v))

;  (check-equal? 9750
;    (let ((ls1 (list 10 100 1000))
;           (ls2 (list 1 2 3 4 5 6))
;           (count 0))
;      (set-cdr! (cddr ls1) ls1)
;      (for-each (lambda (x y) (set! count (+ count (* x y)))) ls2 ls1)
;      count))

;  (check-equal? '(101 100 99 98 97)
;    (let ((v '()))
;      (string-for-each
;        (lambda (c) (set! v (cons (char->integer c) v)))
;        "abcde")
;      v))

;  (check-equal? '(0 1 4 9 16) (let ((v (make-list 5)))
;                                (vector-for-each
;                                  (lambda (i) (list-set! v i (* i i)))
;                                    '#(0 1 2 3 4))
;                                v))

;  (check-equal? -3 (call-with-current-continuation
;                     (lambda (exit)
;                       (for-each (lambda (x)
;                                   (if (negative? x)
;                                     (exit x)))
;                           '(54 0 37 -3 245 19))
;                       #t)))
  (define list-length
    (lambda (obj)
      (call-with-current-continuation
        (lambda (return)
          (letrec ((r
                     (lambda (obj)
                       (cond ((null? obj) 0)
                         ((pair? obj)
                           (+ (r (cdr obj)) 1))
                         (else (return #f))))))
            (r obj))))))

  (check-equal? 4 (list-length '(1 2 3 4)))

  (check-equal? #f (list-length '(a b . c)))

  (check-equal? 5
    (call-with-values (lambda () (values 4 5))
      (lambda (a b) b)))

  (check-equal? -1 (call-with-values * -))

;  (check-equal? '(connect talk1 disconnect
;                   connect talk2 disconnect)
;    (let ((path '())
;           (c #f))
;      (let ((add (lambda (s)
;                   (set! path (cons s path)))))
;        (dynamic-wind
;          (lambda () (add 'connect))
;          (lambda ()
;            (add (call-with-current-continuation
;                   (lambda (c0)
;                     (set! c c0)
;                       'talk1))))
;          (lambda () (add 'disconnect)))
;        (if (< (length path) 4)
;          (c 'talk2)
;          (reverse path)))))
)

(test-begin "6.11 Exceptions"

;(check-equal? 65
;  (with-exception-handler
;    (lambda (con) 42)
;    (lambda ()
;      (+ (raise-continuable "should be a number")
;        23))))
;
;(check-equal? #t
;  (error-object? (guard (exn (else exn)) (error "BOOM!" 1 2 3))))
;(check-equal? "BOOM!"
;  (error-object-message (guard (exn (else exn)) (error "BOOM!" 1 2 3))))
;(check-equal? '(1 2 3)
;  (error-object-irritants (guard (exn (else exn)) (error "BOOM!" 1 2 3))))
;
;(check-equal? #f
;  (file-error? (guard (exn (else exn)) (error "BOOM!"))))
;(check-equal? #t
;  (file-error? (guard (exn (else exn)) (open-input-file " no such file "))))
;
;(check-equal? #f
;  (read-error? (guard (exn (else exn)) (error "BOOM!"))))
;(check-equal? #t
;  (read-error? (guard (exn (else exn)) (read (open-input-string ")")))))
;(check-equal? #t
;  (read-error? (guard (exn (else exn)) (read (open-input-string "\"")))))
;
;(define something-went-wrong #f)
;(define (test-exception-handler-1 v)
;  (call-with-current-continuation
;    (lambda (k)
;      (with-exception-handler
;        (lambda (x)
;          (set! something-went-wrong (list "condition: " x))
;          (k 'exception))
;        (lambda ()
;          (+ 1 (if (> v 0) (+ v 100) (raise 'an-error))))))))
;(check-equal? 106 (test-exception-handler-1 5))
;(check-equal? #f something-went-wrong)
;(check-equal? 'exception (test-exception-handler-1 -1))
;(check-equal? '("condition: " an-error) something-went-wrong)
;
;(set! something-went-wrong #f)
;(define (test-exception-handler-2 v)
;  (guard (ex (else 'caught-another-exception))
;    (with-exception-handler
;      (lambda (x)
;        (set! something-went-wrong #t)
;        (list "exception:" x))
;      (lambda ()
;        (+ 1 (if (> v 0) (+ v 100) (raise 'an-error)))))))
;(check-equal? 106 (test-exception-handler-2 5))
;(check-equal? #f something-went-wrong)
;(check-equal? 'caught-another-exception (test-exception-handler-2 -1))
;(check-equal? #t something-went-wrong)
;
;;; Based on an example from R6RS-lib section 7.1 Exceptions.
;;; R7RS section 6.11 Exceptions has a simplified version.
;(let* ((out (open-output-string))
;        (value (with-exception-handler
;                 (lambda (con)
;                   (cond
;                     ((not (list? con))
;                       (raise con))
;                     ((list? con)
;                       (display (car con) out))
;                     (else
;                       (display "a warning has been issued" out)))
;                   42)
;                 (lambda ()
;                   (+ (raise-continuable
;                        (list "should be a number"))
;                     23)))))
;  (check-equal? "should be a number" (get-output-string out))
;  (check-equal? 65 value))
;
;;; From SRFI-34 "Examples" section - #3
;(define (test-exception-handler-3 v out)
;  (guard (condition
;           (else
;             (display "condition: " out)
;             (write condition out)
;             (display #\! out)
;               'exception))
;    (+ 1 (if (= v 0) (raise 'an-error) (/ 10 v)))))
;(let* ((out (open-output-string))
;        (value (test-exception-handler-3 0 out)))
;  (check-equal? 'exception value)
;  (check-equal? "condition: an-error!" (get-output-string out)))
;
;(define (test-exception-handler-4 v out)
;  (call-with-current-continuation
;    (lambda (k)
;      (with-exception-handler
;        (lambda (x)
;          (display "reraised " out)
;          (write x out) (display #\! out)
;          (k 'zero))
;        (lambda ()
;          (guard (condition
;                   ((positive? condition)
;                       'positive)
;                   ((negative? condition)
;                       'negative))
;            (raise v)))))))
;
;;; From SRFI-34 "Examples" section - #5
;(let* ((out (open-output-string))
;        (value (test-exception-handler-4 1 out)))
;  (check-equal? "" (get-output-string out))
;  (check-equal? 'positive value))
;;; From SRFI-34 "Examples" section - #6
;(let* ((out (open-output-string))
;        (value (test-exception-handler-4 -1 out)))
;  (check-equal? "" (get-output-string out))
;  (check-equal? 'negative value))
;;; From SRFI-34 "Examples" section - #7
;(let* ((out (open-output-string))
;        (value (test-exception-handler-4 0 out)))
;  (check-equal? "reraised 0!" (get-output-string out))
;  (check-equal? 'zero value))
;
;;; From SRFI-34 "Examples" section - #8
;(check-equal? 42
;  (guard (condition
;           ((assq 'a condition) => cdr)
;           ((assq 'b condition)))
;    (raise (list (cons 'a 42)))))
;
;;; From SRFI-34 "Examples" section - #9
;(check-equal? '(b . 23)
;  (guard (condition
;           ((assq 'a condition) => cdr)
;           ((assq 'b condition)))
;    (raise (list (cons 'b 23)))))
;
;(check-equal? 'caught-d
;  (guard (condition
;           ((assq 'c condition) 'caught-c)
;           ((assq 'd condition) 'caught-d))
;    (list
;      (sqrt 8)
;      (guard (condition
;               ((assq 'a condition) => cdr)
;               ((assq 'b condition)))
;        (raise (list (cons 'd 24)))))))

)

(test-begin "6.12 Environments and evaluation"

;;; (check-equal? 21 (eval '(* 7 3) (scheme-report-environment 5)))
;
;(check-equal? 20
;  (let ((f (eval '(lambda (f x) (f x x)) (null-environment 5))))
;    (f + 10)))
;
;(check-equal? 1024 (eval '(expt 2 10) (environment '(scheme base))))
;;; (sin 0) may return exact number
;(check-equal? 0.0 (inexact (eval '(sin 0) (environment '(scheme inexact)))))
;;; ditto
;(check-equal? 1024.0 (eval '(+ (expt 2 10) (inexact (sin 0)))
;               (environment '(scheme base) '(scheme inexact))))

)

;(test-begin "6.13 Input and output")
;
;(check-equal? #t (port? (current-input-port)))
;(check-equal? #t (input-port? (current-input-port)))
;(check-equal? #t (output-port? (current-output-port)))
;(check-equal? #t (output-port? (current-error-port)))
;(check-equal? #t (input-port? (open-input-string "abc")))
;(check-equal? #t (output-port? (open-output-string)))
;
;(check-equal? #t (textual-port? (open-input-string "abc")))
;(check-equal? #t (textual-port? (open-output-string)))
;(check-equal? #t (binary-port? (open-input-bytevector #u8(0 1 2))))
;(check-equal? #t (binary-port? (open-output-bytevector)))
;
;(check-equal? #t (input-port-open? (open-input-string "abc")))
;(check-equal? #t (output-port-open? (open-output-string)))
;
;(check-equal? #f
;  (let ((in (open-input-string "abc")))
;    (close-input-port in)
;    (input-port-open? in)))
;
;(check-equal? #f
;  (let ((out (open-output-string)))
;    (close-output-port out)
;    (output-port-open? out)))
;
;(check-equal? #f
;  (let ((out (open-output-string)))
;    (close-port out)
;    (output-port-open? out)))
;
;(check-equal? 'error
;  (let ((in (open-input-string "abc")))
;    (close-input-port in)
;    (guard (exn (else 'error)) (read-char in))))
;
;(check-equal? 'error
;  (let ((out (open-output-string)))
;    (close-output-port out)
;    (guard (exn (else 'error)) (write-char #\c out))))
;
;(check-equal? #t (eof-object? (eof-object)))
;(check-equal? #t (eof-object? (read (open-input-string ""))))
;(check-equal? #t (char-ready? (open-input-string "42")))
;(check-equal? 42 (read (open-input-string " 42 ")))
;
;(check-equal? #t (eof-object? (read-char (open-input-string ""))))
;(check-equal? #\a (read-char (open-input-string "abc")))
;
;(check-equal? #t (eof-object? (read-line (open-input-string ""))))
;(check-equal? "abc" (read-line (open-input-string "abc")))
;(check-equal? "abc" (read-line (open-input-string "abc\ndef\n")))
;
;(check-equal? #t (eof-object? (read-string 3 (open-input-string ""))))
;(check-equal? "abc" (read-string 3 (open-input-string "abcd")))
;(check-equal? "abc" (read-string 3 (open-input-string "abc\ndef\n")))
;
;(let ((in (open-input-string (string #\x10F700 #\x10F701 #\x10F702))))
;  (let* ((c0 (peek-char in))
;          (c1 (read-char in))
;          (c2 (read-char in))
;          (c3 (read-char in)))
;    (check-equal? #\x10F700 c0)
;    (check-equal? #\x10F700 c1)
;    (check-equal? #\x10F701 c2)
;    (check-equal? #\x10F702 c3)))
;
;(check-equal? (string #\x10F700)
;  (let ((out (open-output-string)))
;    (write-char #\x10F700 out)
;    (get-output-string out)))
;
;(check-equal? "abc"
;  (let ((out (open-output-string)))
;    (write 'abc out)
;    (get-output-string out)))
;
;(check-equal? "abc def"
;  (let ((out (open-output-string)))
;    (display "abc def" out)
;    (get-output-string out)))
;
;(check-equal? "abc"
;  (let ((out (open-output-string)))
;    (display #\a out)
;    (display "b" out)
;    (display #\c out)
;    (get-output-string out)))
;
;(check-equal? #t
;  (let* ((out (open-output-string))
;          (r (begin (newline out) (get-output-string out))))
;    (or (equal? r "\n") (equal? r "\r\n"))))
;
;(check-equal? "abc def"
;  (let ((out (open-output-string)))
;    (write-string "abc def" out)
;    (get-output-string out)))
;
;(check-equal? "def"
;  (let ((out (open-output-string)))
;    (write-string "abc def" out 4)
;    (get-output-string out)))
;
;(check-equal? "c d"
;  (let ((out (open-output-string)))
;    (write-string "abc def" out 2 5)
;    (get-output-string out)))
;
;(check-equal? ""
;  (let ((out (open-output-string)))
;    (flush-output-port out)
;    (get-output-string out)))
;
;(check-equal? #t (eof-object? (read-u8 (open-input-bytevector #u8()))))
;(check-equal? 1 (read-u8 (open-input-bytevector #u8(1 2 3))))
;
;(check-equal? #t (eof-object? (read-bytevector 3 (open-input-bytevector #u8()))))
;(check-equal? #t (u8-ready? (open-input-bytevector #u8(1))))
;(check-equal? #u8(1) (read-bytevector 3 (open-input-bytevector #u8(1))))
;(check-equal? #u8(1 2) (read-bytevector 3 (open-input-bytevector #u8(1 2))))
;(check-equal? #u8(1 2 3) (read-bytevector 3 (open-input-bytevector #u8(1 2 3))))
;(check-equal? #u8(1 2 3) (read-bytevector 3 (open-input-bytevector #u8(1 2 3 4))))
;
;(check-equal? #t
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (eof-object? (read-bytevector! bv (open-input-bytevector #u8())))))
;
;(check-equal? #u8(6 7 8 9 10)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 0 5)
;    bv))
;
;(check-equal? #u8(6 7 8 4 5)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 0 3)
;    bv))
;
;(check-equal? #u8(1 2 3 6 5)
;  (let ((bv (bytevector 1 2 3 4 5)))
;    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 3 4)
;    bv))
;
;(check-equal? #u8(1 2 3)
;  (let ((out (open-output-bytevector)))
;    (write-u8 1 out)
;    (write-u8 2 out)
;    (write-u8 3 out)
;    (get-output-bytevector out)))
;
;(check-equal? #u8(1 2 3 4 5)
;  (let ((out (open-output-bytevector)))
;    (write-bytevector #u8(1 2 3 4 5) out)
;    (get-output-bytevector out)))
;
;(check-equal? #u8(3 4 5)
;  (let ((out (open-output-bytevector)))
;    (write-bytevector #u8(1 2 3 4 5) out 2)
;    (get-output-bytevector out)))
;
;(check-equal? #u8(3 4)
;  (let ((out (open-output-bytevector)))
;    (write-bytevector #u8(1 2 3 4 5) out 2 4)
;    (get-output-bytevector out)))
;
;(check-equal? #u8()
;  (let ((out (open-output-bytevector)))
;    (flush-output-port out)
;    (get-output-bytevector out)))
;
;(check-equal? #t
;  (and (member
;         (let ((out (open-output-string))
;                (x (list 1)))
;           (set-cdr! x x)
;           (write x out)
;           (get-output-string out))
;         ;; labels not guaranteed to be 0 indexed, spacing may differ
;           '("#0=(1 . #0#)" "#1=(1 . #1#)"))
;    #t))
;
;(check-equal? "((1 2 3) (1 2 3))"
;  (let ((out (open-output-string))
;         (x (list 1 2 3)))
;    (write (list x x) out)
;    (get-output-string out)))
;
;(check-equal? "((1 2 3) (1 2 3))"
;  (let ((out (open-output-string))
;         (x (list 1 2 3)))
;    (write-simple (list x x) out)
;    (get-output-string out)))
;
;(check-equal? #t
;  (and (member (let ((out (open-output-string))
;                      (x (list 1 2 3)))
;                 (write-shared (list x x) out)
;                 (get-output-string out))
;           '("(#0=(1 2 3) #0#)" "(#1=(1 2 3) #1#)"))
;    #t))
;
;(test-begin "Read syntax")
;
;;; check reading boolean followed by eof
;(check-equal? #t (read (open-input-string "#t")))
;(check-equal? #t (read (open-input-string "#true")))
;(check-equal? #f (read (open-input-string "#f")))
;(check-equal? #f (read (open-input-string "#false")))
;(define (read2 port)
;  (let* ((o1 (read port)) (o2 (read port)))
;    (cons o1 o2)))
;;; check reading boolean followed by delimiter
;(check-equal? '(#t . (5)) (read2 (open-input-string "#t(5)")))
;(check-equal? '(#t . 6) (read2 (open-input-string "#true 6 ")))
;(check-equal? '(#f . 7) (read2 (open-input-string "#f 7")))
;(check-equal? '(#f . "8") (read2 (open-input-string "#false\"8\"")))
;
;(check-equal? '() (read (open-input-string "()")))
;(check-equal? '(1 2) (read (open-input-string "(1 2)")))
;(check-equal? '(1 . 2) (read (open-input-string "(1 . 2)")))
;(check-equal? '(1 2) (read (open-input-string "(1 . (2))")))
;(check-equal? '(1 2 3 4 5) (read (open-input-string "(1 . (2 3 4 . (5)))")))
;(check-equal? '1 (cadr (read (open-input-string "#0=(1 . #0#)"))))
;(check-equal? '(1 2 3) (cadr (read (open-input-string "(#0=(1 2 3) #0#)"))))
;
;(check-equal? '(quote (1 2)) (read (open-input-string "'(1 2)")))
;(check-equal? '(quote (1 (unquote 2))) (read (open-input-string "'(1 ,2)")))
;(check-equal? '(quote (1 (unquote-splicing 2))) (read (open-input-string "'(1 ,@2)")))
;(check-equal? '(quasiquote (1 (unquote 2))) (read (open-input-string "`(1 ,2)")))
;
;(check-equal? #() (read (open-input-string "#()")))
;(check-equal? #(a b) (read (open-input-string "#(a b)")))
;
;(check-equal? #u8() (read (open-input-string "#u8()")))
;(check-equal? #u8(0 1) (read (open-input-string "#u8(0 1)")))
;
;(check-equal? 'abc (read (open-input-string "abc")))
;(check-equal? 'abc (read (open-input-string "abc def")))
;(check-equal? 'ABC (read (open-input-string "ABC")))
;(check-equal? 'Hello (read (open-input-string "|H\\x65;llo|")))
;
;(check-equal? 'abc (read (open-input-string "#!fold-case ABC")))
;(check-equal? 'ABC (read (open-input-string "#!fold-case #!no-fold-case ABC")))
;
;(check-equal? 'def (read (open-input-string "#; abc def")))
;(check-equal? 'def (read (open-input-string "; abc \ndef")))
;(check-equal? 'def (read (open-input-string "#| abc |# def")))
;(check-equal? 'ghi (read (open-input-string "#| abc #| def |# |# ghi")))
;(check-equal? 'ghi (read (open-input-string "#; ; abc\n def ghi")))
;(check-equal? '(abs -16) (read (open-input-string "(#;sqrt abs -16)")))
;(check-equal? '(a d) (read (open-input-string "(a #; #;b c d)")))
;(check-equal? '(a e) (read (open-input-string "(a #;(b #;c d) e)")))
;(check-equal? '(a . c) (read (open-input-string "(a . #;b c)")))
;(check-equal? '(a . b) (read (open-input-string "(a . b #;c)")))
;
;(define (test-read-error str)
;  (test-assert str
;    (guard (exn (else #t))
;      (read (open-input-string str))
;      #f)))
;
;(test-read-error "(#;a . b)")
;(test-read-error "(a . #;b)")
;(test-read-error "(a #;. b)")
;(test-read-error "(#;x #;y . z)")
;(test-read-error "(#; #;x #;y . z)")
;(test-read-error "(#; #;x . z)")
;
;(check-equal? #\a (read (open-input-string "#\\a")))
;(check-equal? #\space (read (open-input-string "#\\space")))
;(check-equal? 0 (char->integer (read (open-input-string "#\\null"))))
;(check-equal? 7 (char->integer (read (open-input-string "#\\alarm"))))
;(check-equal? 8 (char->integer (read (open-input-string "#\\backspace"))))
;(check-equal? 9 (char->integer (read (open-input-string "#\\tab"))))
;(check-equal? 10 (char->integer (read (open-input-string "#\\newline"))))
;(check-equal? 13 (char->integer (read (open-input-string "#\\return"))))
;(check-equal? #x7F (char->integer (read (open-input-string "#\\delete"))))
;(check-equal? #x1B (char->integer (read (open-input-string "#\\escape"))))
;(check-equal? #x03BB (char->integer (read (open-input-string "#\\λ"))))
;(check-equal? #x03BB (char->integer (read (open-input-string "#\\x03BB"))))
;
;(check-equal? "abc" (read (open-input-string "\"abc\"")))
;(check-equal? "abc" (read (open-input-string "\"abc\" \"def\"")))
;(check-equal? "ABC" (read (open-input-string "\"ABC\"")))
;(check-equal? "Hello" (read (open-input-string "\"H\\x65;llo\"")))
;(check-equal? 7 (char->integer (string-ref (read (open-input-string "\"\\a\"")) 0)))
;(check-equal? 8 (char->integer (string-ref (read (open-input-string "\"\\b\"")) 0)))
;(check-equal? 9 (char->integer (string-ref (read (open-input-string "\"\\t\"")) 0)))
;(check-equal? 10 (char->integer (string-ref (read (open-input-string "\"\\n\"")) 0)))
;(check-equal? 13 (char->integer (string-ref (read (open-input-string "\"\\r\"")) 0)))
;(check-equal? #x22 (char->integer (string-ref (read (open-input-string "\"\\\"\"")) 0)))
;(check-equal? #x7C (char->integer (string-ref (read (open-input-string "\"\\|\"")) 0)))
;(check-equal? "line 1\nline 2\n" (read (open-input-string "\"line 1\nline 2\n\"")))
;(check-equal? "line 1continued\n" (read (open-input-string "\"line 1\\\ncontinued\n\"")))
;(check-equal? "line 1continued\n" (read (open-input-string "\"line 1\\ \ncontinued\n\"")))
;(check-equal? "line 1continued\n" (read (open-input-string "\"line 1\\\n continued\n\"")))
;(check-equal? "line 1continued\n" (read (open-input-string "\"line 1\\ \t \n \t continued\n\"")))
;(check-equal? "line 1\n\nline 3\n" (read (open-input-string "\"line 1\\ \t \n \t \n\nline 3\n\"")))
;(check-equal? #x03BB (char->integer (string-ref (read (open-input-string "\"\\x03BB;\"")) 0)))
;
;(define-syntax test-write-syntax
;  (syntax-rules ()
;    ((test-write-syntax expect-str obj-expr)
;      (let ((out (open-output-string)))
;        (write obj-expr out)
;        (check-equal? expect-str (get-output-string out))))))
;
;(test-write-syntax "|.|" '|.|)
;(test-write-syntax "|a b|" '|a b|)
;(test-write-syntax "|,a|" '|,a|)
;(test-write-syntax "|\"|" '|\"|)
;(test-write-syntax "|\\||" '|\||)
;(test-write-syntax "||" '||)
;(test-write-syntax "|\\\\123|" '|\\123|)
;(test-write-syntax "a" '|a|)
;;; (test-write-syntax "a.b" '|a.b|)
;(test-write-syntax "|2|" '|2|)
;(test-write-syntax "|+3|" '|+3|)
;(test-write-syntax "|-.4|" '|-.4|)
;(test-write-syntax "|+i|" '|+i|)
;(test-write-syntax "|-i|" '|-i|)
;(test-write-syntax "|+inf.0|" '|+inf.0|)
;(test-write-syntax "|-inf.0|" '|-inf.0|)
;(test-write-syntax "|+nan.0|" '|+nan.0|)
;(test-write-syntax "|+NaN.0|" '|+NaN.0|)
;(test-write-syntax "|+NaN.0abc|" '|+NaN.0abc|)
;
;(test-end)
;
;(test-begin "Numeric syntax")
;
;;; Numeric syntax adapted from Peter Bex's tests.
;;;
;;; These are updated to R7RS, using string ports instead of
;;; string->number, and "error" tests removed because implementations
;;; are free to provide their own numeric extensions.  Currently all
;;; tests are run by default - need to cond-expand and test for
;;; infinities and -0.0.
;
;(define-syntax test-numeric-syntax
;  (syntax-rules ()
;    ((test-numeric-syntax str expect strs ...)
;     (let* ((z (read (open-input-string str)))
;            (out (open-output-string))
;            (z-str (begin (write z out) (get-output-string out))))
;       (check-equal? expect (values z))
;       (check-equal? #t (and (member z-str '(str strs ...)) #t))))))
;
;;; Each test is of the form:
;;;
;;;   (test-numeric-syntax input-str expected-value expected-write-values ...)
;;;
;;; where the input should be eqv? to the expected-value, and the
;;; written output the same as any of the expected-write-values.  The
;;; form
;;;
;;;   (test-numeric-syntax input-str expected-value)
;;;
;;; is a shorthand for
;;;
;;;   (test-numeric-syntax input-str expected-value (input-str))
;
;;; Simple
;(test-numeric-syntax "1" 1)
;(test-numeric-syntax "+1" 1 "1")
;(test-numeric-syntax "-1" -1)
;(test-numeric-syntax "#i1" 1.0 "1.0" "1.")
;(test-numeric-syntax "#I1" 1.0 "1.0" "1.")
;(test-numeric-syntax "#i-1" -1.0 "-1.0" "-1.")
;;; Decimal
;(test-numeric-syntax "1.0" 1.0 "1.0" "1.")
;(test-numeric-syntax "1." 1.0 "1.0" "1.")
;(test-numeric-syntax ".1" 0.1 "0.1" "100.0e-3")
;(test-numeric-syntax "-.1" -0.1 "-0.1" "-100.0e-3")
;;; Some Schemes don't allow negative zero. This is okay with the standard
;(test-numeric-syntax "-.0" -0.0 "-0." "-0.0" "0.0" "0." ".0")
;(test-numeric-syntax "-0." -0.0 "-.0" "-0.0" "0.0" "0." ".0")
;(test-numeric-syntax "#i1.0" 1.0 "1.0" "1.")
;(test-numeric-syntax "#e1.0" 1 "1")
;(test-numeric-syntax "#e-.0" 0 "0")
;(test-numeric-syntax "#e-0." 0 "0")
;;; Decimal notation with suffix
;(test-numeric-syntax "1e2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1E2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1s2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1S2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1f2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1F2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1d2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1D2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1l2" 100.0 "100.0" "100.")
;(test-numeric-syntax "1L2" 100.0 "100.0" "100.")
;;; NaN, Inf
;(test-numeric-syntax "+nan.0" +nan.0 "+nan.0" "+NaN.0")
;(test-numeric-syntax "+NAN.0" +nan.0 "+nan.0" "+NaN.0")
;(test-numeric-syntax "+inf.0" +inf.0 "+inf.0" "+Inf.0")
;(test-numeric-syntax "+InF.0" +inf.0 "+inf.0" "+Inf.0")
;(test-numeric-syntax "-inf.0" -inf.0 "-inf.0" "-Inf.0")
;(test-numeric-syntax "-iNF.0" -inf.0 "-inf.0" "-Inf.0")
;(test-numeric-syntax "#i+nan.0" +nan.0 "+nan.0" "+NaN.0")
;(test-numeric-syntax "#i+inf.0" +inf.0 "+inf.0" "+Inf.0")
;(test-numeric-syntax "#i-inf.0" -inf.0 "-inf.0" "-Inf.0")
;;; Exact ratios
;(test-numeric-syntax "1/2" (/ 1 2))
;(test-numeric-syntax "#e1/2" (/ 1 2) "1/2")
;(test-numeric-syntax "10/2" 5 "5")
;(test-numeric-syntax "-1/2" (- (/ 1 2)))
;(test-numeric-syntax "0/10" 0 "0")
;(test-numeric-syntax "#e0/10" 0 "0")
;(test-numeric-syntax "#i3/2" (/ 3.0 2.0) "1.5")
;;; Exact complex
;(test-numeric-syntax "1+2i" (make-rectangular 1 2))
;(test-numeric-syntax "1+2I" (make-rectangular 1 2) "1+2i")
;(test-numeric-syntax "1-2i" (make-rectangular 1 -2))
;(test-numeric-syntax "-1+2i" (make-rectangular -1 2))
;(test-numeric-syntax "-1-2i" (make-rectangular -1 -2))
;(test-numeric-syntax "+i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
;(test-numeric-syntax "0+i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
;(test-numeric-syntax "0+1i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
;(test-numeric-syntax "-i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
;(test-numeric-syntax "0-i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
;(test-numeric-syntax "0-1i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
;(test-numeric-syntax "+2i" (make-rectangular 0 2) "2i" "+2i" "0+2i")
;(test-numeric-syntax "-2i" (make-rectangular 0 -2) "-2i" "0-2i")
;;; Decimal-notation complex numbers (rectangular notation)
;(test-numeric-syntax "1.0+2i" (make-rectangular 1.0 2) "1.0+2.0i" "1.0+2i" "1.+2i" "1.+2.i")
;(test-numeric-syntax "1+2.0i" (make-rectangular 1 2.0) "1.0+2.0i" "1+2.0i" "1.+2.i" "1+2.i")
;(test-numeric-syntax "1e2+1.0i" (make-rectangular 100.0 1.0) "100.0+1.0i" "100.+1.i")
;(test-numeric-syntax "1s2+1.0i" (make-rectangular 100.0 1.0) "100.0+1.0i" "100.+1.i")
;(test-numeric-syntax "1.0+1e2i" (make-rectangular 1.0 100.0) "1.0+100.0i" "1.+100.i")
;(test-numeric-syntax "1.0+1s2i" (make-rectangular 1.0 100.0) "1.0+100.0i" "1.+100.i")
;;; Fractional complex numbers (rectangular notation)
;(test-numeric-syntax "1/2+3/4i" (make-rectangular (/ 1 2) (/ 3 4)))
;;; Mixed fractional/decimal notation complex numbers (rectangular notation)
;(test-numeric-syntax "0.5+3/4i" (make-rectangular 0.5 (/ 3 4))
;  "0.5+0.75i" ".5+.75i" "0.5+3/4i" ".5+3/4i" "500.0e-3+750.0e-3i")
;;; Complex NaN, Inf (rectangular notation)
;;;(test-numeric-syntax "+nan.0+nan.0i" (make-rectangular the-nan the-nan) "+NaN.0+NaN.0i")
;(test-numeric-syntax "+inf.0+inf.0i" (make-rectangular +inf.0 +inf.0) "+Inf.0+Inf.0i")
;(test-numeric-syntax "-inf.0+inf.0i" (make-rectangular -inf.0 +inf.0) "-Inf.0+Inf.0i")
;(test-numeric-syntax "-inf.0-inf.0i" (make-rectangular -inf.0 -inf.0) "-Inf.0-Inf.0i")
;(test-numeric-syntax "+inf.0-inf.0i" (make-rectangular +inf.0 -inf.0) "+Inf.0-Inf.0i")
;;; Complex numbers (polar notation)
;;; Need to account for imprecision in write output.
;;;(test-numeric-syntax "1@2" -0.416146836547142+0.909297426825682i "-0.416146836547142+0.909297426825682i")
;;; Base prefixes
;(test-numeric-syntax "#x11" 17 "17")
;(test-numeric-syntax "#X11" 17 "17")
;(test-numeric-syntax "#d11" 11 "11")
;(test-numeric-syntax "#D11" 11 "11")
;(test-numeric-syntax "#o11" 9 "9")
;(test-numeric-syntax "#O11" 9 "9")
;(test-numeric-syntax "#b11" 3 "3")
;(test-numeric-syntax "#B11" 3 "3")
;(test-numeric-syntax "#o7" 7 "7")
;(test-numeric-syntax "#xa" 10 "10")
;(test-numeric-syntax "#xA" 10 "10")
;(test-numeric-syntax "#xf" 15 "15")
;(test-numeric-syntax "#x-10" -16 "-16")
;(test-numeric-syntax "#d-10" -10 "-10")
;(test-numeric-syntax "#o-10" -8 "-8")
;(test-numeric-syntax "#b-10" -2 "-2")
;;; Combination of prefixes
;(test-numeric-syntax "#e#x10" 16 "16")
;(test-numeric-syntax "#i#x10" 16.0 "16.0" "16.")
;(test-numeric-syntax "#x#i10" 16.0 "16.0" "16.")
;(test-numeric-syntax "#i#x1/10" 0.0625 "0.0625")
;(test-numeric-syntax "#x#i1/10" 0.0625 "0.0625")
;;; (Attempted) decimal notation with base prefixes
;(test-numeric-syntax "#d1." 1.0 "1.0" "1.")
;(test-numeric-syntax "#d.1" 0.1 "0.1" ".1" "100.0e-3")
;(test-numeric-syntax "#x1e2" 482 "482")
;(test-numeric-syntax "#d1e2" 100.0 "100.0" "100.")
;;; Fractions with prefixes
;(test-numeric-syntax "#x10/2" 8 "8")
;(test-numeric-syntax "#x11/2" (/ 17 2) "17/2")
;(test-numeric-syntax "#d11/2" (/ 11 2) "11/2")
;(test-numeric-syntax "#o11/2" (/ 9 2) "9/2")
;(test-numeric-syntax "#b11/10" (/ 3 2) "3/2")
;;; Complex numbers with prefixes
;;;(test-numeric-syntax "#x10+11i" (make-rectangular 16 17) "16+17i")
;(test-numeric-syntax "#d1.0+1.0i" (make-rectangular 1.0 1.0) "1.0+1.0i" "1.+1.i")
;(test-numeric-syntax "#d10+11i" (make-rectangular 10 11) "10+11i")
;;;(test-numeric-syntax "#o10+11i" (make-rectangular 8 9) "8+9i")
;;;(test-numeric-syntax "#b10+11i" (make-rectangular 2 3) "2+3i")
;;;(test-numeric-syntax "#e1.0+1.0i" (make-rectangular 1 1) "1+1i" "1+i")
;;;(test-numeric-syntax "#i1.0+1.0i" (make-rectangular 1.0 1.0) "1.0+1.0i" "1.+1.i")
;
;(define-syntax test-precision
;  (syntax-rules ()
;    ((test-round-trip str alt ...)
;     (let* ((n (string->number str))
;            (str2 (number->string n))
;            (accepted (list str alt ...))
;            (ls (member str2 accepted)))
;       (test-assert (string-append "(member? " str2 " "
;                                   (let ((out (open-output-string)))
;                                     (write accepted out)
;                                     (get-output-string out))
;                                   ")")
;         (pair? ls))
;       (when (pair? ls)
;         (test-assert (string-append "(eqv?: " str " " str2 ")")
;           (eqv? n (string->number (car ls)))))))))
;
;(test-precision "-1.7976931348623157e+308" "-inf.0")
;(test-precision "4.940656458412465e-324" "4.94065645841247e-324" "5.0e-324" "0.0")
;(test-precision "9.881312916824931e-324" "9.88131291682493e-324" "1.0e-323" "0.0")
;(test-precision "1.48219693752374e-323" "1.5e-323" "0.0")
;(test-precision "1.976262583364986e-323" "1.97626258336499e-323" "2.0e-323" "0.0")
;(test-precision "2.470328229206233e-323" "2.47032822920623e-323" "2.5e-323" "0.0")
;(test-precision "2.420921664622108e-322" "2.42092166462211e-322" "2.4e-322" "0.0")
;(test-precision "2.420921664622108e-320" "2.42092166462211e-320" "2.421e-320" "0.0")
;(test-precision "1.4489974452386991" "1.4489975")
;(test-precision "0.14285714285714282" "0.14285714285714288" "0.14285715")
;(test-precision "1.7976931348623157e+308" "+inf.0")
;
;(test-end)
;
;(test-end)
;
;(test-begin "6.14 System interface")
;
;;; 6.14 System interface
;
;;; (check-equal? "/usr/local/bin:/usr/bin:/bin" (get-environment-variable "PATH"))
;
;(check-equal? #t (string? (get-environment-variable "PATH")))
;
;;; (check-equal? '(("USER" . "root") ("HOME" . "/")) (get-environment-variables))
;
;(let ((env (get-environment-variables)))
;  (define (env-pair? x)
;    (and (pair? x) (string? (car x)) (string? (cdr x))))
;  (define (all? pred ls)
;    (or (null? ls) (and (pred (car ls)) (all? pred (cdr ls)))))
;  (check-equal? #t (list? env))
;  (check-equal? #t (all? env-pair? env)))
;
;(check-equal? #t (list? (command-line)))
;
;(check-equal? #t (real? (current-second)))
;(check-equal? #t (inexact? (current-second)))
;(check-equal? #t (exact? (current-jiffy)))
;(check-equal? #t (exact? (jiffies-per-second)))
;
;(check-equal? #t (list? (features)))
;(check-equal? #t (and (memq 'r7rs (features)) #t))
;
;(check-equal? #t (file-exists? "."))
;(check-equal? #f (file-exists? " no such file "))
;
;(check-equal? #t (file-error?
;          (guard (exn (else exn))
;            (delete-file " no such file "))))
;
;(test-end)
