; (void)
; 覆盖 void 定义
; (define void 1)
; (println void)
;
; 编译不过...
; expand 时候, 因为 define 时候 void 从 topbinding 被变成 localBinding
; void 变成 gensym
; (void) -> (voidxxx) 这时候 void 还没定义, 编译时候找不到

(println true)
;(exit)

(assert (= 1 1))
; (assert (= 1 2))

(assert-equals 1 1)
;(assert-equals 1 2)

(letrec () (void))

(let* () (void))



; '(2 3 4)
(println (member 2 (list 1 2 3 4)))
;#f
(println (member 9 (list 1 2 3 4)))
;(cons 'b 'etc)
(println (member 'b (cons 'a (cons 'b 'etc))))



(println (str))
(println (str 1 2 3 "!"))

(println (arraylist 1 2 3 4))

(println (hashmap (1 2) (3 4)))


(println
  (-> 2 (+ 3))) ; 5
(println
  (-> 2 (+ 3) (- 7))) ; -2
(println
  (-> 4 (* 4 3) (- 6))) ; 42

(println (-> (str "A") (str "B") (str "C")))
(println (->> (str "A") (str "B") (str "C")))

(println (doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2)))
(println (.. java.lang.System (getProperties) (get "java.vm.version")))


(let*-values ([(a) 1] [(b) a] [(c) b]) 1)
(println null)
(println true)
(println false)


(let () 1)
(let f () 1)


(define a 42)
(.println java.lang.System/out a)

(define (f . args) args)
(.println java.lang.System/out (f 1 2 3))

(define (add a b) (+ a b))
(.println java.lang.System/out (add 1 2))

(define (test a . rest) (cons a rest))
(.println java.lang.System/out (test 1 2 3))



(let*-values () 1)
(let*-values ([(a) 1] [(b) a] [(c) b]) 1)


(define (f1)
  (define s 1)
  (println s)
  (.toString s))
(f1)



(or)
(or 1)
(or false)
(or true)
(or 1 2 3)
(or #f 2 3)
(or #f #f 3)
(or 1 (raise "not here"))
(or (println 100) 42)



(println (and))
(println (and 1))
(println (and false))
(println (and true))
(println (and 1 '()))
(println (and #f 2 3))
(println (and 1 #f 3))
(println (and 1 2 3))
(println (and #f (raise "fail")))





; '(("Burroughs") ("Rice" "Burroughs") ("Edgar" "Rice" "Burroughs"))
(println (let* ([x (list "Burroughs")]
                 [y (cons "Rice" x)]
                 [z (cons "Edgar" y)])
           (list x y z)))

;; Sequential Binding: let*
; '("Edgar" "Rice" "Burroughs")
(println (let* ([name (list "Burroughs")]
                 [name (cons "Rice" name)]
                 [name (cons "Edgar" name)])
           name))




;;(let* ([a 1])
;;  (assert-equals a 1))
;;(let* ([a 1] [b a])
;;  (assert-equals b 1))
;;(let* ([a 1] [b a] [c b])
;;  (assert-equals c 1))
;;(let* ([a 1] [a 2])
;;  (assert-equals a 2))
;;(assert-equals '(2 1)
;;  (let* ([x 1]
;;          [y (+ x 1)])
;;    (list y x)))
;;


;; Recursive Binding: letrec
; '(vine vine tarzan vine))
(println (letrec ([swing
                    (lambda (t)
                      (if (eq? (car t) 'tarzan)
                        (cons 'vine
                          (cons 'tarzan (cdr (cdr t))))
                        (cons (car t)
                          (swing (cdr t)))))])
           (swing '(vine tarzan vine vine))))

;; (letrec ([a b] [b a]) (println a) (println b))


(letrec ([is-even? (lambda (n)
                     (or (zero? n)
                       (is-odd? (sub1 n))))]
          [is-odd? (lambda (n)
                     (and (not (zero? n))
                       (is-even? (sub1 n))))])
  (println (is-odd? 11))
  (println (is-even? 12)))




;;; BUG
(let ([+ (lambda (x y)
           (if (string? x)
             (string-append x y)
             (+ x y)))]) ; use original +
  (list (+ 1 2)
    (+ "see" "saw")))

;; Occasionally, the parallel nature of let bindings is convenient
;; for swapping or rearranging a set of bindings:
(let ([me "Tarzan"]
       [you "Jane"])
  (let ([me you]
         [you me])
    (list me you)))



; 3628800
(let fac ([n 10])
  (if (zero? n)
    1
    (* n (fac (sub1 n)))))

(let () (void))
(let () (void) 1)
(let ([a 1] [b 2]) (+ a b))

(let ([x 5])
  (let ([x 2]
         [y x])
    (list y x)))

;; Named let
;; '("apple" "cheese burger!" "cheese burger!" "banana")
(let ()
  (define (duplicate pos lst)
    (let dup ([i 0]
               [lst lst])
      (if
        (= i pos)
        (cons (car lst) lst)
        (cons (car lst) (dup (+ i 1) (cdr lst))))))
  (duplicate 1 (list "apple" "cheese burger!" "banana")))
