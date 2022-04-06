#lang racket

(define DEBUG #f)

; State Definition
(struct State [exp env kont])

; Env Definition
(struct Env [table penv])
(define (make-env bindings [penv '()])
  (Env (make-hash bindings) penv))
(define (env-lookup env name)
  (match env
    [(Env table '())
      (hash-ref table name
        (lambda () (error (format "~s undefined. ~n" name))))]
    [(Env table parent)
      (if (hash-has-key? table name)
        (hash-ref table name)
        (env-lookup parent name))]))
(define (env-define env name val)
  (hash-set! (Env-table env) name val))
(define (env-set env name val)
  (match env
    ['() (error (format "can't set undefined symbol: ~s~n" name))]
    [(Env t penv)
      (if (hash-has-key? t name)
        (hash-set! (Env-table env) name val)
        (env-set penv name val))]))

; Kont Definition
(struct EvalArgs [args kont] #:prefab)
(struct EvalArgs1 [remind-args evaluated-args f kont] #:prefab)
(struct Apply [f evaluated-args kont] #:prefab)
(struct Return [env kont] #:prefab)
(struct Def [name kont] #:prefab)
(struct Begin [exprs kont] #:prefab)
(struct If [t f kont] #:prefab)
(struct Apply/CC [kont] #:prefab)
(struct Set! [name kont] #:prefab)

(struct Exp [exp] #:prefab)
(struct Closure (lam env) #:prefab)
(struct CC [env kont] #:prefab)

; step :: State -> State
(define (step state)
  (match state
    [(State (Exp `(lambda (,args ...) ,body ...)) env kont)
      (State (Closure `(lambda (,@args) (begin ,@body)) env) env kont)]

    [(State (Exp (? symbol? exp)) env kont)
      (State (env-lookup env exp) env kont)]

    [(State (Exp `(define (,name ,args ...) ,bodies ...)) env kont)
      (State (Exp `(define ,name (lambda (,@args) ,@bodies))) env kont)]

    [(State (Exp `(define ,name ,exp)) env kont)
      (State (Exp exp) env (Def name kont))]

    [(State (Exp `(begin)) env kont)
      (State (void) env kont)]
    [(State (Exp `(begin ,exp ,exps ...)) env kont)
      (State (Exp exp) env (Begin exps kont))]

    [(State (Exp `(if ,cond ,t ,f)) env kont)
      (State (Exp cond) env (If t f kont))]

    [(State (Exp `(quote ,exp)) env kont)
      (State exp env kont)]

    [(State (Exp `(set! ,name ,exp)) env kont)
      (State (Exp exp) env (Set! name kont))]

    [(State (Exp `(call-with-current-continuation ,f)) env kont)
      (State (Exp `(call/cc ,f)) env kont)]
    [(State (Exp `(call/cc ,f)) env kont)
      (State (Exp f) env (Apply/CC kont))]

    [(State (Exp `(,f ,args ...)) env kont)
      (State (Exp f) env (EvalArgs args kont))]

    ; Literal
    [(State (Exp v) env kont)
      (State v env kont)]

    ; Kont
    [(State v env (Set! name kont))
      (env-set env name v)
      (State v env kont)]

    [(State f env (Apply/CC kont))
      (State (void) env (Apply f (list (CC env kont)) kont))]

    [(State cond env (If t f kont))
      (State (Exp (if cond t f)) env kont)]

    [(State v env (Def name kont))
      (env-define env name v)
      (State (void) env kont)]

    [(State v env (Begin '() kont))
      (State v env kont)]
    [(State _ env (Begin `(,exp ,exps ...) kont))
      (State (Exp exp) env (Begin exps kont))]

    ; 函数调用-参数求值：0个参数
    [(State f env (EvalArgs '() kont))
      (State (void) env (Apply f '() kont))]
    ; 函数调用-参数求值：>=1个参数
    [(State f env (EvalArgs `(,arg ,args ...) kont))
      (State (Exp arg) env (EvalArgs1 args '() f kont))]
    [(State e-arg env (EvalArgs1 '() e-args f kont))
      (State (void) env (Apply f (reverse (cons e-arg e-args)) kont))]
    [(State e-arg env (EvalArgs1 `(,arg ,args ...) e-args f kont))
      (State (Exp arg) env (EvalArgs1 args (cons e-arg e-args) f kont))]

    ; call/cc
    [(State _ _ (Apply (CC env kont) `(,v) _))
      (State v env kont)]
    ; 函数调用 内置函数
    [(State _ env (Apply (? procedure? f) args kont))
      (State (apply f args) env kont)]
    ; 函数调用 用户自定义函数
    [(State _ env (Apply
                    (Closure
                        `(lambda (,f-args ...) ,body)
                      def-env) args kont))
      (let* ([bindings (map cons f-args args)])
        (State (Exp body) (make-env bindings def-env) (Return env kont)))]

    [(State e _ (Return env kont))
      (State e env kont)]

    [(State _ env (Apply f _ _))
      (error (format "application: not a procedure. ~n given: ~s~n state: ~s" f state))]

    [else (error "illegal state: ~s~n" state)]))


(define init-bindings
  (let* ([ns (make-base-namespace)]
          [sym-lst (namespace-mapped-symbols ns)]
          [bindings (map
                      (lambda (sym)
                        (let ([v (namespace-variable-value sym #t (lambda () null) ns)])
                          (cons sym v)))
                      sym-lst)]
          [procedure-bindings (filter
                                (lambda (pair)
                                  (procedure? (cdr pair)))
                                bindings)])
    procedure-bindings))

; (define init-bindings
;   (map (lambda (sym) (cons sym (eval sym)))
;        ;    '(+ - * =)))
;        '(number? integer? real? zero? + - * / < > <= >= =
;                  string? string substring string-length string-ref
;                  char? char->integer integer->char
;                  pair? list? null? list cons car cdr
;                  display void)))

;  boolean? and or not
;  eq? eqv? equal?)))

; inject :: Program -> State
(define (inject exp)
  (State (Exp exp) (make-env init-bindings) 'DONE))

; final? :: State -> Bool
(define (final? state)
  (match state
    [(State exp _ 'DONE)
      #:when (not (Exp? exp))
      #t]
    [else #f]))

; run :: (State -> State) -> (State -> Bool) -> State -> State
(define (run step final? state)
  (let ([next-state (step state)])
    (when DEBUG
      (display (format "~s~n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~n" state)))
    (cond
      [(final? next-state)
        (when DEBUG (display (format "~s~n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~n" next-state)))
        next-state]
      [else (run step final? next-state)])))

(define (interp exp)
  (when DEBUG
    (display (format "~ninterp ~s~n" exp)))
  (let ([final-state (run step final? (inject exp))])
    (State-exp final-state)))

(require rackunit rackunit/text-ui)

(define all-tests
  (test-suite
    "Tests"
    (test-case
      "symbol"
      (check-exn exn:fail? (lambda () (interp 'a)))
      (check-equal? (interp '(begin (define a 1) a)) 1))

    (test-case
      "lambda"
      (check-equal? (interp '((lambda (a) a) 2)) 2)
      (check-equal? (interp '(((lambda (a) (lambda (a) a)) 2) 3)) 3))

    (test-case
      "define"
      (check-equal? (interp '(begin
                               (define fac (lambda (n)
                                             (if (= n 1)
                                               1
                                               (* n (fac (- n 1))))))
                               (fac 5))) 120)
      (check-equal? (interp '(begin
                               (define (fac n)
                                 (if (= n 1)
                                   1
                                   (* n (fac (- n 1)))))
                               (fac 5))) 120)
      (check-equal? (interp '(begin
                               (define (fib n)
                                 (define (iter n a b)
                                   (if (= n 1)
                                     a
                                     (iter (- n 1) b (+ a b))))
                                 (iter n 0 1))
                               (fib 10))) 34))

    (test-case
      "begin"
      (check-equal? (interp '((begin
                                (define a 1)
                                (define b 2)
                                (lambda () (begin a b))))) 2))

    (test-case
      "procedure"
      (check-equal? (interp '(+ 1 2)) 3)
      (check-equal? (interp '(- 2 1)) 1)
      (check-equal? (interp '(list 1 2 3 4 5)) '(1 2 3 4 5))
      (check-equal? (interp '(cons 1 2)) '(1 . 2))
      (check-equal? (interp '(begin
                               (define (add a b) (+ a b))
                               (define (apply op a b) (op a b) (op a b))
                               (apply add 1 2))) 3))


    (test-case
      "quote"
      (check-equal? (interp '(quote (1 2 3))) '(1 2 3))
      (check-equal? (interp '(quote a)) 'a)
      (check-equal? (interp '(quote 1)) 1)
      (check-equal? (interp '(quote (quote 1))) '(quote 1)))

    (test-case
      "set!"
      (check-equal? (interp '(begin (define a 1) (set! a 2) a)) 2)
      (check-equal? (interp '(begin
                               (define (f) (set! a 2))
                               (define a 1)
                               (f)
                               a)) 2))

    (test-case
      "call/cc"
      (check-equal? (interp '(call/cc (lambda (cc) (cc 1)))) 1)
      (check-equal? (interp '(begin
                               (define (f return) (return 2) 3)
                               (call/cc f))) 2)
      (check-equal? (interp '(begin
                               (define (for-each f lst)
                                 (if (null? lst)
                                   (void)
                                   (begin
                                     (f (car lst))
                                     (for-each f (cdr lst)))))

                               (define (generate-one-element-at-a-time lst)
                                 (define (control-state return)
                                   (for-each
                                     (lambda (element)
                                       (set! return (call-with-current-continuation
                                                      (lambda (resume-here)
                                                        (set! control-state resume-here)
                                                        (return element)))))
                                     lst)
                                   (return 'you-fell-off-the-end))

                                 (define (generator)
                                   (call-with-current-continuation control-state))

                                 generator)

                               (define generate-digit
                                 (generate-one-element-at-a-time '(1 2 3)))

                               (list (generate-digit) (generate-digit) (generate-digit) (generate-digit)))) '(1 2 3 you-fell-off-the-end)))))


(define _ (run-tests all-tests 'verbose))
