;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a4
;; -------------------------------------------------------------------
;; Grader: jason
;; Grade: S+
;; -------------------------------------------------------------------
;; Scale:
;; S+	Great work.
;; S	Better than average.
;; S-	Below average. Contains a good deal of mistakes or
;; 	unattempted problems.
;; U	Unacceptable. Code which does not compile
;; 	in Racket and execute the student test
;; 	file without exception or warning receives a U.
;; N	Not turned in, or turned in after the due date
;; -------------------------------------------------------------------
;; Comments:
;; 
;; As you've figured out this unop/binop approach won't scale to the
;; final program.
;; 
;;         (if (pair? y) (if (eqv? (car y) x) (set! a (unbox (cdr y))) (apply-env env y))
;;             (if (eq? x y) a (apply-env env y))))))
;; 
;; 
;; You shouldn't need as much of this infrastructure as you have.
;; 
;;              [env env-empty]
;; 
;; You don't want to hardcode the env to be empty, which is what I think is going on here.
;; 
;; 
;; 
;; -------------------------------------------------------------------
;; poor-fact '(((lambda (f)
;;                 (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
;;               (lambda (f)
;;                 (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
;;              5)
#lang racket

(define lex
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) (list 'const n)]
      [`,y #:when (or (symbol? y)(eqv? y '!)) (apply-env-lex env y)]
      [`(,x ,x) #:when (and (symbol? x) (symbol? x)) (list (lex x env) (lex x env))]
      [`(zero? ,x) (list 'zero? (lex x env))]
      [`(sub1 ,x) (list 'sub1 (lex x env))]
      [`(if ,t ,c ,a) (list 'if (lex t env) (lex c env) (lex a env))]
      [`(let ((,key ,val)) ,body) (let ([arg (lex val env)])
                                    (list 'let  arg (lex body (extend-env key env))))]
      [`(lambda(,x) ,body) (list 'lambda (lex body (extend-env x env)))]
      [`(* ,m-exp1 ,m-exp2) (list '* (lex m-exp1 env) (lex m-exp2 env))]
      [`(,unop ,rand) (list (lex unop env) (lex rand env))]
      [`(,binop ,rand1 ,rand2) (list (lex binop env) (lex rand1 env) (lex rand2 env))]
      [`(,rator ,rand) (list (lex rator env) (lex rand env))] )))  

#|(define empty-env
  (lambda () '()))|#

(define apply-env-lex (lambda (env id)
  (if (not (null? env)) (if (eqv? (car env) id) '(var 0)
      (cons 'var (list (add1 (car (cdr (apply-env-lex (cdr env) id))))))) '())
  ))

(define (extend-env id env)
  (cons id env)
  )


;Data Structure independent interpreter
(define value-of-ds
  (lambda (e env)
    (match e
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x),body)(make-closure-ds x body env)]
      [`(if ,test ,true-case ,false-case) (if (value-of-ds test env) (value-of-ds true-case env) (value-of-ds false-case env))]
      [`(let ((,key ,val)) ,body) (let ([arg (value-of-ds val env)])
                                    (value-of-ds body (ext-env key arg env)))]
      [`(zero? ,v) (zero? (value-of-ds v env))]
      [`(sub1 ,w) (sub1 (value-of-ds w env))]
      [`(* ,m-exp1 ,m-exp2) (* (value-of-ds m-exp1 env) (value-of-ds m-exp2 env))]
      [`(+ ,n-exp1 ,n-exp2) (+ (value-of-ds n-exp1 env) (value-of-ds n-exp2 env))]
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))]
      )))

;Functionally independent interpreter
(define value-of-fn
  (lambda (e env)
    (match e
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,n #:when (number? n) n]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x),b) (make-closure-fn x b env)]
      [`(if ,test ,true-case ,false-case) (if (value-of-fn test env) (value-of-fn true-case env) (value-of-fn false-case env))]
      [`(let ((,key ,val)) ,body) (let ([arg (value-of-fn val env)])
                                    (value-of-fn body (ext-env key arg env)))]
      [`(zero? ,v) (zero? (value-of-fn v env))]
      [`(sub1 ,w) (sub1 (value-of-fn w env))]
      [`(* ,m-exp1 ,m-exp2) (* (value-of-fn m-exp1 env) (value-of-fn m-exp2 env))]
      [`(+ ,n-exp1 ,n-exp2) (+ (value-of-fn n-exp1 env) (value-of-fn n-exp2 env))]
      [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))]
      )))

;Generic Helper functions
(define ext-env
  (lambda (x a env)
     (lambda (y)
        (if (pair? y) (if (eqv? (car y) x) (set! a (unbox (cdr y))) (apply-env env y))
            (if (eq? x y) a (apply-env env y))))))

(define empty-env
  (lambda () (lambda (y) (error 'value-of "unbounded variable ~s" y))))

(define apply-env
  (lambda (env y)
    (env y)))

;functionally independent closures
(define apply-closure-fn
  (lambda (c a)
    (c a) ))

(define make-closure-fn
  (lambda (x b env)
    (lambda (arg)
      (value-of-fn b (ext-env x arg env)) )))

;data structure independent closures
(define apply-closure-ds
  (lambda (c a)
    (match c
      [`(closure,x,b,env) (value-of-ds b (ext-env x a env))] )))

(define make-closure-ds
  (lambda (x b env)
    `(closure ,x ,b ,env)))


(define value-of-dynamic  
  (lambda (exp env)
  (match exp
    [`,y #:when (symbol? y) (apply-env env y)]
    [`,n #:when (number? n) n]
    [`'() '()]
    [`(zero? ,v) (zero? (value-of-dynamic v env))]
    [`(null?, ls) (null? (value-of-dynamic ls env))]
    [`(sub1 ,w) (sub1 (value-of-dynamic w env))]
    [`(if ,test ,true-case ,false-case) (if (value-of-dynamic test env)
                                            (value-of-dynamic true-case env)
                                            (value-of-dynamic false-case env))]
    [`(let ((,key ,val)) ,body) (let (
                                      [arg (value-of-dynamic val env)])
                                  (value-of-dynamic body (ext-env key arg env)))]
    [`(cons ,l1 ,l2) (cons (value-of-dynamic l1 env ) (value-of-dynamic l2 env))]
    [`(car ,ls) (car (value-of-dynamic ls env))]
    [`(cdr ,ls) (cdr (value-of-dynamic ls env))]
    [`(lambda(,x) ,body) `(lambda(,x) ,body)]
    [`(,rator ,rand) (match-let (
                                 [`,arg (value-of-dynamic rand env)]
                                 [`(lambda (,x),body) (value-of-dynamic rator env)])
                       (value-of-dynamic body (ext-env x arg env)))]
    [`(* ,m-exp1 ,m-exp2) (* (value-of-dynamic m-exp1 env) (value-of-dynamic m-exp2 env))] )))



(define value-of-ri
  (lambda (env-empty env-extend env-apply closure-make closure-apply)
    (letrec (
             [env env-empty]
             [evaluator (lambda (exp)
                          (match exp
                          [`,y #:when (symbol? y) (env-apply env y)]
                          [`,n #:when (number? n) n]
                          [`,b #:when (boolean? b) b]
                          [`(lambda (,x),b) `(lambda(,x) ,b)]
                          [`(if ,test ,true-case ,false-case) (if (evaluator test)
                                                                  (evaluator true-case)
                                                                  (evaluator false-case))]
                          [`(let ((,key ,val)) ,body) (let (
                                                            [key (evaluator key)]
                                                            [val (evaluator val)])
                                                        (evaluator body))]
                          [`(zero? ,v) (zero? (evaluator v))]
                          [`(sub1 ,w) (sub1 (evaluator w))]
                          [`(* ,m-exp1 ,m-exp2) (* (evaluator m-exp1) (evaluator m-exp2))]
                          [`(+ ,n-exp1 ,n-exp2) (+ (evaluator n-exp1) (evaluator n-exp2))]
                          
                          [`(,rator ,rand) (match-let (
                                                       [`,arg (evaluator rand)]
                                                       [`(lambda (,x),body) (evaluator rator)])
                                             (begin
                                               (set! env (closure-apply (closure-make x body env env-extend) arg))
                                               (evaluator body)) )] ))])
      evaluator )))

;functionally independent closures
(define apply-closure-fn-ri
  (lambda (c a)
    (c a) ))

(define closure-fn-ri
  (lambda (x b env extend-env)
    (lambda (arg)
       (extend-env x arg env) )))
      
;data structure independent closures
(define apply-closure-ds-ri
  (lambda (c a )
    (match c
      [`(closure,x,b,env,ext-env) (ext-env x a env)] )))

(define closure-ds-ri
  (lambda (x b env ext-env)
    `(closure ,x ,b ,env ,ext-env)))

;Helper functions
(define extend-env-fn
  (lambda (x a env)
     (lambda (y)
        (if (eq? x y) a (apply-env env y)))))

(define empty-env-fn
  (lambda () (lambda (y) (error 'value-of "unbounded variable ~s" y))))

(define apply-env-fn
  (lambda (env y)
    (env y)))

(define empty-env-ds
  (lambda ()
    '(empty-env-ds)))

(define apply-env-ds
  (lambda (env y)
    (match env
      [`(empty-env-ds) 1]
      [`(ext-env-ds ,x ,a ,env) (if (eqv? y x) a (apply-env-ds env y))] )))

(define extend-env-ds
  (lambda (x a env)
    `(ext-env-ds ,x ,a ,env)))





