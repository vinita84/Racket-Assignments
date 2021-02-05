;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a7
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
;; I don't see what I need to tell you here. Seems like you got it. 
;; 
;; -------------------------------------------------------------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (define car$ car)
;; 
;; ---------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (define cdr$ (lambda ($) (force (cdr $))))
;; 
;; ---------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (define take$
;;   (lambda (n $)
;;     (cond ((zero? n) '()) (else (cons (car$ $) (take$ (sub1 n) (cdr$ $)))))))
;; 
;; ---------------
#lang racket

;Part I
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
               ((null? ls) ls)
               ((eqv? (car ls) 0) (k (last-non-zero (cdr ls))))
               (else (cons (car ls) (last-non-zero (cdr ls))))
	       ;; fill in lines here
  	       ))))
	(last-non-zero ls)))))

;Part II
(define lex
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) (list 'const n)]
      [`,y #:when (or (symbol? y)(eqv? y '!)) (apply-env-lex env y)]
      [`(,x ,x) #:when (and (symbol? x) (symbol? x)) (append `(app ,(lex x env) ,(lex x env)))]
      [`(zero? ,nexp) (append `(zero ,(lex nexp env) ) '())]
      [`(sub1 ,x) (list 'sub1 (lex x env))]
      [`(if ,t ,c ,a) (list 'if (lex t env) (lex c env) (lex a env))]
      [`(let ((,key ,val)) ,body) (let ([arg (lex val env)])
                                    (list 'let  arg (lex body (extend-env key env))))]
      [`(let/cc ,key ,body) (list 'letcc (lex body (extend-env key env)))]
      [`(throw ,key ,val) (list 'throw (lex key env) (lex val env))]
      [`(lambda(,x) ,body) (list 'lambda (lex body (extend-env x env)))]
      [`(* ,nexp1 ,nexp2) (append `(mult ,(lex nexp1 env) ,(lex nexp2 env)) '())]
      [`(,rator ,rand) (append `(app ,(lex rator env) ,(lex rand env)) '())]
      )))  

;Environment Helper Functions for lex
(define apply-env-lex (lambda (env id)
  (if (not (null? env)) (if (eqv? (car env) id) '(var 0)
      (cons 'var (list (add1 (car (cdr (apply-env-lex (cdr env) id))))))) '())
  ))

(define (extend-env id env)
  (cons id env)
  )

;CPS-ed Interpreter Part III
(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      
      [`(mult ,x1 ,x2) (value-of-cps x1 env-cps (*-outer-k x2 env-cps k))]
      [`(sub1 ,x) (value-of-cps x env-cps (sub1-k k ))]
      [`(zero ,x) (value-of-cps x env-cps (zero-k k ))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps (if-k conseq alt env-cps k))]
      [`(letcc ,body) (value-of-cps body (ext-env k env-cps k) k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env-cps (throw-outer-k v-exp env-cps k))]
      [`(let ,e ,body) (value-of-cps e env-cps (let-k body env-cps k))]
      [`(var ,expr) (apply-env env-cps expr k)]
      [`(lambda ,body) (apply-k k (make-closure body env-cps))]
      [`(app ,rator ,rand) (value-of-cps rator env-cps (rator-k rand env-cps k))] )))

;Continuation Constructors and Helpers
(define sub1-k
  (lambda (k^)
    `(sub1-k ,k^)))

(define zero-k
  (lambda(k^)
    `(zero-k ,k^)))

(define rator-k
  (lambda(rand^ env-cps^ k^)
    `(rator-k ,rand^ ,env-cps^ ,k^)))
    

(define rand-k
  (lambda(c-cps^ k^)
    `(rand-k ,c-cps^ ,k^)))

(define let-k
  (lambda (body^ env-cps^ k^)
    `(let-k ,body^ ,env-cps^ ,k^)))

          
(define throw-outer-k
  (lambda (v-exp^ env-cps^ k^)
    `(throw-outer-k ,v-exp^ ,env-cps^ ,k^) ))

(define throw-inner-k
  (lambda (k^)
    `(throw-inner-k ,k^)))

(define if-k
  (lambda (conseq^ alt^ env-cps^ k^)
    `(if-k ,conseq^ ,alt^ ,env-cps^ ,k^)))

(define *-inner-k
  (lambda (m^ k^)
    `(*-inner-k ,m^ ,k^)))

(define *-outer-k
  (lambda (x2^ env-cps^ k^)
    `(*-outer-k ,x2^ ,env-cps^ ,k^)))

(define empty-k
  (lambda ()
    `(empty-k)))

(define apply-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [`(zero-k ,k^) (apply-k k^ (zero? v))]
      [`(sub1-k ,k^) (apply-k k^ (sub1 v))]
      [`(*-inner-k ,m^ ,k^) (apply-k k^ (* m^ v))]
      [`(rand-k ,c-cps^ ,k^) (apply-closure c-cps^ v k^)]
      [`(rator-k ,rand^ ,env-cps^ ,k^) (value-of-cps rand^ env-cps^ (rand-k v k^))]
      [`(let-k ,body^ ,env-cps^ ,k^) (value-of-cps body^ (ext-env v env-cps^ k^) k^)]
      [`(throw-outer-k ,v-exp^ ,env-cps^ ,k^) (value-of-cps v-exp^ env-cps^ (throw-inner-k v ))]
      [`(*-outer-k ,x2^ ,env-cps^ ,k^) (value-of-cps x2^ env-cps^ (*-inner-k v k^))]
      [`(throw-inner-k ,k^) (apply-k k^ v)]
      [`(if-k ,conseq^ ,alt^ ,env-cps^ ,k^) (if v (value-of-cps conseq^ env-cps^ k^) (value-of-cps alt^ env-cps^ k^))]
      
      )))

;Closure helpers
(define make-closure
  (lambda(body^ env-cps^)
    `(closure ,body^ ,env-cps^)
      ))
                
(define apply-closure
  (lambda (c a k^)
    (match c
      [`(closure ,body^ ,env-cps^) (value-of-cps body^ (ext-env a env-cps^ k^) k^)]
      
      )))

;Environment Helpers
(define ext-env
  (lambda (a^ env-cps^ k)
    `(ext-env ,a^ ,env-cps^) ))
      
 
(define empty-env
  (lambda ()
    (lambda (y k^)
      (error 'value-of "unbound identifier"))))
 
(define apply-env
  (lambda(env-cps y k^)
    (match env-cps
      [`(empty-env ) (empty-env)]
      [`(ext-env ,a^ ,env-cps^) (if (zero? y) (apply-k k^ a^) (apply-env env-cps^ (sub1 y) k^))]
      )))


;Brain Teaser Part IV
(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
              (let ((n- (sub1 n)))
                (cond
                  ((zero? n-) '())
                  (else (take$ n- (cdr$ $))))))))))

(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))
 
(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))

(define tribo
  (lambda (trib$)
    (cons$ (+ (car$ trib$) (+ (car$ (cdr$ trib$)) (car$ (cdr$ (cdr$ trib$))))) (tribo (cdr$ trib$)))))

(define trib$ (cons$ 0 (cons$ 1 (cons$ 1 (tribo trib$)))))

         







         
