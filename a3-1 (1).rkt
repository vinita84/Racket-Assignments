;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a3
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
;; Good work. Good work. I think there's a slightly cleaner way to do
;; what you're doing for implementing set!, but Dan should have shown
;; that off in class.
;; 
;; 
;; (define (apply-env-lex env num)
;;   (list-ref env num)
;;   )
;; (define (extend-env-lex a env)
;;   (cons a env)
;;   )
;; 
;; You could of course just define these as
;; 
;; (define extend-env-lex cons), etc.
;; 
;; -------------------------------------------------------------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (define value-of-lex
;;   (lambda (exp env)
;;     (match
;;      exp
;;      (`(const ,expr) expr)
;;      (`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env)))
;;      (`(zero ,x) (zero? (value-of-lex x env)))
;;      (`(sub1 ,body) (sub1 (value-of-lex body env)))
;;      (`(if ,t ,c ,a)
;;       (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
;;      (`(var ,num) (apply-env-lex env num))
;;      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
;;      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
;; 
;; ---------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (define empty-env-lex (lambda () '()))
;; 
;; ---------------
;; --------------------
;; A3 > church numerals > csub1 undefined
;; csub1 undefined
;; FAILURE
;; name:       check-not-exn
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:477:16
;; params:     #<procedure:temp694>
;; Check failure
;; --------------------
;; 61 success(es) 1 failure(s) 0 error(s) 62 test(s) run
#lang racket


(define value-of
  (lambda (e env)
    (match e
      (`,y #:when (symbol? y) (env y))
      (`,n #:when (number? n) n)
      (`,b #:when (boolean? b) b)
      (`(lambda (,x) ,body) (lambda (arg) (value-of body (lambda (y) (if (pair? y) (if (eqv? (car y) x) (set! arg (unbox (cdr y))) (apply-env-fn env y)) (if (eq? x y) arg ( env y)))))))
      (`(if ,test ,true-case ,false-case) (if (value-of test env) (value-of true-case env) (value-of false-case env)))
      (`(let ((,key ,val)) ,body) (let ([arg (value-of val env)]) (value-of body (lambda (y) (if (pair? y) (if (eqv? (car y) key) (set! arg (unbox (cdr y))) (apply-env-fn env y)) (if (eqv? key y) arg (env y)))))))
      (`(zero? ,v) (zero? (value-of v env)))
      (`(sub1 ,w) (sub1 (value-of w env))) 
      (`(,id . #&,bx) (apply-env-fn env (cons id (box bx))))
      (`(set! ,key ,val) (value-of (cons key (box (value-of val env))) env)) 
      (`(begin2 ,b-exp1 ,b-exp2) (begin (value-of b-exp1 env) (value-of  b-exp2 env)))
      (`(* ,m-exp1 ,m-exp2) (* (value-of m-exp1 env) (value-of m-exp2 env)))
      (`(+ ,n-exp1 ,n-exp2) (+ (value-of n-exp1 env) (value-of n-exp2 env)))
      (`(,rator ,rand) ((value-of rator env) (value-of rand env)))
      )))

(define value-of-fn
  (lambda (e env)
    (match e
      (`,y #:when (symbol? y) (apply-env-fn env y))
      (`,n #:when (number? n) n)
      (`,b #:when (boolean? b) b)
      (`(lambda (,x) ,body) (lambda (arg) (value-of-fn body (ext-env-fn x arg env))))
      (`(if ,test ,true-case ,false-case) (if (value-of-fn test env) (value-of-fn true-case env) (value-of-fn false-case env)))
      (`(let ((,key ,val)) ,body) (value-of-fn body (ext-env-fn key (value-of-fn val env) env)))
      (`(zero? ,v) (zero? (value-of-fn v env)))
      (`(sub1 ,w) (sub1 (value-of-fn w env)))
      (`(* ,m-exp1 ,m-exp2) (* (value-of-fn m-exp1 env) (value-of-fn m-exp2 env)))
      (`(+ ,n-exp1 ,n-exp2) (+ (value-of-fn n-exp1 env) (value-of-fn n-exp2 env)))
      (`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env)))
      )))

(define ext-env-fn
  (lambda (x a env)
     (lambda (y)
        (if (pair? y) (if (eqv? (car y) x) (set! a (unbox (cdr y))) (apply-env-fn env y))
            (if (eq? x y) a (apply-env-fn env y))))))

(define empty-env-fn
  (lambda () (lambda (y) (error 'value-of "unbounded variable ~s" y))))

(define apply-env-fn
  (lambda (env y)
    (env y)))



(define value-of-ds
  (lambda (e env)
    (match e
      (`,y #:when (symbol? y) (apply-env-ds env y))
      (`,n #:when (number? n) n)
      (`,b #:when (boolean? b) b)
      (`(lambda (,x) ,body) (lambda (arg) (value-of-ds body (ext-env-ds x arg env))))
      (`(if ,test ,true-case ,false-case) (if (value-of-ds test env) (value-of-ds true-case env) (value-of-ds false-case env)))
      (`(let ((,key ,val)) ,body) (value-of-ds body (ext-env-ds key (value-of-ds val env) env)))
      (`(zero? ,v) (zero? (value-of-ds v env)))
      (`(sub1 ,w) (sub1 (value-of-ds w env)))
      (`(* ,m-exp1 ,m-exp2) (* (value-of-ds m-exp1 env) (value-of-ds m-exp2 env)))
      (`(+ ,n-exp1 ,n-exp2) (+ (value-of-ds n-exp1 env) (value-of-ds n-exp2 env)))
      (`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env)))
      )))

(define empty-env-ds
  (lambda ()
    '(empty-env-ds)))

(define apply-env-ds
  (lambda (env y)
    (match env
      (`(empty-env-ds) 1)
      (`(ext-env-ds ,x ,a ,env) (if (eqv? y x) a (apply-env-ds env y))))))

(define ext-env-ds
  (lambda (x a env)
    `(ext-env-ds ,x ,a ,env)))

(define fo-eulav
  (lambda (e env)
    (match e
      (`,y #:when (symbol? y) (env y))
      (`,n #:when (number? n) n)
      (`,b #:when (boolean? b) b)
      (`(,body (,x) adbmal) (lambda (arg) (fo-eulav body (lambda (y) (if (eqv? x y) arg (env y))))))
      (`(,false-case ,true-case ,test fi) (if (fo-eulav test env) (fo-eulav true-case env) (fo-eulav false-case env)))
      (`(let ((,key ,val)) ,body) (fo-eulav body (lambda (y) (if (eqv? key y) (fo-eulav val env) (env y)))))
      (`(,v ?orez) (zero? (fo-eulav v env)))
      (`(,w 1bus) (sub1 (fo-eulav w env)))
      (`(,m-exp2 ,m-exp1 *) (* (fo-eulav m-exp1 env) (fo-eulav m-exp2 env)))
      (`(+ ,n-exp1 ,n-exp2) (+ (fo-eulav n-exp1 env) (fo-eulav n-exp2 env)))
      (`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env)))
      )))

(define empty-env
  (lambda () (lambda (y) (error 'value-of "unbounded variable ~s" y))))

(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))

(define empty-env-lex 
  (lambda () '()))

(define (apply-env-lex env num)
  (list-ref env num)
  )
(define (extend-env-lex a env)
  (cons a env)
  )
