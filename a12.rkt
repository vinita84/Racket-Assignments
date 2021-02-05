;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a12
;; -------------------------------------------------------------------
;; Grader: 
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
;; This might be me, don't worry about it. 
;; 
;; -------------------------------------------------------------------
;; --------------------
;; a12 > state/sum > state/sum 1
;; state/sum 1
;; ERROR
;; get-state: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 0
;; 
;; --------------------
;; --------------------
;; a12 > state/sum > state/sum 2
;; state/sum 2
;; ERROR
;; get-state: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 0
;; 
;; --------------------
;; --------------------
;; a12 > state/sum > state/sum 3
;; state/sum 3
;; ERROR
;; get-state: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 0
;; 
;; --------------------
;; --------------------
;; a12 > state/sum > state/sum 4
;; state/sum 4
;; ERROR
;; get-state: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 0
;; 
;; --------------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (define fact-5
;;   '((lambda (f) ((f f) 5))
;;     (lambda (f) (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))))
;; 
;; ---------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (redefine capture-fun1 '(* 3 (capture q (* 2 (return 4 q)))))
;; 
;; ---------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (redefine capture-fun2 '(* 2 (capture cc (* 5 (return (* 2 6) cc)))))
;; 
;; ---------------
;; --------------------
;; a12 > value-of-cps > value-of-cps 2
;; value-of-cps 2
;; ERROR
;; application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: 4
;;   arguments...:
;;    #<continuation>
;; 
;; --------------------
;; --------------------
;; a12 > value-of-cps > value-of-cps 3
;; value-of-cps 3
;; ERROR
;; application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: 12
;;   arguments...:
;;    #<continuation>
;; 
;; --------------------
;; --------------------
;; a12 > same-fringe > yield-cont undefined
;; yield-cont undefined
;; FAILURE
;; name:       check-not-exn
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:477:16
;; params:     #<procedure:temp379>
;; Check failure
;; --------------------
;; 26 success(es) 1 failure(s) 6 error(s) 33 test(s) run
#lang racket
(require "monads.rkt")
;(require racket/trace)
(define k (lambda(v) v))

(define assv-maybe
  (λ(x ls)
    (if (assv x ls)
        (return-maybe (cdr (assv x ls)))
        (fail))))


(define partition-writer
  (lambda (x ls)
    (cond
      [(null? ls) (return-writer '())]
      
      [(x (car ls))
       (bind-writer (tell-writer (car ls))
                    (lambda(a) (bind-writer (partition-writer x (cdr ls))
                                       (lambda(b) (return-writer b))
                                                    )))]
      [else
       (bind-writer (return-writer (car ls))
                    (λ(a) (bind-writer (partition-writer x (cdr ls))
                                       (λ(b) (return-writer (cons a b))))))]
      )))

(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 1)]
      ;[(= n 1) `(,x . (,x))]
      [(= n 1) (return-writer x)]
      [(odd? n) (bind-writer (return-writer (sub1 n)) (lambda(a) (bind-writer (powerXpartials x a) (lambda(b)  (cons (* x b) (return-writer b))))))]
      [(even? n) (bind-writer (return-writer (/ n 2)) (lambda(a) (bind-writer (powerXpartials x a) (lambda(b) (cons (* b b) (return-writer b))))))] )))

(define replace-with-count
  (lambda (x ls)
    (cond
      [(null? ls) (return-state '())]
      [(pair? (car ls))
       (do bind-state
         ;(s <- (get-state))
         ;(put-state s)
         (d <- (replace-with-count x (car ls)))
         (f <- (replace-with-count x (cdr ls)))
         (return-state (cons d f)))]
      [(equal? (car ls) x)
       (do bind-state
         (s <- (get-state))
         (put-state (add1 s))
         (d <- (replace-with-count x (cdr ls)))
         (return-state (cons s d)))]
      [else 
       (do bind-state
         ;(s <- (get-state))
         ;(put-state s)
         (d <- (replace-with-count x (cdr ls)))
         (return-state (cons (car ls) d)))])))

(define traverse
    (lambda (return bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))

(define reciprocal
  (lambda(x)
    (if (zero? x)
        (fail)
        ;(bind-writer (return-writer 1) (lambda (a) (bind-writer (return-writer x) (lambda(b) (return-writer (car `(1/,b)))))))
        ;(bind-maybe (return-maybe `(1/,x)) (lambda(a) (return-maybe a)))
        (return-maybe (/ 1 x))
        )))
       
(define traverse-reciprocal
    (traverse return-maybe bind-maybe reciprocal))

(define halve
  (lambda(x)
    (cond
      [(even? x) (bind-writer (return-writer x) (lambda(a) (return-writer (/ a 2 ))))]
      [else (bind-writer (tell-writer x) (lambda(a) (return-writer x)))] )))

(define traverse-halve
    (traverse return-writer bind-writer halve))

(define state/sum
  (lambda (x)
    (do bind-state
         (s <- (get-state))
         (put-state (+ x s))
        ; (d <- (get-state))
         ;(d <- (replace-with-count x (cdr ls)))
         (return-state s)) ))

(define traverse-state/sum
    (traverse return-state bind-state state/sum))

 (define fact-5
    '((lambda (f)
        ((f f) 5))
      (lambda (f)
        (lambda (n)
          (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

(define capture-fun
    '(* 3 (capture q (* 2 (return q 4)))))
 

(define value-of-cps 
  (lambda (expr env)
    
    (match expr
      [(? number?) (return-cont expr)]
      [(? boolean?) (return-cont expr)]      
      [(? symbol?) (return-cont (apply-env env expr k))]
      [`(* ,x1 ,x2) ((bind-cont (value-of-cps x1 env) apply-k) (*-outer-k x2 env k))]
      [`(sub1 ,x) ((bind-cont (value-of-cps x env) apply-k) (sub1-k k ))]
      [`(zero? ,x) ((bind-cont (value-of-cps x env) apply-k) (zero-k k ))]
      [`(if ,test ,conseq ,alt) ((bind-cont (value-of-cps test env) apply-k) (if-k conseq alt env k))]
      [`(capture ,k-id ,body) (return-cont (call/cc (lambda (k^)
                                         ((bind-cont (value-of-cps  body (ext-env k-id k^ env)) apply-k) k))))]
      [`(return ,k-exp ,v-exp) ((bind-cont (value-of-cps  k-exp env)  apply-k) (lambda(v) (bind-cont (value-of-cps  v-exp env) (lambda(d) (v d))) ))]
      [`(lambda (,id) ,body) (return-cont (closure id body env))]
      [`(,rator ,rand) ((bind-cont (value-of-cps rator env) apply-k) (rator-k rand env k))] )))

(define sub1-k
  (lambda (k^)
    `(sub1-k ,k^)))

(define zero-k
  (lambda(k^)
    `(zero-k ,k^)))

(define if-k
  (lambda (conseq^ alt^ env-cps^ k^)
    `(if-k ,conseq^ ,alt^ ,env-cps^ ,k^)))

(define *-inner-k
  (lambda (m^ k^)
    `(*-inner-k ,m^ ,k^)))

(define *-outer-k
  (lambda (x2^ env-cps^ k^)
    `(*-outer-k ,x2^ ,env-cps^ ,k^)))

(define rator-k
  (lambda(rand^ env-cps^ k^)
    `(rator-k ,rand^ ,env-cps^ ,k^)))
    
(define rand-k
  (lambda(c-cps^ k^)
    `(rand-k ,c-cps^ ,k^)))

(define apply-k
  (lambda (v)
    (lambda(k)
    (match k
      [`(empty-k) v]
      ;[`(zero-k ,k^) ((apply-k (zero? v)) k^ )]
      [`(zero-k ,k^) (return-cont (zero? v))]
      [`(sub1-k ,k^) (return-cont (sub1 v))]
      ;[`(sub1-k ,k^) ((apply-k (sub1 v)) k^)]
      [`(*-inner-k ,m^ ,k^) (return-cont (* m^ v))]
      [`(*-outer-k ,x2^ ,env-cps^ ,k^) ((bind-cont (value-of-cps x2^ env-cps^) apply-k) (*-inner-k v k^))]
      [`(rand-k ,c-cps^ ,k^) (return-cont (apply-proc c-cps^ v k^))]
      [`(rator-k ,rand^ ,env-cps^ ,k^) ((bind-cont (value-of-cps rand^ env-cps^) apply-k) (rand-k v k^))]
      [`(if-k ,conseq^ ,alt^ ,env-cps^ ,k^) (return-cont (if v ((bind-cont (value-of-cps conseq^ env-cps^) apply-k) k^) ((bind-cont (value-of-cps alt^ env-cps^) apply-k) k^)))]
      (else (k v))))))
 
;Closure helpers
(define closure
  (lambda(id^ body^ env-cps^)
    `(closure ,id^ ,body^ ,env-cps^)
      ))
                
(define apply-proc
  (lambda (c a k^)
    (match c
      [`(closure ,id^ ,body^ ,env-cps^) ((bind-cont (value-of-cps body^ (ext-env id^ a env-cps^)) apply-k) k^)]
      
      )))

;Environment Helpers
(define ext-env
  (lambda (id^ a^ env-cps^)
    `(ext-env ,id^ ,a^ ,env-cps^) ))
      
 
(define empty-env
  (lambda ()
    ;(lambda (y k^)
    ;  (error 'value-of "unbound identifier"))
    '(empty-env)
  ))

(define apply-env
  (lambda(env-cps y k^)
    (match env-cps
      [`(empty-env ) (error 'value-of-cps "unbound identifier")]
      ;[`(ext-env ,id^ ,a^ ,env-cps^) (if (eqv? id^ y) ((apply-k a^) k^) (apply-env env-cps^ y k^))]
      [`(ext-env ,id^ ,a^ ,env-cps^) (if (eqv? id^ y) ((apply-k a^) k^) (apply-env env-cps^ y k^))]
      )))





