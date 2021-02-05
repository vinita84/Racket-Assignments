;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a8
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
;; These are a little tricky, and pascal-reg especially so. Did you get
;; what it's supposed to be doing? It's a pretty neat little function.
;; 
;;                 (begin
;;                   (set! fact2 (lambda ()
;;                                 (cond
;;                                   [(zero? fact-n*) (begin  (set! fact-v* 1)  (fact-apply-k))]
;;                                   [else (begin (set! fact-k* (fact-inner-k fact-n* fact-k*)) (set! fact-n* (sub1 fact-n*)) (fact2))] )))
;;                   (fact2))
;; 
;; You can actually do an optimization here, and make the calls to fact2
;; just calls to fact, and then remove everything outside of the
;; cond. It's pretty neat.
;; 
;; There's a couple of places you can do that. 
;; 
;; -------------------------------------------------------------------
;; --------------------
;; a8 > pascal-reg-driver > pascal-reg-driver 2
;; pascal-reg-driver 2
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; #<procedure:pascal-v*>
;; '(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
;; expected:   '(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
;; tested:     (pascal-reg-driver 20)
;; Check failure
;; --------------------
;; --------------------
;; a8 > pascal-reg-driver > pascal-reg-driver 3
;; pascal-reg-driver 3
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; #<procedure:pascal-v*>
;; '(1)
;; expected:   '(1)
;; tested:     (pascal-reg-driver 1)
;; Check failure
;; --------------------
;; --------------------
;; a8 > bi-tramp-driver > bi-tramp-driver undefined
;; bi-tramp-driver undefined
;; FAILURE
;; name:              check-not-exn
;; location:          /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:477:16
;; params:            #<procedure:temp283>
;; message:           "Exception raised"
;; exception-message: "bi-tramp-driver: undefined;\n cannot reference an identifier before its definition\n  in module: 'boolchandanivinita_6076746_65404947_a8"
;; exception:         #(struct:exn:fail:contract:variable "bi-tramp-driver: undefined;\n cannot reference an identifier before its definition\n  in module: 'boolchandanivinita_6076746_65404947_a8" #<continuation-mark-set> bi-tramp-driver)
;; Check failure
;; --------------------
;; 22 success(es) 3 failure(s) 0 error(s) 25 test(s) run
#lang racket


(require racket/trace)
(define n* 0)
(define m* 0)
(define ack-k* '())
(define depth-k* '())
(define ack-v* 0)
(define depth-v* 0)
(define ls* '())
(define fact-n* 0)
(define fact-k* '())
(define fact2 '())
(define fact-v* '())
(define pascal-k* '())
(define pascal-n* 0)
(define pascal1 pascal-n*)
(define pascal-v* '())
(define pascal-m 1)
(define pascal-a 0)




(define empty-k
  (lambda ()
      `(empty-k) ))

(define ack-reg-driver
  (trace-lambda (m n)
    (begin
      (set! n* n)
      (set! m* m)
      (set! ack-k* (empty-k))
      (ack)) ))

(define ack
  (trace-lambda ()
    (cond
      [(zero? m*) (begin (set! ack-v* (add1 n*)) (ack-apply-k))]
      [(zero? n*) (begin (set! m* (sub1 m*)) (set! n* 1)
                        (ack))]
      [else (begin (set! ack-k* (inner-k m* ack-k*)) (set! n* (sub1 n*))  (ack))] )))


;lambda (v) (ack (sub1 m) v k))

(define inner-k
  (lambda (m^ k^)
    `(inner-k ,m^ ,k^)))

(define ack-apply-k
  (lambda()
    (match ack-k*
      [`(empty-k) ack-v*]
      [`(inner-k ,m^ ,k^) (begin (set! ack-k* k^) (set! n* ack-v*) (set! m* (sub1 m^)) (ack))] )))

;;------------------------------------------------------------------------------------------------------

(define depth-reg-driver
  (lambda(ls)
    (begin
      (set! ls* ls)
      (set! depth-k* (empty-k))
      (depth) )))

(define depth
  (trace-lambda ()
    (cond
      [(null? ls*) (begin (set! depth-v* 1) (depth-apply-k))]
      [(pair? (car ls*)) (begin (set! depth-k* (depth-outer-k ls* depth-k*)) (set! ls* (car ls*)) (depth))]
      [else (begin (set! ls* (cdr ls*)) (depth))] )))

(define depth-inner-k
  (lambda (v^ k^)
    `(depth-inner-k ,v^ ,k^) ))

(define depth-outer-k
  (lambda (ls^ k^)
    `(depth-outer-k ,ls^ ,k^) ))

(define depth-apply-k
  (lambda ()
    (match depth-k*
      [`(empty-k) depth-v*]
      [`(depth-inner-k ,v^ ,k^ ) (let ((l (add1 v^))) (if (< l depth-v*) (begin (set! depth-k* k^) (depth-apply-k)) (begin (set! depth-k* k^) (set! depth-v* l) (depth-apply-k))))]
      [`(depth-outer-k ,ls^ ,k^) (begin (set! depth-k* (depth-inner-k depth-v* k^)) (set! ls* (cdr ls^)) (depth))] )))

;;------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define fact-reg-driver
  (lambda(n )
    (begin
      (set! fact-n* n) (set! fact-k* (empty-k)) (fact))))

(define fact
  (trace-lambda ()
                (begin
                  (set! fact2 (lambda ()
                                (cond
                                  [(zero? fact-n*) (begin  (set! fact-v* 1)  (fact-apply-k))]
                                  [else (begin (set! fact-k* (fact-inner-k fact-n* fact-k*)) (set! fact-n* (sub1 fact-n*)) (fact2))] )))
                  (fact2)) ))
            
(define fact-inner-k
  (lambda(n^ k^)
    `(fact-inner-k ,n^ ,k^) ))

(define fact-apply-k
  (lambda ()
    (match fact-k*
      [`(empty-k) fact-v*]
      [`(fact-inner-k ,n^ ,k^)  (begin (set! fact-k* k^) (set! fact-v* (* n^ fact-v*)) (fact-apply-k))]
      )))
;;---------------------------------------------------------------------------------------------------------------------------------------------------------------

(define pascal-reg-driver
  (λ (n)
    (begin
      (set! pascal-k* (empty-k))
      (set! pascal-n* n)
      (pascal) )))
      
(define pascal
  (trace-lambda ()
                (begin
                  (set! pascal-k* (pascal-outermost-k pascal-k*))
                  (set! pascal
                        (lambda ()
                          (begin (set! pascal-v*
                                       (lambda ()
                                         (cond
                                           [(> pascal-m pascal-n*) (begin  (set! pascal-v*  '()) (pascal-apply-k))]
                                           [else (let ((pascal-a (+ pascal-a pascal-m)))
                                                   (begin (set! pascal-k* (pascal-outer-k pascal-m pascal-a pascal-k*))  (pascal)))]  )))
                                 (pascal-apply-k))))
                  (pascal))))

(define pascal-outermost-k
  (λ (k^)
    `(pascal-outermost-k ,k^) ))

(define pascal-inner-k
  (λ (a^ k^)
    `(pascal-inner-k ,a^ ,k^)))

(define pascal-outer-k
  (λ (m^ a^ k^)
    `(pascal-outer-k ,m^ ,a^ ,k^))) 

(define pascal-apply-k
  (trace-lambda ()
    (match pascal-k*
      [`(empty-k) pascal-v*]
      [`(pascal-inner-k ,a^ ,k^) (begin (set! pascal-k* k^) (set! pascal-v* (cons a^ pascal-v*)) (pascal-apply-k))]
      [`(pascal-outer-k ,m^ ,a^ ,k^) (begin (set! pascal-k* (pascal-inner-k a^ k^ )) (set! pascal-a a^) (set! pascal-m (add1 m^)) (pascal-v*))]
      [`(pascal-outermost-k ,k^) (begin (set! pascal-k* k^) (pascal-v*))]
      )))


;;-------------------------------------------------------------------------BRAIN TEASER---------------------------------------------------------------------------------------------

(define (ramp-empty-k j) `(empty-k ,j))
(define (ramp-outer-k n^ k^) `(outer-k ,n^ ,k^))
(define (ramp-inner-k v^ k^) `(inner-k ,v^ ,k^))

(define ramp-apply-k
  (λ (k v)
    (match k
      [`(empty-k ,j) (j v)]
      [`(outer-k ,n^ ,k^) (λ ()(fib (sub1 (sub1 n^)) (ramp-inner-k v k^)))]
      [`(inner-k ,v^ ,k^)  (λ () (ramp-apply-k k^ (+ v^ v)))] )))

(define fib
  (trace-lambda (n k)
    (cond
      [(and (not (negative? n)) (< n 2)) (lambda() (ramp-apply-k k n))]
      ;[else (fib (sub1 n) (lambda(v) (fib (sub1 (sub1 n)) (lambda(d) (k (+ v d))))))])))
      [else (lambda() (fib (sub1 n) (ramp-outer-k n k)))])))
;(fib 7 (lambda(v) v))



(define (rampoline th1 th2 th3)
  (rampoline (th2) th3 th1))

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (let/cc jumpout
      (rampoline
        (lambda ()
          (fib n1 (ramp-empty-k jumpout)))
        (lambda ()
          (fib n2 (ramp-empty-k jumpout)))
        (lambda ()
          (fib n3 (ramp-empty-k jumpout)))))))
