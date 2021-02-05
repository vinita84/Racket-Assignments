;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a6
;; -------------------------------------------------------------------
;; Grader: jason
;; Grade: S
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
;;                              [else (bin-to-dec (cdr n) (lambda(v) (+ (car n) (* 2 (k v))) ))]
;; 
;; You wrap k too early here. The entire expression is simple.
;; 
;; You also have in the plus-cps, missing a continuation application to
;; the inner function. You wanna have a single flow of control (snake).
;; 
;;     ((lambda (f)
;;        (f f n k))
;;      (lambda (g n k)
;;        (cond
;; 	 [(zero? n) (k 0)]
;; 	 [(= 1 n) (k 1)] 
;; 	 [else (g g (sub1 n) (lambda (v) (g g (sub1 (sub1 n)) (lambda(d) (k (+ d v)))))) ] )))
;; 
;; Here, too, in principle you need an additional parameter k and in that parameter list you need an f and a k.
;; 
;;        ((h h) seed '() k)
;; 
;; serious call in non-tail position (h h)
;; 
;; ((M-cps
;; 
;; is also a serious non-tail call
;; 
;; There are more things here too, I just wanted to make sure I mentioned one of each. 
;; 
;; 
;; -------------------------------------------------------------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (redefine
;;  binary-to-decimal-cps-driver
;;  (lambda (ls) (binary-to-decimal-cps ls (empty-k))))
;; 
;; ---------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (define empty-k
;;   (lambda ()
;;     (let ((once-only #f))
;;       (lambda (v)
;;         (if once-only
;;           (error 'empty-k "You can only invoke the empty continuation once")
;;           (begin (set! once-only #t) v))))))
;; 
;; ---------------
;; --------------------
;; A6 > plus-cps > plus-cps 1
;; plus-cps 1
;; ERROR
;; plus-cps: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 2
;;   arguments...:
;;    2
;;    #<procedure>
;; 
;; --------------------
;; --------------------
;; A6 > plus-cps > plus-cps 2
;; plus-cps 2
;; ERROR
;; plus-cps: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 2
;;   arguments...:
;;    5
;;    #<procedure>
;; 
;; --------------------
;; --------------------
;; A6 > plus-cps > plus-cps 3
;; plus-cps 3
;; ERROR
;; plus-cps: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 2
;;   arguments...:
;;    0
;;    #<procedure>
;; 
;; --------------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (redefine
;;  remv-first-9*-cps-driver
;;  (lambda (ls) (remv-first-9*-cps ls (empty-k))))
;; 
;; ---------------
;; --------------------
;; A6 > count-syms*-cps > count-syms*-cps undefined
;; count-syms*-cps undefined
;; FAILURE
;; name:              check-not-exn
;; location:          /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:477:16
;; params:            #<procedure:temp307>
;; message:           "Exception raised"
;; exception-message: "count-syms*-cps: undefined;\n cannot reference an identifier before its definition\n  in module: 'boolchandanivinita_late_6076746_65113905_a6-3"
;; exception:         #(struct:exn:fail:contract:variable "count-syms*-cps: undefined;\n cannot reference an identifier before its definition\n  in module: 'boolchandanivinita_late_6076746_65113905_a6-3" #<continuation-mark-set> count-syms*-cps)
;; Check failure
;; --------------------
;; --------------------
;; A6 > M-cps > M-cps 1
;; M-cps 1
;; ERROR
;; M-cps: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 2
;;   arguments...:
;;    #<procedure>
;;    #<procedure>
;; 
;; --------------------
;; --------------------
;; A6 > M-cps > M-cps 2
;; M-cps 2
;; ERROR
;; M-cps: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 2
;;   arguments...:
;;    #<procedure>
;;    #<procedure>
;; 
;; --------------------
;; --------------------
;; A6 > M-cps > M-cps 3
;; M-cps 3
;; ERROR
;; M-cps: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 2
;;   arguments...:
;;    #<procedure>
;;    #<procedure>
;; 
;; --------------------
;; --------------------
;; A6 > M-cps > M-cps 4
;; M-cps 4
;; ERROR
;; M-cps: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 1
;;   given: 2
;;   arguments...:
;;    #<procedure>
;;    #<procedure>
;; 
;; --------------------
;; --------------------
;; A6 > use-of-strange-cps > use-of-strange-cps 1
;; use-of-strange-cps 1
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; #f
;; #t
;; expected:   #t
;; tested:     (procedure? use-of-strange-cps)
;; Check failure
;; --------------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (redefine
;;  testing-almost-length-cps
;;  (lambda (f k)
;;    (k
;;     (lambda (ls k)
;;       (if (null? ls) (k 0) (f (cdr ls) (lambda (v) (k (add1 v)))))))))
;; 
;; ---------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (redefine empty-l (lambda () (lambda (v) (v '(1 2 3 4 5) (empty-k)))))
;; 
;; ---------------
;; --------------------
;; A6 > why-cps > why-cps 1
;; why-cps 1
;; ERROR
;; add1: contract violation
;;   expected: number?
;;   given: #<procedure>
;; 
;; --------------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (redefine
;;  testing-almost-length-cps-cps
;;  (lambda (f c k)
;;    (c
;;     (lambda (ls c k)
;;       (if (null? ls) (c 0 k) (f (cdr ls) (lambda (v k) (c (add1 v) k)) k)))
;;     k)))
;; 
;; ---------------
;; ---------------
;; We have added or changed the following definition:
;; 
;; (redefine
;;  empty-c
;;  (lambda () (lambda (v k) (v '(1 2 3 4 5) (lambda (v k) (k v)) k))))
;; 
;; ---------------
;; --------------------
;; A6 > why-cps-cps > why-cps-cps undefined
;; why-cps-cps undefined
;; FAILURE
;; name:              check-not-exn
;; location:          /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:477:16
;; params:            #<procedure:temp919>
;; message:           "Exception raised"
;; exception-message: "why-cps-cps: undefined;\n cannot reference an identifier before its definition\n  in module: 'boolchandanivinita_late_6076746_65113905_a6-3"
;; exception:         #(struct:exn:fail:contract:variable "why-cps-cps: undefined;\n cannot reference an identifier before its definition\n  in module: 'boolchandanivinita_late_6076746_65113905_a6-3" #<continuation-mark-set> why-cps-cps)
;; Check failure
;; --------------------
;; 64 success(es) 3 failure(s) 8 error(s) 75 test(s) run
#lang racket
(require racket/trace)
(define binary-to-decimal-cps
  (lambda (n k)
    (letrec ([bin-to-dec (lambda (n k)
                           (cond
                             [(null? n) (k 0)]
                             [else (bin-to-dec (cdr n) (lambda(v) (+ (car n) (* 2 (k v))) ))])) ])
      (bin-to-dec (reverse n) k)
     )))

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

;(binary-to-decimal '(1 1 0 1) (empty-k))

(define times-cps 
  (lambda (ls k)
    (cond 
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (v) (* (car ls) (k v))))] )))

;(times-cps '(1 2 3 4 5) (empty-k))  

(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls) (lambda (v) (* (car ls) (k v))))] )))

(define plus-cps
  (lambda (m)
    (lambda (n)
                  (letrec ([plus-cps (lambda (m k)
                                        (cond
                                          [(zero? m) (k n)]
                                          [else (plus-cps (sub1 m) (lambda (v) (add1 (k v)))) ] ))])
                    (plus-cps m (empty-k)) ))))



(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps (car ls) (lambda (v) (cond
                                               [(equal? v (car ls)) (remv-first-9*-cps (cdr ls) (lambda (v) (k (cons (car ls) v)))) ]
       [else (remv-first-9*-cps (car ls) (lambda (v) (k (cons v (cdr ls)))))] )))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else  (remv-first-9*-cps (cdr ls) (lambda (v) (k (cons (car ls) v))))] )))
;(remv-first-9* '((1 2 (3) 9)) (empty-k))


(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls) (lambda (v) (cons-cell-count-cps (cdr ls) (lambda (a) (k (add1 (+ v  a)))))))]
      [else (k 0)])))

;(cons-cell-count-cps '((1 2) 3 4) (empty-k))



;(find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))

(define find-cps 
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s (lambda(v) (k v))) (k u)))))



(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda(v) (ack-cps (sub1 m) v k)) )])))

(define fib-cps
  (lambda (n k)
    ((lambda (f)
       (f f n k))
     (lambda (g n k)
       (cond
	 [(zero? n) (k 0)]
	 [(= 1 n) (k 1)] 
	 [else (g g (sub1 n) (lambda (v) (g g (sub1 (sub1 n)) (lambda(d) (k (+ d v)))))) ] )))))

(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h)
       ((h h) seed '() k))
     (lambda (h)
       (lambda (seed ans k)
         (p seed (lambda(n) (if n
             (k ans)
             (g seed (lambda(m) ((h h) m ans (lambda(v) (f seed (lambda(d) (cons d (k v))))))))))))))))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))

(define car-cps
    (lambda (pr k)
      (k (car pr))))

(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

(define empty-s
  (lambda ()
    '()))
 
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (if (pair? v)
           (find-cps (car u) s (lambda(m) (find-cps (car v) s (lambda(n) (unify-cps m n s (lambda(v^) (if v^ (find-cps (cdr u) v^ (lambda(m) (find-cps (cdr v) v^ (lambda(n) (unify-cps m n v^ (lambda(d) (k d))))))) (k #f))))))))
	   (k #f)))
      (else (k #f)))))


(define M-cps
  (lambda (f)
    (lambda (ls k)
      (cond
        ((null? ls) (k '()))
        (else (f (car ls)  (lambda(v) (cons v ((M-cps f) (cdr ls) (lambda(m) (k m)))))))))))

(define add1-cps
  (lambda (n k)
    (k (add1 n))))


(define use-of-M-cps
 ((M-cps (lambda (n k) (add1-cps n k))) '(1 2 3 4 5) (empty-k)))


(define strange-cps
  (trace-lambda (x k)
    ((trace-lambda (g k) (k (trace-lambda (x k) (g g k))))
     (trace-lambda (g k) (k (trace-lambda (x k) (g g k)))) k)))


(define use-of-strange-cps
  (let ([strange^ (strange-cps 5 (trace-lambda(g) (g 6 (trace-lambda(v) (v 7 (trace-lambda(d) d))))))]) 
    (strange^ 8 (trace-lambda(g) (g 9 (trace-lambda(v) (v 10 (trace-lambda(d) 'cps-ed_fine))))))))




(define why-cps
  (trace-lambda (f k)
    ((trace-lambda (g k)
       (f (trace-lambda (x k) ((g g k) x)) k))
     (lambda (g k)
       (f (trace-lambda (x k) ((g g k) x)) k)) k)))

(define almost-length-cps
    (trace-lambda (f-cps k)
      (trace-lambda (ls)
        (if (null? ls)
           (k 0)
            (f-cps (cdr ls) (trace-lambda(v) (add1 (k v))))))))












