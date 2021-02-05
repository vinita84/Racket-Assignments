;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a1
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
;; By the law of bad data, this shouldn't happen. Morevoer, it doesn't work as an error check, because of the natl-rec
;; ((null? ls) "Empty List")
;; 
;; Oohh, I see what you're doing there. Yeah, you don't *have* to do this on non-flat lists. I guess we should have specified that, but it seems odd to say "on a flat list" on every problem, rather than just specifying when we mean something special.
;; 
;;       ((null? ls) "Bad Data")
;;       ((null? (cdr ls)) -12340098)
;; 
;; These look like bad data cases. Which of course we don't have.
;; 
;;       ((null? ls) "Empty List")
;;       ((null? (cdr ls)) (cons (x (car ls)) '()))
;;       (else (cons (x (car ls)) (map x (cdr ls)))) 
;; 
;; You can simplify your control logic here. Treat the (null? (cdr ...))
;; case as any old recursive case, and then make (null? ls) return '().
;; 
;; Not only does this simplify the control logic, but it makes your program correct when someone maps over an empty list. Maybe not common, but if you (map (lambda (ls) (map add1 ls)) list-of-lists) ...
;; 
;; That's a similar comment throughout, so I'm not gonna stress it, but you see what we're about.
;; 
;; Wow. You powered through that powerset function. Did you read the suggeseted paper on it. It has (IMO) a really pretty simple solution, and one that works also. But lemme see if I can whip this up right quickly.
;; 
;; It's a straight natural recursion problem.
;; 
;; ((null? ls) '(()))
;; (else
;;   (append
;;     (map (lambda (s) (cons (car ls) s)) (powerset (cdr ls)))
;;     (powerset (cdr ls))))
;; 
;; If you feel like the two recursive calls is cheating, you can certainly
;; 
;; ((lambda (rec)
;;    (append (map (lambda (s) (cons (car ls) s)) rec) rec))
;;  (powerset (cdr ls)))
;; 
;; Which is of course just a let statement, right? 
;; 
;; 
;; So, if I have a set '(a b c d), I get to assume that I have the powerset of the set '(b c d). That's my assumption. Natl. recursion. So now what I want is all the ways I can include and exclude a. Well, if I cons it on to every element, I'll include it. And I need to append that list of things to the cases where I don't include it.
;; 
;; Yours might work. 
;; 
;; Does that make sense? I'm gonna leave you cartesian-product to play with too.
;; 
;; (λ (ls lso) (cons ls lso))
;; 
;; Hmm.. that looks like a function that takes two arguments and conses
;; them together. Can you think of a shorter way to write this function??
;; 
;; :-P
;; 
;; Good work on the collatz. 
;; 
;; This is good work here. I think now that we've got a way to give feedback, they'll be even better in the future.
;; 
;; 
;; 
;; 
;; -------------------------------------------------------------------
;; --------------------
;; A1: > remv-1st > remv-1st 3
;; remv-1st 3
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; "Empty List"
;; '()
;; expected:   '()
;; tested:     (remv-1st (quote x) (quote ()))
;; Check failure
;; --------------------
;; --------------------
;; A1: > filter > filter 4
;; filter 4
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; '(a b c (d e) f (g h))
;; '(a b c f)
;; expected:   '(a b c f)
;; tested:     (filter (lambda (a) (not (pair? a))) (quote (a b c (d e) f (g h))))
;; Check failure
;; --------------------
;; --------------------
;; A1: > map > map 2
;; map 2
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; "Empty List"
;; '()
;; expected:   '()
;; tested:     (map sub1 (quote ()))
;; Check failure
;; --------------------
;; --------------------
;; A1: > powerset-fr > powerset-fr 1
;; powerset-fr 1
;; ERROR
;; sort: contract violation
;;   expected: list?
;;   given: "Empty List"
;; 
;; --------------------
;; --------------------
;; A1: > powerset-fr > powerset-fr 2
;; powerset-fr 2
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; '((2 1 0) (2 1) (2 0) (2) (1 0) (1) (0))
;; '((2 1 0) (2 1) (2 0) (2) (1 0) (1) (0) ())
;; expected:   '((2 1 0) (2 1) (2 0) (2) (1 0) (1) (0) ())
;; tested:     (letrec ((<-set (lambda (s1 s2) (cond ((null? s1) #f) ((null? s2)) ((< (car s1) (car s2)) #f) ((< (car s2) (car s1))) (else (<-set (cdr s1) (cdr s2))))))) (sort (map (lambda (s) (sort s >)) (powerset-fr (quote (2 0 1)))) <-set))
;; Check failure
;; --------------------
;; --------------------
;; A1: > powerset-fr > powerset-fr 3
;; powerset-fr 3
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; '((5 4 3 1)
;;   (5 4 3)
;;   (5 4 1)
;;   (5 4)
;;   (5 3 1)
;;   (5 3)
;;   (5 1)
;;   (5)
;;   (4 3 1)
;;   (4 3)
;;   (4 1)
;;   (4)
;;   (3 1)
;;   (3)
;;   (1))
;; '((5 4 3 1)
;;   (5 4 3)
;;   (5 4 1)
;;   (5 4)
;;   (5 3 1)
;;   (5 3)
;;   (5 1)
;;   (5)
;;   (4 3 1)
;;   (4 3)
;;   (4 1)
;;   (4)
;;   (3 1)
;;   (3)
;;   (1)
;;   ())
;; expected:   '((5 4 3 1)
;;   (5 4 3)
;;   (5 4 1)
;;   (5 4)
;;   (5 3 1)
;;   (5 3)
;;   (5 1)
;;   (5)
;;   (4 3 1)
;;   (4 3)
;;   (4 1)
;;   (4)
;;   (3 1)
;;   (3)
;;   (1)
;;   ())
;; tested:     (letrec ((<-set (lambda (s1 s2) (cond ((null? s1) #f) ((null? s2)) ((< (car s1) (car s2)) #f) ((< (car s2) (car s1))) (else (<-set (cdr s1) (cdr s2))))))) (sort (map (lambda (s) (sort s >)) (powerset-fr (quote (1 3 4 5)))) <-set))
;; Check failure
;; --------------------
;; --------------------
;; A1: > quine > quine undefined
;; quine undefined
;; FAILURE
;; name:              check-not-exn
;; location:          /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:477:16
;; params:            #<procedure:temp1445>
;; message:           "Exception raised"
;; exception-message: "quine: undefined;\n cannot reference an identifier before its definition\n  in module: 'boolchandanivinita_6076746_64114356_a1"
;; exception:         #(struct:exn:fail:contract:variable "quine: undefined;\n cannot reference an identifier before its definition\n  in module: 'boolchandanivinita_6076746_64114356_a1" #<continuation-mark-set> quine)
;; Check failure
;; --------------------
;; 124 success(es) 6 failure(s) 1 error(s) 131 test(s) run
#lang racket
(define countdown
  (λ (n)
    (cond
      ((zero? n) (cons 0 '()))
      (else (cons n (countdown (sub1 n)) )) )))

(define insertR
  (λ (x y ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls)) (cons (insertR x y (car ls)) (insertR x y (cdr ls))))
      ((eqv? (car ls) x) (cons x (cons y (insertR x y (cdr ls)))))
      (else (cons (car ls) (insertR x y (cdr ls)))) )))


(define remv-1st
  (λ (x ls)
    (cond
      ((null? ls) "Empty List")
      ((pair? (car ls))
       (cond
         ((eqv? (remv-1st x (car ls)) (car ls)) (cons (car ls) (remv-1st x (cdr ls))))
         (else (cons (remv-1st x (car ls)) (cdr ls)))))
      ((eqv? (car ls) x) (cdr ls))
      ((null? (cdr ls)) (cons (car ls) '()))
      (else (cons (car ls) (remv-1st x (cdr ls)))) )))


(define list-index-ofv?
  (λ (x ls)
    (cond
      ((null? ls) "Bad Data")
      
      ((eqv? (car ls) x) 0 )
      ((null? (cdr ls)) -12340098)
      (else (add1 (list-index-ofv? x (cdr ls)))) )))


(define filter
  (λ (x ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls)) (cons (filter x (car ls)) (filter x (cdr ls))))
      ((x (car ls)) (cons (car ls) (filter x (cdr ls))))
      (else (filter x (cdr ls))) )))


(define zip
  (λ (lsa lsb)
    (cond
      ((null? lsa ) '())
      ((null? lsb) '())
      (else (cons (cons (car lsa) (car lsb)) (zip (cdr lsa) (cdr lsb)))) )))

(define map
  (λ (x ls)
    (cond
      ((null? ls) "Empty List")
      ;((pair? (car ls)) (cons (map x (car ls)) (map x (cdr ls))))
      ((null? (cdr ls)) (cons (x (car ls)) '()))
      (else (cons (x (car ls)) (map x (cdr ls)))) )))

(define append
  (λ (lsa lsb)
    (cond
      ((null? lsa) lsb)
      ((null? lsb) lsa)
      ((null? (cdr lsa)) (cons (car lsa) lsb))
      (else (cons (car lsa) (append (cdr lsa) lsb))) )))

(define reverse
  (λ (ls)
    
    (cond
      ((null? ls) "Empty list")
      ((null? (cdr ls)) (cons (car ls) '()))
      (else (append (reverse (cdr ls)) (cons (car ls) '()))) )))

(define fact
  (λ (n)
    (cond
      ((zero? n) 1)
      (else (* n (fact (sub1 n)))) )))

(define memv
  (λ (x ls)
    (cond
      ((null? ls) #f)
      ((pair? (car ls)) (cons (memv x (car ls)) (cdr ls)))
      ((eqv? (car ls) x) ls)
      (else (memv x (cdr ls))) )))

(define fib
  (λ (n)
    (cond
      ((zero? n) 0)
      ((eqv? n 1) 1)
      ((eqv? n 2) 1)
      (else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))) )))

; (equal? '((w x) y (z)) '((w . (x . ())) . (y . ((z) . () ))))

(define binary->natural
  (λ (ls)
    (cond
      ((not (pair? ls)) 0 )
      ((null? ls) 0)
      (else (+ (car ls) (* 2 (binary->natural (cdr ls))))) )))

(define minus
  (λ (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m)))) )))

(define div
  (λ (n m)
    (cond
      ((zero? m) "Bad Data")
      ((zero? n) 0)
      (else (add1 (div (- n m) m))) )))


(define append-map
  (λ (x ls)
    (cond
      ((null? ls) '())
      (else (append (x (car ls) ) (append-map x (cdr ls)))) )))
     ; (else (flatten (cons (x n) (append-map x (sub1 n))))) )))

(define occurs?
  (λ (x ls)
    (cond
      ((null? ls) #f)
      ((pair? (car ls)) (or (occurs? x (car ls)) (occurs? x (cdr ls))))
      ((eqv? (car ls) x) #t)
      (else (occurs? x (cdr ls))) )))

(define set-difference
  (λ (lsa lsb)
    (cond
      ((null? lsa) '())
      ((null? lsb) lsa)
      ((occurs? (car lsa) lsb) (set-difference (cdr lsa) lsb))
      (else (cons (car lsa) (set-difference (cdr lsa) lsb))) )))

(define append-each ;Used in Powerset function
  (λ (x ls)
  (cond
    ((null? ls) '())
    ((and (null? (cdr ls)) (= (length (car ls)) 0)) '(()))
   ; (cdr ls)
   (else (append (cons (sort (cons x (car ls)) >) (cons (car ls) '())) (append-each x (cdr ls))))) ))
   ; (else (append (cons (cons x (car ls)) '()) (append-each x (cdr ls))))) ))

(define powerset
  (λ (ls)
    (cond
      ((null? ls) '(()))
      ((not (pair? ls)) (append (cons (list ls) '()) '(())))
      
      ( (null? (cdr ls)) (cons ls '(())))
      (else
       ; (append (cons   (cons (car ls) '())   (cons (cdr ls) '()) ) ( append-each (car ls) (powerset (cdr ls))))) )))
       (append (cons   (cons (car ls) '()) '() ) ( append-each (car ls) (powerset (cdr ls))))) )))

(define cartesian-each
  (λ (x ls)
    (cond
      ((null? ls) '())
      ((eqv? ls '()) '())
      (else (cons (cons x (cons (car ls) '())) (cartesian-each x (cdr ls))) ) )))

(define cartesian-product
  (λ (ls)
    (define lsa (car ls))
    (define lsb (cdr ls))
    (cond
      ((eqv? lsa '()) lsb)
      ((null? lsb) lsa)
      ((null? (cdr lsa)) (cartesian-each (car lsa) (car lsb)))
      (else (append (cartesian-each (car lsa) (car lsb)) (cartesian-product (append (list (cdr lsa)) lsb)))) )))

(define insertR-fr
  (λ (x y lsi)
    (foldr (λ (ls lso)
             (cond
               ((eqv? ls x) (cons x (cons y lso)))
               (else
                (cons ls lso))))
           '() lsi )))

(define filter-fr
  (λ (x lsi)
    (foldr (λ (ls lso)
             (cond
               ((x ls) (cons ls lso))
               (else lso)))
           '() lsi)))

(define map-fr
  (λ (x lsi)
    (foldr (λ (ls lso)
             (cons (x ls) lso) )
           '() lsi)))
(define append-fr
  (λ (lsi x)
    (foldr (λ (ls lso)
             (cons ls lso) )
           x lsi) ))

(define reverse-fr
  (λ (lsi)
    (foldr (λ (ls lso)
             (append-fr lso (cons ls '())))
           '() lsi)))

(define binary->natural-fr
  (λ (lsi)
    
    (foldr (λ (ls lso)
             (+ (* lso 2) ls))
           0  lsi)))

(define append-map-fr
  (λ (x lsi)
    (foldr (λ (ls lso)
            (append (x ls) lso ))
          '() lsi ) ))

(define set-difference-fr
  (λ (lsa lsb)
    (foldr (λ (lsa lso)
             (cond
               ((not (occurs? lsa lsb)) (cons lsa lso))
               (else lso)))
           '() lsa)))

(define append-each-fr ;Used in Powerset-fr function
  (λ (x lsi)
    (foldr (λ (ls lso)
             (cond
              ; ((null? ls) (list '(()))  )
               ;((and (null? (cdr ls)) (= (length (car ls)) 0)) '(()))
               ; (else (append (cons (cons x ls) (cons ls '())) lso) )))
               (else (cons  (cons x ls)   lso) )))
           '() lsi )))



(define powerset-fr
  (λ (lsi)
       (foldr (λ (ls lso)
               (append (append (cons (cons ls '()) '())  (append-each-fr ls lso) ) lso))
              '() lsi)))

                     
(define cartesian-product-fr
  (λ (lsi)
    (foldr (λ (lsa lso)
           (append  (foldr (λ (lsb ls)
                      (append (list (cons lsa (cons lsb '()))) ls))
                    '() (car (cdr lsi)) ) lso))
           '() (car lsi))))

(define collatz 
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
      ;(define n (read))
     ; ( (or (odd-case collatz n) (even-case collatz n ) (one-case collatz n)) 1) ))

    (one-case (even-case (odd-case base))) ))
