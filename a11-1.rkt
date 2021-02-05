;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a11
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
;;       ((fresh (ne1 ne2)
;;          (== `(* ,ne1 ,ne2) e)
;;          (== 'Int t)
;;          (!- G ne1 'Int)
;;          (!- G ne2 'Int)))
;; 
;; I think we haven't had folk type it at 'Int in a while ....
;; 
;; But it looks good. 
;; 
;; -------------------------------------------------------------------
;; --------------------
;; a11 > !- > !- 24
;; !- 24
;; ERROR
;; with-limit: out of time
;; 
;; --------------------
;; --------------------
;; a11 > !- > !- 38
;; !- 38
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; '()
;; '((pairof Nat Nat))
;; expected:   '((pairof Nat Nat))
;; tested:     (run* (q) (!- (quote ()) (quote (let ((f (lambda (x) x))) (if (f #t) (f (cons (f 4) 5)) (f (cons 5 (f 6)))))) q))
;; Check failure
;; --------------------
;; --------------------
;; a11 > !- > !- 65
;; !- 65
;; ERROR
;; with-limit: out of time
;; 
;; --------------------
;; 64 success(es) 1 failure(s) 2 error(s) 67 test(s) run
#lang racket
(require "mk.rkt")

(define apply-Go
  (lambda (G e t)
    (fresh (a G^)
      (== `(,a . ,G^) G)
      (fresh (aa da)
        (== `(,aa . ,da) a)
        (conde
          ((== aa e) (== da t))
          ((=/= aa e) (apply-Go G^ e t)))))))

(define !-
  (lambda (G e t)
    (conde
      ((numbero e) (== 'Nat t))
      ((== t 'Bool)
       (conde
         ((== #t e))
         ((== #f e))))
      ((fresh (ne1 ne2)
         (== `(+ ,ne1 ,ne2) e)
         (== 'Nat t)
         (!- G ne1 'Nat)
         (!- G ne2 'Nat)))
      ((fresh (ne1 ne2)
         (== `(* ,ne1 ,ne2) e)
         (== 'Int t)
         (!- G ne1 'Int)
         (!- G ne2 'Int)))
      ((fresh (ne1)
         (== `(sub1 ,ne1) e)
         (== 'Nat t)
         (!- G ne1 'Nat)))
      ((fresh (ne1)
         (== `(zero? ,ne1) e)
         (== 'Bool t)
         (!- G ne1 'Nat)))
      ((fresh (ne1)
         (== `(not ,ne1) e)
         (== 'Bool t)
         (!- G ne1 'Bool)))
      ((fresh (ne1 ne2)
         (== `(cons ,ne1 ,ne2) e)
         (fresh (ta tb)
          ;(== `(,ta . ,tb) t)
         (== `(pairof ,ta ,tb) t)
         (!- G ne1 ta)
         (!- G ne2 tb))))
      
     
     ((fresh (ne1)
         (== `(fix ,ne1) e)
         (fresh (ta tb tc td)
                (== `(,ta -> ,ta) t)
         (!- G ne1 `(,td -> ,tc)))))
        ; (!- G ne1 ta))))
     
     ((fresh (ne1)
         (== `(car ,ne1) e)
         (fresh (ta tb)
          (== ta t)
          (!- G ne1 `(pairof ,ta ,tb))
          )))
     ((fresh (ne1)
         (== `(cdr ,ne1) e)
         (fresh (ta tb)
          (== tb t)
          (!- G ne1 `(pairof ,ta ,tb))
          )))


    #|((symbolo e) (apply-Go G e t))
        ((fresh (x b)
        (== `(lambda (,x) ,b) e)
        (symbolo x)
        (fresh (tx tb tc)          
          (== `(,tx -> ,tb) t)
          (== `(Nat . ,tc) tx)
          (!- `((,x . ,tx) . ,G) b tb))))|#
      
      ((fresh (teste anse elsee)
        (== `(if ,teste ,anse ,elsee) e)
        (!- G teste 'Bool)
        (!- G anse t)
        (!- G elsee t)))
      ((symbolo e) (apply-Go G e t))
      ((fresh (x b)
        (== `(lambda (,x) ,b) e)
        (symbolo x)
        (fresh (tx tb)          
          (== `(,tx -> ,tb) t)
          (!- `((,x . ,tx) . ,G) b tb))))
          
      ((fresh (e1 arg)
        (== `(,e1 ,arg) e)
        (fresh (targ)
          (!- G e1 `(,targ -> ,t))
          (!- G arg targ)))))))