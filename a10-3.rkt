;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a10
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
;; stream is the wrong word there. Constraints represent restrictions on acceptable subsets of the m.g.u.
;; 
;; 
;; 
;; 
;; 
;; -------------------------------------------------------------------
#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))

#| ANS:'(5). EXPLAINATION: initially, q is associated with 5 in the second statement. Then the inner conde associated q with 5 and then with 6. Since this is logically inconsistent, the first statement of inner conde returns an empty list. the second statement of inner conde associates q with 5 which is acceptable. then the second statement of outer conde associates q with 5 aagain. hence the output is '(5) which is the association of q.|#

;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))

#| ANS: '(((_.0 _.1)
   (=/= ((_.0 tag))) 
   (sym _.0)
   (absento (tag _.1))))
 EXPLAINATION: (_.0 _.1) shows that q remains unbound and has been associated with a list of a and b. (=/= ((_.0 tag))) clause is the disequality constraint which implies that 'a' cannot be associated with 'tag' while being the car of q. and (sym _.0) is the symbolo constraint which implies that 'a' has to be a symbol. the constraint (absento (tag _.1)) implies 'b' cannot be associated with 'tag or a stream containing tag while being the cdr of q  |#

;; 3 What do the following miniKanren constraints mean?
;; a == associates 'a' with a value which is the second operand
;; b =/= disequality constraint and restrictes b from association with something which is the second operand
;; c absento is an absent constraint that takes 'tag and t as operands and restricts t from containing 'tag anywhere in it 
;; d numbero constraints the variable to be number only
;; e symbolo constraints the variable to be symbol only.

;; Part II goes here.


(define (assoco x ls out)
  
   (fresh (a d aa da)
          (== `(,a . ,d) ls)
          (== `(,aa . ,da) a)
          (conde
           ((== aa x) (== a out))
           ((=/= aa x) (assoco x d out)))))


(define (reverseo ls out)
  (fresh (a d res)
  (conde
   ((== '() ls) (== out'()))
   
          
           ((== `(,a . ,d) ls) (reverseo d res) (appendo res `(,a) out)))))

(define (stuttero ls out)
  (fresh (a d res)
    (conde
      ((== '() ls) (== out '()))
      ((== `(,a . ,d) ls)  (== `(,a ,a . ,res) out) (stuttero d res)))))


(define (lengtho ls out)
  (fresh (a d res-a res-d)
  (conde
   ((== '() ls) (== out '()))
   ((== `(,a . ,d) ls) (lengtho d res-d) (addero 0 '(1) res-d out)))))
