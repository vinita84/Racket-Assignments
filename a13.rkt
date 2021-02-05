;; -------------------------------------------------------------------
;; Username: 
;; Assignment: a13
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
;; This all seems to work; I think once again my test program/framework
;; is being sassy. But good for me.
;; 
;; -------------------------------------------------------------------
;; program::-1: run: bad syntax
;;   in: (run 1 (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11) (fresh (ls d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11) (conde ((fresh (a) (membero (quasiquote (lindon unquote d1)) ls) (membero d1 e) (absento (quasiquote (eriador unquote d1)) ls) (absento (quasiquote (forodwaith un...
#lang racket
(require "mk.rkt")
(require "numbers.rkt")
(require racket/trace)
(define (listo s)
  (conde
   [( == s '())]
   ((fresh (x y)
   (== s `(,x . ,y)) (listo y)))))

(define (facto n o)
  
   (conde
   [(== '() n) (== '(1) o)]
   [(fresh (k p)
    ;(== (build-num n) n^)
   (minuso n '(1) k)
   (facto k p)
   (*o n p o))]))

(define fibs
    (lambda (n)
      (cond
        ((eqv? n 0) (values 1 1))
        (else
         (let ((n- (- n 1)))
           (let-values (((u v) (fibs n-)))
             (let ((u+v (+ u v)))
               (values v u+v))))))))

(define (fibso m n p)
  (conde
   [(== m '()) (== n '(1)) (== p '(1))]
   [(fresh (m- u v u+v)
           (minuso m '(1) m-)
           (fibso m- u n)
           (pluso u n p)
           )]))

(define lookup
  (trace-lambda (x vars vals o)
  (fresh (y vars^ a vals^)
    (== `(,vars^ . ,y) vars)
    (== `(,a . ,vals^) vals)
    (conde
      ((== x y) (== o a))
      ((=/= x y) (lookup x vars^ vals^ o))))))


(define list-evalo
  (lambda (es vars vals o)
    (conde
     [(== es '()) (== o '())]
     [(fresh (a d)
             (== es `(,a . ,d))
             (fresh (vs v)
                    (== o `(,vs  ,v))
                    (fo-lavo a vars vals vs)
                    (fo-lavo d vars vals v)
                     ))])))

(define (valof* es vars vals o)
  (conde
    [(== es `()) (== '() o)]
    [(fresh (e es^)
       (== es `(,e . ,es^))
       (fresh (v vs)
         (== `(,v . ,vs) o)
         (fo-lavo e vars vals v)
         (valof* es^ vars vals vs)))]))
             
(define fo-lavo
  (trace-lambda (e vars vals o)
    (conde
      [(symbolo e) (lookup e vars vals o)]
      [(== e `(,o etouq))
       (absento 'etouq vars)
       (absento 'closure e)]
     [(fresh (es)
              (== e `(,es tsil))
              (absento 'tsil vars)
              (fo-lavo es vars vals o))]
      [(fresh (es1 es)
              (==  e `(,es . ,es1))
              (=/= es1 'tsil)
              (list-evalo e vars vals o))]
      
      [(fresh (x b)
              (== e `(,b (,x) adbmal))
              (symbolo x)
              (absento 'adbmal vars)
              (== `(closure ,x ,b ,vars ,vals) o))]
      [(fresh (rator rand)
       (== e `(,rand ,rator))
       ;; (=/= rator 'quote)
       ;; (=/= rator 'list)
       (fresh (x b vars^ vals^ a)
              (fo-lavo rator vars vals `(closure ,x ,b ,vars^ ,vals^))
              (fo-lavo rand vars vals a)
              (fo-lavo b `(,x . ,vars^) `(,a . ,vals^) o)))])))

(define (reverseo ls out)
  (fresh (a d res)
  (conde
   [(== '() ls) (== out'())]
   [(== `(,a . ,d) ls) (reverseo d res) (appendo res `(,a) out)])))


(define membero
  (lambda(a ls)
    (fresh (s d)
    (conde
     [(== `(,a . ,s) ls)]
     [(== `(,s . ,d) ls) (membero a d)]))))
;(define color-middle-earth

(define color-middle-earth
  (trace-lambda (e)
  (run 1 (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11)
       (fresh (ls d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11)
             (conde
              [(fresh (a )
                     (membero `(lindon . ,d1) ls)
                     (membero d1 e)
                     (absento `(eriador . ,d1) ls)
                     (absento `(forodwaith . ,d1) ls) (== `(lindon . ,d1) q1)
              
                     (membero `(forodwaith . ,d2) ls)
                     (membero d2 e)
                     (absento `(lindon . ,d2) ls)
                     (absento `(rhovanion . ,d2) ls)
                     (absento `(eriador . d2) ls) (== `(forodwaith . ,d2) q2)

                     (membero `(eriador . ,d3) ls)
                     (membero d3 e)
                     (absento `(lindon . ,d3) ls)
                     (absento `(rhovanion . ,d3) ls)
                     (absento `(forodwaith . ,d3) ls)
                     (absento `(enedwaith . ,d3) ls)(== `(eriador . ,d3) q3)

                     (membero `(rhovanion . ,d4) ls)
                     (membero d4 e)
                     (absento `(rhun . ,d4) ls)
                     (absento `(rohan . ,d4) ls)
                     (absento `(forodwaith . ,d4) ls)
                     (absento `(enedwaith . ,d4) ls)
                     (absento `(eriador . ,d4) ls)(== `(rhovanion . ,d4) q4)

                     (membero `(enedwaith . ,d5) ls)
                     (membero d5 e)
                     (absento `(rohan . ,d5) ls)
                     (absento `(gondor . ,d5) ls)
                     (absento `(rhovanion . ,d5) ls)
                     (absento `(eriador . ,d5) ls)(== `(enedwaith . ,d5) q5)

                     (membero `(rohan . ,d6) ls)
                     (membero d6 e)
                     (absento `(rhun . ,d6) ls)
                     (absento `(mordor . ,d6) ls)
                     (absento `(gondor . ,d6) ls)
                     (absento `(rhovanion . ,d6) ls)
                     (absento `(enedwaith . ,d6) ls)(== `(rohan . ,d6) q6)

                     (membero `(gondor . ,d7) ls) ;gondor enedwaith rohan mordor
                     (membero d7 e)
                     (absento `(enedwaith . ,d7) ls)
                     (absento `(mordor . ,d7) ls)
                     (absento `(rohan . ,d7) ls) (== `(gondor . ,d7) q7)
              
                     (membero `(rhun . ,d8) ls) ;rhun rohan rhovanion khand mordor
                     (membero d8 e)
                     (absento `(rohan . ,d8) ls)
                     (absento `(rhovanion . ,d8) ls)
                     (absento `(khand . ,d8) ls)
                     (absento `(mordor . d8) ls) (== `(rhun . ,d8) q8)

                     (membero `(mordor . ,d9) ls) ;mordor gondor rohan rhun khand harad
                     (membero d9 e)
                     (absento `(gondor . ,d9) ls)
                     (absento `(rohan . ,d9) ls)
                     (absento `(rhun . ,d9) ls)
                     (absento `(khand . ,d9) ls)
                     (absento `(harad . ,d9) ls)(== `(mordor . ,d9) q9)

                     (membero `(khand . ,d10) ls) ;(khand mordor rhun harad)
                     (membero d10 e)
                     (absento `(mordor . ,d10) ls)
                     (absento `(harad . ,d10) ls)
                     (absento `(rhun . ,d10) ls)(== `(khand . ,d10) q10)

                     (membero `(harad . ,d11) ls) ;(harad mordor khand)
                     (membero d11 e)
                     (absento `(mordor . ,d11) ls)
                     (absento `(khand . ,d11) ls)(== `(harad . ,d11) q11))]
                     
             )))))
