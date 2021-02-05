#lang racket
;(require racket/trace)
(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
           (cond
           ((zero? n) ls)
           (else (cdr (nth-cdr (sub1 n)))))
	   ;; complete the definition
           )))
      (car (nth-cdr n)))))

(define union
  (lambda (ls1 ls2)
    (letrec
      ((x
         (lambda (ls1)
           (cond
             ((null? ls1) ls2)
             ((memv (car ls1) ls2) (x (cdr ls1)))
             (else (cons (car ls1) (x (cdr ls1))))))))
      (x ls1))))

(define extend
  (lambda (x pred)
    (letrec
        ((func
          (lambda (n)
            (cond
            ((or (eqv? n x) (pred n)) #t)
            (else #f)))))
      func)))

(define walk-symbol
  (lambda (x s)
    (cond
      ((not (assv x s)) x)
      (else (walk-symbol (cdr (assv x s)) s)) )))

(define lambda->lumbda
  (lambda (e)
    (match e
      (`,y #:when (symbol? y) y)
      (`(lambda (,x), body) (list3 `lumbda (list1 x) (lambda->lumbda body)))
      (`(,e1,e2) (list2 (lambda->lumbda e1) (lambda->lumbda e2)))
      )))
(define list3
  (lambda (a b c)
    `(,a ,b ,c)))
(define list2
  (lambda (a b)
    `(,a ,b)))
(define list1
  (lambda (a)
    `(,a)))

(define var-occurs?
  (lambda (v e)
    (match e
      (`,y #:when (symbol? y) (eqv? y v))
      (`(lambda (,x) ,body) (var-occurs? v body))
      (`(,e1,e2) (or (var-occurs? v e1) (var-occurs? v e2)))
      )))

(define vars
  (lambda (e)
    (match e
      (`,y #:when (symbol? y) (list y))
      (`(lambda (,x), body) (vars body))
      (`(,e1,e2) (append (vars e1) (vars e2)))
      )))

(define unique-vars
  (lambda (e)
    (match e
      (`,y #:when (symbol? y) (list y))
      (`(lambda (,x), body) (unique-vars body))
      (`(,e1,e2) (union (unique-vars e1) (unique-vars e2)))
      )))

(define var-occurs-free?
  (lambda (v e )
    (letrec
        ((func
          (lambda (state e)
            (match e
              (`,y #:when (symbol? y)  (and (eqv? v y) (not state)))
              (`(lambda (,x) ,body)   (func (or (eqv? v x) state) body))
              (`(,rator ,rand) (or (func state rator) (func state rand)))))))
      (func #f e)) ))

(define var-occurs-bound?
  (lambda (v e )
   ; (define state #f)
    (letrec
        ((func
          (lambda (state e)
            (match e
              (`,y #:when (symbol? y)  (and (eqv? v y) state))
              (`(lambda (,x) ,body)   (func (or (eqv? v x) state) body))
              (`(,rator ,rand) (or (func state rator) (func state rand)))))))
      (func #f e)) ))


(define unique-free-vars
  (lambda (e)
    (letrec
        ((func
          (lambda ( var e)
            (match e
              ;(`,y #:when (symbol? y) (if (not (memv y bound-lst) (not (eqv? y var)) (list y) '()))
              (`,y #:when (symbol? y) (if (not (eqv? y var)) (list y) '()))
              (`(lambda (,x) ,body)    (remv x (func x body)))
              (`(,rator ,rand) (union (func var rator) (func var rand)))))))
      (func '4 e)) ))

(define unique-bound-vars
  (lambda (e)
    (letrec
        ((func
          (lambda ( var e)
            (match e
              (`,y #:when (symbol? y) (if (eqv? y var) (list var) '()))
              (`(lambda (,x) ,body)    (union (if (eqv? x var) '() (func var body)) (func x body)))
              (`(,rator ,rand) (union (func var rator) (func var rand)))))))
      (func '4 e)) ))


(define pos
  (lambda (ls y)
    (cond
      ((null? ls) y)
      ((eqv? (car ls) y) '(var 0))
      (else
       (cons 'var (map add1 (cdr (pos (cdr ls) y))))))
    ))

(define lex
  (lambda (e acc)
    (match e
      (`,y #:when (symbol? y) (pos acc y))
      (`(lambda (,x), body) (list2 `lambda (lex body (cons x acc))))
      (`(,e1,e2) (list2 (lex e1 acc) (lex e2 acc)))
      )))

(define walk-symbol-update
  (lambda (e ls)
    (match (assv e ls)
      (`,y #:when (boolean? y) (and (eq? y #f)  e))
      (`(,x . ,m) (and (set-box! m (walk-symbol-update (unbox m) ls) ) (unbox m)))
      )))


(define var-occurs-both?
  (lambda (v e )
   ; (define state #f)
    (letrec
        ((func
          (lambda (state e)
            (match e
              (`,y #:when (symbol? y)  (values (and (eqv? v y) (not state)) (and (eqv? v y) state)))
              (`(lambda (,x) ,body)   (func (or (eqv? v x) state) body))
              (`(,rator ,rand) (let-values([(f b) (func state rator)] [(f1 b1) (func state rand)])
                                 (values (or f f1) (or b b1))))
              ))))
      (func #f e)) ))
