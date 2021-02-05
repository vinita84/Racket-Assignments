;; -------------------------------------------------------------------
;; Username: 
;; Assignment: bonus
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
;; Slightly off on filter*, but as it was pointed out to me, you really
;; don't need SPS to solve this problem anyway, so it's not a great problem anyway. 
;; 
;; 
;; 
;; 
;; 
;; -------------------------------------------------------------------
;; --------------------
;; bonus > filter*-sps > filter*-sps 2
;; filter*-sps 2
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; '((((4 (4) 6 ((2)) (4) 6) 0) 2) 1 5 7 9)
;; '((((4 (4) 6 ((2)) (4) 6) 0) 2) (1 (() 5 (7 () 9) ())))
;; expected:   '((((4 (4) 6 ((2)) (4) 6) 0) 2) (1 (() 5 (7 () 9) ())))
;; tested:     (let-values (((a d) (filter*-sps even? (quote ((1 (4 (4) 5 6 (7 (2) 9) (4) 6) 0) 2)) (quote ())))) (cons a d))
;; Check failure
;; --------------------
;; --------------------
;; bonus > filter*-sps > filter*-sps 3
;; filter*-sps 3
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; '((1 (3 (5)) 7) 6 2 4)
;; '((1 (3 (5)) 7) (2 (4)) 6)
;; expected:   '((1 (3 (5)) 7) (2 (4)) 6)
;; tested:     (let-values (((a d) (filter*-sps odd? (quote (1 (2 3 (4 5)) 6 7)) (quote ())))) (cons a d))
;; Check failure
;; --------------------
;; --------------------
;; bonus > filter*-sps > filter*-sps 4
;; filter*-sps 4
;; FAILURE
;; name:       check
;; location:   /Users/jhemann/assignment-grading/autograde-lib/autograde.rkt:498:12
;; params:     #<procedure:equal?>
;; '(((2 (4)) 6 ((8 9) 10)) 1 7 3 5)
;; '(((2 (4)) 6 ((8 9) 10)) 1 (3 (5)) 7 (()))
;; expected:   '(((2 (4)) 6 ((8 9) 10)) 1 (3 (5)) 7 (()))
;; tested:     (let-values (((a d) (filter*-sps (lambda (x) (or (even? x) (< 7 x))) (quote (1 (2 3 (4 5)) 6 7 ((8 9) 10))) (quote ())))) (cons a d))
;; Check failure
;; --------------------
;; '(define-syntax copy-code (syntax-rules () ((_ x) `(,x x))))
;; '(define-syntax quote-quote (syntax-rules () ((_ e) ''e)))
;; 30 success(es) 3 failure(s) 0 error(s) 33 test(s) run
#lang racket
(require racket/trace)
(define fib-sps
  (lambda (n store)
    (cond
      [(assv n store) (values (cdr (assv n store)) store)]
      [(zero? n) (values 0 (cons `(0 . 0) store))]
      [(zero? (sub1 n)) (values 1 (cons `(1 . 1) store))]
      [else (let-values ([(sub2-result sub2-store) (fib-sps (sub1 (sub1 n)) store)])
              (let-values ([(sub1-result sub1-store) (fib-sps (sub1 n) sub2-store)])
              (let ([r (+ sub2-result sub1-result)])
                (values r (cons `(,n . ,r) sub1-store)))))])))



(define filter-sps
  (λ (x ls store)
    (cond
      ((null? ls) (values '() store))
      ;((pair? (car ls)) (cons (filter x (car ls)) (filter x (cdr ls))))
      ((x (car ls)) (let-values ([(res sub-store) (filter-sps x (cdr ls) store)])
              (values (cons (car ls) res) sub-store)))
      (else (let-values ([(res sub-store) (filter-sps x (cdr ls) store)])
              (values res (cons (car ls) sub-store)))))))

      ;((x (car ls)) (cons (car ls) (filter x (cdr ls))))
      ;(else (filter x (cdr ls))) )))

(define filter*-sps
  (lambda (f ls store)
    (cond
      [(null? ls) (values '() store)]
      [(pair? (car ls)) (let-values ([(res sub-store) (filter*-sps f (car ls) store)])
                          (let-values ([(res-1 sub-store-1) (filter*-sps f (cdr ls) sub-store)])
                            (values (cons res res-1) sub-store-1)))]
     ;  (cons (filter*-sps f (car ls)) (filter*-sps f (cdr ls)))]
      [(null? (car ls)) (values '() store)]
      [(f (car ls)) (let-values ([(res sub-store) (filter*-sps f (cdr ls) store)])
                      (values (cons (car ls) res) sub-store))]
    ;   (cons (car ls) (filter* f (cdr ls)))]
      [else (let-values ([(res sub-store) (filter*-sps f (cdr ls) store)])
              (values res (cons (car ls) sub-store  )))])))
      ; (filter* f (cdr ls))])))


(define-syntax and*-old
  (lambda(stx)
    (syntax-case stx ()
      [(and*) (syntax #t)]
      [(and* x) (syntax (if x x #f))]
      [(and* x rest ...)
       (syntax (if (not x)
                   #f
                   (and* rest ...)))])))

(define-syntax and*
  (syntax-rules ()
      [(and*) #t]
      [(and* x) (if x x #f)]
      [(and* x rest ...)
       (if (not x)
                   #f
                   (and* rest ...))]))


(define-syntax list*
  (lambda (stx)
    (syntax-case stx()
      [(list*) (raise-syntax-error "Incorrect argument-count to list*")]
      [(list* x) (syntax x)]
      [(list* x rest ...) (syntax (cons x (list* rest ...)))] )))

(define-syntax macro-list
  (lambda (stx)
    (syntax-case stx()
      [(macro-list) (syntax '())]
      [(macro-list x) (syntax `(,x))]
      [(macro-list x rest ...) (syntax (cons x (macro-list rest ...)))] )))


(define-syntax mcond
  (lambda (stx)
    (syntax-case stx()
      [(mcond (else x)) (syntax x)]
      [(mcond () (else x)) (syntax #t)]
      [(mcond (x rest ...)(else y)) (syntax (if x (mcond (rest ...) (else y)) y))]
      [(mcond (x rest ...)(y)) (syntax (if x (mcond (rest ...) (y)) y))]
      )))
      
(define-syntax quote-quote
    (syntax-rules ()
      [(_ e) (quote (quote e))]))
(define-syntax copy-code
    (syntax-rules ()
      [(_ x) `(,x x)]))

(define-syntax macro-map
  (lambda(stx)
    (syntax-case stx()
      ;[(macro-map m '(y rest ...)) (syntax (if (null? rest ...) (cons (m y) '()) (cons (m y) (macro-map m '(rest ...))) ))])))
     [(macro-map m '(y)) (syntax (cons (m y) '()))]
     [(macro-map m '(y rest ...)) (syntax (cons (m y) (macro-map m '(rest ...))))])))