#lang sicp
;(define (count-change amount) (cc amount 5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))
                    
                     

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 20 100 5 0.5 50 10 1 2))
(define eu-coins (list 100 50 20 10 5 2 1))

#|(define (first-denomination k)
  (cond ((= k 1) 1)
        ((= k 2) 5)
        ((= k 3) 10)
        ((= k 4) 25)
        ((= k 5) 50)))
|#