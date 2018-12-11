#lang racket
(define (square n) (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

#;(define (expt b n)
  (expt-iter 1 b n))

(define (expt-iter a b n)
  (cond ((and (= n 0) (= b 0) -1)
         ((= n 0) 1)
         ((= b 0) 0))))

       ;#
         
       
       
        