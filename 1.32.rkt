#lang sicp
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))
         
;(define (add a b) (+ a b))
;(define (mult a b) (* a b))
(define (identity x) x)

(define (sum a b)
  (accumulate-i + 0 identity a inc b))

(define (product a b)
  (accumulate * 1.0 identity a inc b))


  
