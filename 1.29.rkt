#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (even? x)
  (= (remainder x 2) 0))

(define (s-integral f a b n)
  (define h (/ (- b a) n))
  (define (y-coef k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))          
  (define (y k) (* (y-coef k) (f (+ a (* k h)))))
  (* (/ h 3)
     (sum y 0 inc n)))

(define (cube x) (* x x x))
