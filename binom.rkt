#lang planet neil/sicp

(define (binom x y)
  (/ (factorial x)) (* (factorial (- x y)) (factorial y)))

(define (factorial x)
  (if (= x 1)
       1
       (* x (factorial (- x 1)))))