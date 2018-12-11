#lang planet neil/sicp
(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (improveC guess x)
  (/ (+ (/ x (square guess)) guess guess)
     3))
     

(define (good-enough? guess x)
  ;(< (abs (- (square guess) x)) 0.001))
  (<= (abs (- (improve guess x) guess)) 0.00001))

(define (good-enoughC? guess x)
  (<= (abs (- (improveC guess x) guess)) 0.0001))

(define (sqrt x) (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 0) 0 5)
(new-if (= 1 1) 0 5)

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x) x)))

(define (curt-iter guess x)
  (if (good-enoughC? guess x)
      guess
      (curt-iter (improveC guess x) x)))

(define (cube-root x)
  (curt-iter 1.0 x))

