#lang planet neil/sicp
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;;applicative-order evaluation
(test 0 (p))
(test 0 (p))
(test 0 (p))

;;normal-order evaluation
(test 0 (p))
(if (= 0 0) 0 (p))
0