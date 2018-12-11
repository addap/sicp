#lang racket
(define (pascal r c)
  (if (or (= c 0) (= c r))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))