#lang racket
;(define (inc n) (+ n 1))
;(define (dec n) (- n 1))

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;;; - 1 -
(+ 4 5)
(inc (+ (dec 4) 5)))
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
9

;;; - 2 -
(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
