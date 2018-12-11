#lang sicp
(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a)
         (product term (next a) next b))))

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1.0))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (square x) (* x x))

; output is nan for n > 84
; because the product increases monotonically with n it becomes so big that
; racket cannot represent it and sets it to infinity thus the nan result
(define (pi-approx n)
  (define (plus2 x) (+ x 2))
  (define limit (* 2 n))
  (* 8 (/ (/ (product-i square 4 plus2 (+ limit 2))
             (+ limit 2))
          (product-i square 3 plus2 (+ limit 1)))))