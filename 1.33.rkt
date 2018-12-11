#lang sicp
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
          (combiner null-value (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (even? x)
  (= (remainder x 2) 0))

(define (prime? n)
  (define (smallest-divisor n) (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b) (= (remainder b a) 0))
  (define (next n) (if (= n 2) 3 (+ n 2)))                       
  (= n (smallest-divisor n)))

(define (square x) (* x x))

(define (rel-prime? i n)
  (= (gdc i n) 1))

(define (gdc a b)
  (if (= b 0)
      a
      (gdc b (remainder a b))))

(define (product-of-rel-primes n)
  (define (filter x)
    (rel-prime? x n))
  (filtered-accumulate filter * 1 (lambda (x) x) 1 inc n))

