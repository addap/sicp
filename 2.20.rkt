#lang sicp

; 2.20
(define (same-parity a . b)
  (define (helper items)
    (if (null? items)
        nil
        (if (even? (+ a (car items)))
            (cons (car items) (helper (cdr items)))
            (helper (cdr items)))))
  (cons a (helper b)))