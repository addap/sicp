#lang sicp

; 2.24
; (list 1 (list 2 (list 3 4)))
; -> {1 {2 { 3 4}}}

; 2.25
; (1 3 (5 7) 9)
; -> (car (cdr (car (cdr (cdr list)))))
; ((7))
; -> (car (car list))
; (1 (2 (3 (4 (5 (6 7))))))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(define a (append x y))
(define b (cons x y))
(define c (list x y))

; a: (1 2 3 4 5 6)
; b: ((1 2 3)  4 5 6)
; c: ((1 2 3) (4 5 6))