#lang sicp

; 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (cons (car items) nil)
      (last-pair (cdr items))))

; 2.18
(define (reverse items)
  (define (helper items rev-items)
    (if (null? items)
        rev-items
        (helper (cdr items) (cons (car items) rev-items))))
  (helper items nil))

; (reverse (list 1 2 3))
; (cons 1 nil)
; (cons 2 (cons 1 nil))
; (cons 3 (cons 2 (cons 1 nil)))
; cdr -> nil -> stop