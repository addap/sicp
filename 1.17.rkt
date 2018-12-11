#lang racket
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (even? x)
  (= (remainder x 2) 0))

(define (fast-*1 a b)
  (cond ((or (= b 0) (= a 0)) 0)
        ((even? b) (double (fast-*1 a (halve b))))
        (else (+ a (fast-*1 a (- b 1))))))
  
(define (fast-*2 a b)
  (cond ((or (= b 0) (= a 0)) 0)
        ((= b 1) a)
        ((even? b) (fast-*2 (double a) (halve b)))
        (else (fast-*2 (double (+ a b)) (halve (- b 1))))))

; (f1 3 7)
; (+ 3 (f1 3 6))
; (+ 3 (double (f1 3 3)))
; (+ 3 (double (+ 3 (f1 3 2))))
; (+ 3 (double (+ 3 (double (f1 3 1)))))
; (+ 3 (double (+ 3 (double (+ 3 (f1 3 0))))))
; (+ 3 (double (+ 3 (double (+ 3 0)))))
; (21)