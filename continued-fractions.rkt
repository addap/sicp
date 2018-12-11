#lang sicp

(define (cont-frac n d k)
  (cont-frac-iter n d k))
  
; k = 12 to be accurate to 4 decimal places
(define (cont-frac-rec n d k)
  (define (recursive i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (recursive (+ i 1))))))
  (recursive 1))

(define (cont-frac-iter n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i)
                 (+ result (d i)))
              (- i 1))))
  (iter 0 k))

(define (phi k)
  (/ 1
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))

; 1.38
(define (e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
                      (/ (+ i 1) 1.5)
                      1.0))
                k)))

; 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (* x x))))
             (lambda (i) (- (* 2.0 i) 1))
             k))
