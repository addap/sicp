#lang sicp

  (define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "x = ")
      (display guess)
      (display " and f(x) = ")
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (average x y)
  (/ (+ x y) 2))

; 1.35
; x -> 1 + 1/x |*x
; x^2 -> x + 1

; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
; (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0)

; 1.36
; x^x = 1000 |log_x
; x = log_x(1000)
; x = log(1000)/log(x)
;
; (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)


; procedures as returned values
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (square x) (* x x))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (fourth-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y y))))
               1.0))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))


(define (newtons-method g guess)
  (define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

(define (sqrt3 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

; 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; 1.41
(define (double g)
  (lambda (x) (g (g x))))

; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43
(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f))

; proc's value is the repeated application of the function computed by f
(define (repeated-iter f n)
  (define (iter proc i)
    (if (= i n)
        proc
        (iter (compose proc f) (+ i 1))))
  (iter f 1))

; 1.44
(define (smooth f)
  (define (average x1 x2 x3) (/ (+ x1 x2 x3) 3))
  (define dx 0.00001)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; 1.45
