#lang sicp

(define (display-interval x)
  (display "[")
  (display (lower-bound x))
  (display "/")
  (display (upper-bound x))
  (display "]"))

(define (display-interval-percent x)
  (display (center x))
  (display "+-")
  (display (percent x))
  (display "%"))

; 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (display "Division error (interval spans 0)")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
; 2.13
(define (make-center-percent c p)
  (let ((width (* (/ c 100) p)))
    (make-interval (- c width) (+ c width))))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (percent x)
  (let ((width (/ (- (upper-bound x) (lower-bound x)) 2)))
      (* (/ width (center x)) 100)))

(define 1-4 (list 1 2 3 4))

