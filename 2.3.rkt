#lang sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display "|")
  (display (y-point p))
  (display ")"))

(define (average a b) (/ (+ a b) 2))
(define (average-point p1 p2)
  (make-point (average (x-point p1) (x-point p2))
              (average (y-point p1) (y-point p2))))

(define (midpoint-segment s)
  (average-point (start-segment s)
                 (end-segment s)))

(define (make-rec-points lower-left upper-right)
  (cons lower-left upper-right))
(define (lower-left rec)
  (car rec))
(define (lower-right rec)
  (make-point (x-point (cdr rec))
              (y-point (car rec))))
(define (upper-right rec)
  (cdr rec))
(define (upper-left rec)
  (make-point (x-point (car rec))
              (y-point (cdr rec))))
(define (perimeter rec)
  (* 2 (+ (width rec) (height rec))))
(define (area rec)
  (* (width rec) (height rec)))
(define (width rec)
  (abs (- (x-point (lower-right)) (x-point (lower-left)))))
(define (height rec)
  (abs (- (y-point (upper-left)) (y-point (lower-left)))))

; 2nd implementation
(define (make-rec-segment seg height)
  (cons seg height))
(define (area2 rec)
  (* (length (car rec)) (cdr height)))
(define (perimeter2 rec)
  (* 2 (+ (length (car rec)) (cdr height))))
(define (length seg)
  (;ist mir jetzt zu blöd
   ))

  
  