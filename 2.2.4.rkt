#lang sicp
(#%require sicp-pict)

#|
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define stein4 (flipped-pairs einstein))
|#

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

#|
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
|#

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  ((square-of-four identity flip-vert identity flip-vert) painter))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; 2.45
(define (split orientation-op split-op)
  (lambda (painter)
    (orientation-op painter (split-op painter painter))))

(define right-split2 (split beside below))
(define up-split2 (split below beside))

; 2.46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))


(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scalar v)
  (make-vect (* scalar (xcor-vect v))
             (* scalar (ycor-vect v))))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect -1 v2)))

; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cadr (cdr frame)))

#|
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
;...
(define (edge2-frame frame)
  (cdr (cdr frame)))
|#

    
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

; 2.48
(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

; 2.49
#|
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

a)
(define outline-list (list (make-segment (make-vect 0 0)
                                         (make-vect 0 1))
                           (make-segment (make-vect 0 1)
                                         (make-vect 1 1))
                           (make-segment (make-vect 1 1)
                                         (make-vect 1 0))
                           (make-segment (make-vect 1 0)
                                         (make-vect 0 0))))
b)
(define x-list (list (make-segment (make-vect 0 0)
                                   (make-vect 1 1))
                     (make-segment (make-vect 1 0)
                                   (make-vect 0 1))))
c)
(define diamond-list (list (make-segment (make-vect 0.5 0)
                                         (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5)
                                         (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1)
                                         (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5)
                                         (make-vect 0.5 0))))
d)
*magic*
|#

; 2.50
(define (flip-horiz2 painter)
  ((transform-painter (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0))
   painter))