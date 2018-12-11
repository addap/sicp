#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; 2.33
#|(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))|#
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub-tree)
                         (if (pair? sub-tree)
                             (count-leaves sub-tree)
                             1))
                       t)))

; 2.36
(define (select-cars seq)
  (map car seq))

(define (select-cdrs seq)
  (map cdr seq))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (select-cars seqs))
            (accumulate-n op init (select-cdrs seqs)))))


; 2.37

(define v1 (list 1 2 3 4))
(define v2 (list 4 5 6 6))
(define v3 (list 6 7 8 9))
(define matrix (list v1 v2 v3))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define m (list (list 1 2)
                (list 3 4)
                (list 5 6)))
(define n (list (list 7 9 2 4)
                (list 8 1 3 5)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
           m)))

; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
; to ensure that fold-right and fold-left produce the same result
; op has to follow the associative law

; 2.39
(define (reverse-r sequence)
  (fold-right (lambda (first rest)
                (append rest (list first)))
              nil
              sequence))
(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))