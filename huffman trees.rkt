#lang sicp
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (display "bad bit: CHOOSE BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'C 1)
                    (make-leaf 'D 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; decoded message ACABBDA

; 2.68
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
      
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left-branch (left-branch tree))
            (right-branch (right-branch tree)))
        (cond ((element-of-set? symbol (symbols left-branch))
               (cons 0 (encode-symbol symbol left-branch)))
              ((element-of-set? symbol (symbols right-branch))
               (cons 1 (encode-symbol symbol right-branch)))
              (else (display "error: symbol not in tree:" symbol))))))

; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge trees)
  (if (> (length trees) 1)
      (successive-merge (adjoin-set (make-code-tree (car trees)
                                                    (cadr trees))
                                    (cddr trees)))
      (car trees)))

; 2.70
(define rock-tree (generate-huffman-tree
                   '((A 2) (GET 2) (SHA 3) (WAH 1)
                         (BOOM 1) (JOB 2) (NA 16) (YIP 9))))
(define rock-message '(GET A JOB SHA NA NA NA NA NA NA NA NA
                           GET A JOB SHA NA NA NA NA NA NA NA NA
                           WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP YIP
                           SHA BOOM))
; 86 bits are needed to encode the message
; with fixed length encoding it would be 108

; 2.71
; 1 bit is needed to encode the most frequent symbol
; n-1 bits are needed to encode the least frequent symbol

; 2.72
; Order of growth to encode the most frequent symbol of the alphabet
;     O(1)
; Order of growth to encode the leasr frequent symbol of the alphabet
;     O(