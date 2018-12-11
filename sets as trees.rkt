#lang sicp
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define treeA (make-tree 7
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 5 '() '()))
                         (make-tree 9
                                    '()
                                    (make-tree 11 '() '()))))
(define treeB (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9
                                               '()
                                               (make-tree 11 '() '())))))
(define treeC (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))

(define treeD (make-tree 6
                         (make-tree 4
                                    (make-tree 3 '() '())
                                    '())
                         (make-tree 8
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))

; 2.63a
; yes they produce the same list for every tree
; 2.63b
; the first one has O(n*logn) and the second O(n)

(define listA '(1 3 5 7 9 11))
(define listB '(1 3 5 7 9 11 13))
(define listC '(1 3 5 7 9 11 13 15))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; 2.64a
; partial-tree works by recursively generating a partial-tree with
; successively less nodes. For a given n elements it puts half in the left and
; half in the right sub-tree. If that's not possible the right one gets one element more.
; and one as the entry for that node. When it reaches a leaf node, it will
; put '() as its left branch and pull the first item out of the unused elements
; to use it as the leaf's entry. The remaining elements will then propagate one
; recursion step up and be used in the previous partial tree as his entry and right sub-tree
; You have to do it this way because first half has to be used in the left sub-tree, specifically
; the smallest element has to be used in the leftmost leaf and the next bigger one in the node above
; it, but the next bigger one will be one level deeper again, so you have to build the left subtree first.
; not very clear I know, sucks to be you future Adrian
; 2.64b
; Growth of number of steps is O(n)

; 2.65
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                      (else (cons x1 (union-set (cdr set1) (cdr set2)))))))))
(define (union-tree tree1 tree2)
  (list->tree (union-set (tree->list-2 tree1) (tree->list-2 tree2))))
(define (intersection-tree tree1 tree2)
  (list->tree (intersection-set (tree->list-2 tree1) (tree->list-2 tree2))))

; 2.66
(define (lookup key records)
  (let ((entry-key (key (entry records))))
    (cond ((null? records) false)
          ((= key entry-key)
           (entry records))
          ((< key entry-key)
           (lookup key (left-branch records)))
          ((> key entry-key)
           (lookup key (right-branch records))))))