#lang sicp

; 2.27
(define test-list (list 1 2 (list 3 (list 4 5) 6) 7 8 (list 9)))
(define (reverse items)
  (define (helper items rev-items)
    (if (null? items)
        rev-items
        (helper (cdr items) (cons (car items) rev-items))))
  (helper items nil))


(define (deep-reverse items)
  (if (pair? items)
      (reverse (map deep-reverse items))
      items))

; 2.28
(define list1 (list (list 1 2) (list 3 4)))
(define equiv-cons (cons (cons 1
                               (cons 2 nil))
                         (cons (cons 3 (cons 4 nil))
                               nil)))

(define (fringe items)
  (define (helper items result)
    (cond ((null? items) result)
          ((not (pair? items)) (cons items result))
          (else (helper (car items)
                        (helper (cdr items) result)))))
  (helper items nil))
             
            
; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (balanced? mobile)
  (if (number? mobile)
      true
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (and (balanced? (branch-structure left))
             (balanced? (branch-structure right))
             (= (* (total-weight (branch-structure left)) (branch-length left))
                (* (total-weight (branch-structure right)) (branch-length right)))))))


(define sub-mobile (make-mobile (make-branch 6 8)
                                (make-branch 4 12)))
(define mobile (make-mobile (make-branch 10 9)
                            (make-branch 5 sub-mobile)))

; d) if we changed it to use a cons instead of a list we would
; only have to change the (right-branch) and (branch-structure) selectors
; to use cdr