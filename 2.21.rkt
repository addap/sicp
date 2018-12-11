#lang sicp

; 2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

; 2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; 2.23
(define (for-each proc items)
  (if (null? items)
      (display "")
      ((lambda (x)
         (proc (car x))
         (for-each proc (cdr x)))
       items)))
        