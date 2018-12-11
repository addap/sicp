#lang sicp
;; Queues
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item) )
(define (set-rear-ptr! queue item)
  (set-cdr! queue item) )
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (display "FRONT called with an empty queue" queue)
      (car (front-ptr queue)) ) )
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue )
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue ) ) ) )
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (display "DELETE! called with an empty queue" queue) )
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue ) ) )

;; Ex 3.21
; Ben's examples show what they do bc the queue is just a
; pair of the front and rear pointer. As such, the proper queue
; is the car of that pair and the last element is the cdr
; a print-queue can easily be implemented by
(define (print-queue queue)
  (front-ptr queue))

;; Ex 3.22
(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?)
          (display "error: FRONT called with an empty queue" (print))
          (car front-ptr)))
    (define (insert! item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (print))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (print)))))
    (define (print)
      (display front-ptr))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (delete!)
      (cond ((empty?)
             (display "error: DELETE called with an empty queue." (print)))
            (else
             (set-front-ptr! (cdr front-ptr))
             (print))))
    (define (dispatch m)
      (cond ((eq? m 'print) (print))
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) (delete!))
            ((eq? m 'front) (front))
            ((eq? m 'empty?) (empty?))
            (else (display "error: Invalid dispatch" m))))
    dispatch))

#|
; test case
(define q (make-queue2))
((q 'insert!) 'a)
((q 'insert!) 'b)
((q 'insert!) 'c)
|#

;; Ex 3.23
; Im Moment scheint mir die beste Möglichkeit eine dequeue zu implementieren,
; Ein fron-ptr und ein rear-ptr und jede Zelle hat im car Ihren Wert und im cdr eine
; cons cell mit p jeweils einem pointer zur vorherigen und zur nächsten Zelle.
; Die erste Zelle sähe so aus
; (cons 'a (cons '() 'next-ptr))
; Jede in der mitte so
; (cons 'f (cons 'prev-ptr 'next-ptr))
; Und am Ende so
; (cons 'z (cons 'prev-ptr '()))
