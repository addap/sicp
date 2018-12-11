
;; Ex 3.11
; The local state for acc is kept in its own enivironment E1
; If anoter account acc2 is created, the evaluation of (make-account)
; will create a new environment frame E2 where its local state will be kept
; The shared parts of the environment structure are the procedure bodies and the gloabal env


;; Ex 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y) )
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))) )


; (define x (list 'a 'b))
; (define y (list 'c 'd))
; (define z (append x y))
; response to (cdr x)
; (b) because x has not been modified

; define w (append! x y))
; response to (cdr x)
; (b c d) because x has now been mutated

;; Ex 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '( a b c )))
z

; (last-pair z) starts an infinite loop,
; bc the cell where the terminating nil was contained 
; now points to z itself

;; Ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x) ) ) )
  (loop x '()) )

; mystery reverses a list
; v -> (a b c d)
; w -> (d c b a)

;; Ex 3.16
(define (naive-count-pairs x)
  (if (not (pair? x))
      0
      (+ (naive-count-pairs (car x))
         (naive-count-pairs (cdr x))
         1 ) ) )
; return 3
(define r3 '(a b c))
; return 4
(define r4-1 '(b c))
(set-car! r4-1 (cdr r4-1))
(define r4 (cons 'a r4-1)) 
; return 7
(define r7-1 '(a))
(define r7-2 (cons r7-1 r7-1))
(define r7 (cons r7-2 r7-2))
; never return, anything with a cycle
(define nevar (make-cycle '(a b c)))


;; Ex 3.17
(define (memq? item list)
  (cond ((null? list) false)
        ((eq? item (car list)) true)
        (else (memq? item (cdr list))) ) )
(define (count-pairs x)
  (define counted '())
  (define (helper to-count)
    (cond ((or (not (pair? to-count))
               (memq? to-count counted)) 0)
          (else (set! counted (cons to-count counted))
                (+ (helper (car to-count))
                   (helper (cdr to-count))
                   1 ) ) ) )
  (helper x) )

;; Ex 3.18
(define (cycle? x)
  (define (helper to-check)
    (cond ((null? to-check) #f)
          ((eq? x to-check) #t)
          (else (helper (cdr to-check))) ) )
  (helper (cdr x)) )

;; Ex 3.19
;; I think you have to compare to cells in the list, you start
; with the first and second and then advance them differently
; (the first one by one and the second one by two I guess)
; Iff they ever are the same you have a cycle, otherwise they eventually reach a nil

