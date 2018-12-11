#lang sicp
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) ...)
          (else (display "error etc"))))
  dispatch)

; 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ...))
  dispatch)

; 2.76
; generic operations with explicit dispatch
; when you add more types you have to update each generic procedure to handle the new type
; " operations you have to write instructions for all possible type cases 
; data-directed programming
; " types, ypu have to write a package for that type that includes all the possible procedures and registers them
; " operations, you have to add a corresponding procedure to each package, and maybe a new global procedure usng apply-generic
; or depending on the operation you can install a procedure package that covers all types
; message parsing
; " types, you have to write the type and checks for all possible operations
; " operations, you have to add a correspondong check for it in all types
;
; if lots of new types are added I would go with the message parsing one as it seems easiest/astest to write but if the restriction to one arg procedures
; does not allow for it use data directed programming
; if lots of new operations are added i would use data directed to write a procedure package and maybe reuse some between several types
; in general  data directed programming seems to be the most versatile  as you can either add a batch of new types or new operations

