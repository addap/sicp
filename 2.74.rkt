#lang sicp
; 2.74
; a)
(define (get-record emp dpf) ;employee, division personal file
  ((get (id dpf) 'employee) emp dpf))
; each division personal file must have an id which can be accessed by the (id dpf) procedure
; and must implement its own procedure to return an employee taking two arguments, the emp key and itself (could probably drop that tho)
; it must return false if the meployee is not found
; and put it in the db using its id and the key 'employee

; b)
(define (get-salary emp)
  ((get (get-dpf emp) 'salary) emp))
; all employee records need a backreference to the dpf they are stored in, all accessible by the same global procedure
; the dpf must implement a procedure taking two arguments, the key 'salary and an employee that returns their salary according to its specific structure
; that procedure must be put into the db with the according keys

; c)
(define (get-employee-record emp list-of-dpfs)
  (car (filter (lambda (x) (not (eq? x #f)))
          map (lambda (dpf) (get-record emp dpf)))))
; map over all dpfs and look for the employee, the resulting list will contain only false values exept the employee (if he existst in one of them)
; then filter out all those and take the car of the reuslt

; d)
; the new corporations personell records must be updated with
; a unique id for the file, and procedures for returning an employee and his salary and so on