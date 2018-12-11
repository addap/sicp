(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (inverter-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input inverter-input) 'ok )
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid Signal LOGICAL-NOT" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and a1 a2)
  (cond ((and (= a1 1) (= a2 1)) 1)
        ((or (= a1 0) (= a2 0)) 0)
        (else (error "Invalid Signal LOGICAL-AND" a1 a2))))

;; Ex 3.28
; easily done copying the and-gate
(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)
(define (logical-or o1 o2)
  (cond ((and (= o1 0) (= o2 0)) 0)
        ((or (= o1 0) (= o2 0)) 1)
        (else error "Invalid Signal LOGICAL-OR" o1 o2)))

;; Ex 3.29
; Building an or-gate from an and-gate and inverters
; --Inv---And--Inv---
;         | 
; --Inv---+
; the delay time would be two inverter-delay and one and-delay
(define (or-gate2 o1 o2 output)
  (let ((o1-inv (make-wire)) (o2-inv (make-wire)) (and-output (make-wire)))
    (inverter o1 o1-inv)
    (inverter o2 o2-inv)
    (and-gate o1-inv o2-inv and-output)
    (inverter and-output output)
    'ok))

;; Ex 3.30
(define (ripple-carry-adder ak bk sk carry)
  (let ((c-in (make-wire)))
    (if (null? (cdr a))
	(set-signal! c-in 0)
	(ripple-carry-adder (cdr ak) (cdr bk) (cdr sk) c-in))
    (full-adder (car ak) (car bk) c-in (car sk) carry)))
	
; t(n-ripple) = n*t(full)
; t(full) = t(half-s) + t(half-c) + t(or)
; t(or) = 2*t(inv) + t(and)
; t(half-s) = t(and) + max(t(and) + t(inv), t(or))
;           = 2 * (t(and) + t(inv))
; t(half-c) = t(and)
; t(full) = 4 * (t(and) + t(inv))
; t(ripple) = 4n * (t(and) + t(inv))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
	    (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
	     (call-each (cdr procedures)))))
  
