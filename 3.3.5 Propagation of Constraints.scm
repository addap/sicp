;; Constraint primitives
;; adder, multiplier, constant...
;; Means of combination
;; Connectors
;; Propagation
;; When connectors are given a value, they awaken all the connected constraint boxes which poll to see if
;; they have enough information to set their connected connectors and so on.

(define (potentiator b e s)
  "b^e = s"
  (define (process-new-value)
    (cond ((and (has-value? b) (has-value? e))
	   (set-value! s
		       (expt (get-value b) (get-value e))
		       me))
	  ((and (has-value? b) (has-value? s))
	   (set-value! e
		       (/ (log (get-value s)) (log (get-value b)))
		       me))
	  ((and (has-value? e) (has-value? s))
	   (set-value! b
		       (expt (get-value s) (/ 1 (get-value e)))))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! e me)
    (forget-value! s me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: POTENTIATOR" request))))
  (connect b me)
  (connect e me)
  (connect s me)
  me)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
	   (set-value! sum
		       (+ (get-value a1) (get-value a2))
		       me))
	  ((and (has-value? a1) (has-value? sum))
	   (set-value! a2
		       (- (get-value sum) (get-value a1))
		       me))
	  ((and (has-value? a2) (has-value? sum))
	   (set-value! a1
		       (- (get-value sum) (get-value a2))
		       me))))
  (define (process-forget-value)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (forget-value! sum me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
	       (and (has-value? m2) (= (get-value m2) 0)))
	   (set-value! product 0 me))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product
		       (* (get-value m1) (get-value m2))
		       me))
	  ((and (has-value? m1) (has-value? product))
	   (set-value! m2
		       (/ (get-value product) (get-value m1))
		       me))
	  ((and (has-value? m2) (has-value? product))
	   (set-value! m1
		       (/ (get-value product) (get-value m2))
		       me))))
  (define (process-forget-value)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (forget-value! product me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe '?))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
	     (set! value newval)
	     (set! informant setter)
	     (for-each-except setter
			      inform-about-value
			      constraints))
	    ((not (= value newval))
	     (error "Contradiction: " `(,value != ,newval)))
	    (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	  (begin
	    (set! informant false)
	    (for-each-except retractor
			     inform-about-no-value
			     constraints))
	  'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	  (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
	  (inform-about-value new-constraint)))
    (define (me request)
      (cond ((eq? request 'has-value?)
	     (if informant true false))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-my-value)
	    ((eq? request 'forget) forget-my-value)
	    ((eq? request 'connect) connect)
	    (else (error "Unknown request: CONNECTOR"
			 request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? exception (car items)) (loop (cdr items)))
	  (else (procedure (car items))
		(loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


(define C (make-connector))
(define F (make-connector))

(define (celcius-fahrenheit-converter c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(probe "Celcius temp" C)
(probe "Fahrenheit temp" F)

(define (celcius-fahrenheit-tests)
  (celcius-fahrenheit-converter C F)
  (set-value! C 25 'user)
  (forget-value! C 'user)
  (set-value! F 32 'user)
  (forget-value! F 'user))

;; Ex 3.33
(define (averager a b c)
  (let ((x (make-connector))
	(y (make-connector)))
    (adder a b x)
    (multiplier y c x)
    (constant 2 y)
    'ok))

;; Ex 3.34
;; (define (squarer a b)
;;   (multiplier a a b))
;; The flaw is, the multiplier does not know that both
;; its factors always hold the same value. It can only
;; calculate one by dividing the product with the other
;; one.

;; Ex 3.35
(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? b)
	   (if (< (get-value b) 0)
	       (error "Square must be positive: SQUARER" b)
	       (set-value! a
			   (sqrt (get-value b))
			   me)))
	  ((has-value? a)
	   (set-value! b
		       (square a)
		       me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
	  ((eq? request 'I-lost-my-value) (process-forget-value))
	  (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;; Ex 3.36
;; E_global    
;; ^   ^  ^
;; |   |  |
;; E_a |  E_b
;;     | 
;;     |
;; for-each-except

;; Ex 3.37
;; With an expression-oriented style, we can avoid cumbersome
;; temporary connectors. But we also need more procedures.
(define (c+ a b)
  (let ((c (make-connector)))
    (adder a b c)
    c))
(define (c- c a)
  (let ((b (make-connector)))
    (adder a b c)
    b))
(define (c* m n)
  (let ((p (make-connector)))
    (multiplier m n p)
    p))
(define (c/ p m)
  (let ((n (make-connector)))
    (multiplier m n p)
    n))
(define (cv v)
  (let ((c (make-connector)))
    (constant v c)
    c))

(define (celcius-fahrenheit-converter2 x)
  (c+ (c* (c/ (cv 9) (cv 5))
	  x)
      (cv 32)))



