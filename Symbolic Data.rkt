#lang sicp
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; 2.53
; (list 'a 'b 'c)
; -> {a b c}
; (list (list 'george))
; -> {{george}}
; (cdr '((x1 x2) (y1 y2)))
; -> {{y1 y2}}
; (cadr '((x1 x2) (y1 y2)))
; -> {y1 y2}
; (pair? (car '(a short list)))
; -> #f
; (memq 'red '((red shoes) (blue socks)))
; -> #f
; (memq 'red '(red shoes black socks))
; -> {red shoes black socks}

; 2.54
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (eq? a b)))

; 2.55
; ''abracadabra quotes the quote -> (quote (quote abracadabra))
; thus (car (quote (quote abracadabra))) -> (car ('quote 'abracadabra)) -> quote

; symbolic differentiation
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (pair? (cdddr s))
      (cons '+ (cddr s))
      (caddr s)))
(define (product? x) (and pair? x) (eq? (car x) '*))
(define (multiplier p) (cadr p))
(define (multiplicant p)
  (if (pair? (cdddr p))
      (cons '* (cddr p))
      (caddr p)))
; 2.56
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base expt)
  (cond ((= expt 0) 1) 
        ((= expt 1) base)
        ;((and (number? base) (number? expt)) (fast-expt base expt))
        (else (list '** base expt))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicant exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicant exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp) (- (exponent exp) 1))))
        (else
         (display "unknown expression type: DERIV" exp))))