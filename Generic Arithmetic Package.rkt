#lang sicp
; 2.78
(define (attach-tag tag x)
  (if (number? x)
      x
      (cons tag x)))
(define (type-tag x)
  (if (number? x)
      'scheme-number
      (car x)))
(define (contents x)
  (if (number? x)
      x
      (cdr x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (display "No method for these types: APPLY-GENERIC" (list op type-tags))))))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (define number-tag '(scheme-number scheme-number))
  (put 'add number-tag
       (lambda (x y) (tag (+ x y))))
  (put 'sub number-tag
       (lambda (x y) (tag (- x y))))
  (put 'mul number-tag
       (lambda (x y) (tag (* x y))))
  (put 'div number-tag
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equal '(scheme-number scheme-number)
       =)
  (put '=zero 'scheme-number (lambda (x) (= x 0)))
  (put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;;internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (not (= d 0))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g))))
    (error "Denom must not be 0"))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (is-equal? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (raise x)
    (make-from-real-imag (/ (numer x)
                            (denom x)) ;will need to addressed during ex. 2.86
                         0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equal '(rational rational) is-equal?)
  (put '=zero 'rational
       (lambda (x)
         (= (numer x) 0)))
  (put 'raise 'rational raise)
  (put 'project 'rational
       (lambda (x)
         (make-scheme-number (round x))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (rect-equal? r1 r2)
    (and (= (real-part r1) (real-part r2))
         (= (imag-part r1) (imag-part r2))))
  (define (complex-equal? x y)
    (rect-equal? (apply-generic 'to-rect x)
                 (apply-generic 'to-rect y)))   
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'equal '(complex complex)
       complex-equal?)
  (put 'to-rect 'rectangular
       (lambda (x) x))
  (put 'to-rect 'polar
       (lambda (x) ((get 'make-from-ang-mag 'rectangular)
                    (angle x) (magnitude x))))
  (put '=zero 'complex
       =zero?)
  (put '=zero 'rectangular
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  (put '=zero 'polar
       (lambda (x) (and (= (angle x) 0)
                        (= (magnitude x) 0))))
  (put 'project 'complex
       (lambda (x)
         (make-rational (real-part x)
                        1)))
  
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
;;(define (imag-part...

; 2.77
; when you call real-part on a complex number it gets passed
; to apply generic, there the complex tag is stripped off
; and real-part gets called again. This time the number has
; rectangular or polar as its outer tag, so it gets passed
; to the according procedure installed.

; 2.79
(define (equ? x y) (apply-generic 'equal x y))
; and (put 'equal ...) procedures in each package

; 2.80
(define (=zero? x)
  (apply-generic '=zero x))
; and (put '=zero ...) procedures in each package

; 2.85
(define (drop x)
  (let ((projected (apply-generic 'project x)))
    (if (apply-generic 'equal (apply-generic 'raise projected)
                       x)
        projected
        #f)))


;; SYMBOLIC ALGERBRA
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L))
                    
                   
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? v)
    (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ;; adjoin-term
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  ;;interface rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
  