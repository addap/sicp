;; Ex 3.38

;; a)
;; initially: 100$
;; 1: (set! balance (+ balance 10))
;; 2: (set! balance (- balance 20))
;; 3: (set! balance (- balance (/ balance 2)))

;; 1 2 3
;; 2 1 3
;; -> 45$

;; 1 3 2
;; -> 35$

;; 2 3 1
;; -> 50$

;; 3 1 2
;; 3 2 1
;; -> 40$

;; b)
;; If all three would retrieve the value of the original balance, and the (set!) of 1 happened last,
;; the bank account would hold 110$

;; Ex. 3.39
;; 121, 101 of course
;; and 100 if the (* x x) is computed first, then its set to 11 and only after that it's set to 100

;; Ex. 3.40
;; 1,000,000
;; 100
;; 1000
;; 10,000
;; 100,000

;; if we serialize properly only 1,000,000 remains

Ex. 3.47
(define (make-semaphore n)
  (let ((mutexes (map (lambda (x) (make-mutex))
                      (enumerate 1 n))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (any-acquire? mutexes)
                 true
                 (the-semaphore m)))
            ((eq? m 'release)
             (any-release mutexes))))
    the-semaphore))

(define (any-acquire? mutexes)
  (if (null? mutexes)
      #f
      (if (acquire (car mutexes))
          #t
          (any-acquire? (cdr mutexes)))))
(define (any-release mutexes)
  (if (null? mutexes)
      (error "Could not release any mutexes")
      (if (release (car mutexes))
          #t
          (any-release (cdr mutexes)))))

