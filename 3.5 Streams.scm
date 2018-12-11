(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s stream)
  (stream-map (lambda (x) (* x s))
              stream))

(define (translate-stream t stream)
  (stream-map (lambda (x) (+ x t))
              stream))

(define ones
  (cons-stream 1 ones))

(define integers
  (cons-stream 1
               (add-streams ones
                            integers)))

(define fact-stream
  (cons-stream 1
               (mul-streams (stream-cdr integers)
                            fact-stream)))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (stream-map (lambda (x) (+ x (stream-car s)))
                           (partial-sums (stream-cdr s)))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define hammond
  (cons-stream 1
               (merge (scale-stream 2 hammond)
                      (merge (scale-stream 3 hammond)
                             (scale-stream 5 hammond)))))

(define (stream-take n s)
  (if (< n 0)
      '()
      (cons (stream-car s)
            (stream-take (- n 1) (stream-cdr s)))))
                             
(define (random-stream requests)
  (cons-stream random-init
               ()))
