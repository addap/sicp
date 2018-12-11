#lang sicp
(#%require (only racket/base random))

;tests if n is prime by checking if it is divisible by
;2, 3, 5 ... sqrt(n)
(define (prime? n)
  (define (smallest-divisor n) (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b) (= (remainder b a) 0))
  (define (next n) (if (= n 2) 3 (+ n 2)))                       
  (= n (smallest-divisor n)))

(define (square x) (* x x))

;starts a primality test and passes the current system time
;then returns whether n is prime
(define (timed-prime-test n)
  (start-prime-test n (runtime))
  (fast-prime? n 100))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime n (- (runtime) start-time))))

(define (report-prime prime elapsed-time)
  (newline)
  (display prime)
  (display " *** ")
  (display elapsed-time))

(define (even? n) (= (remainder n 2) 0))

(define (three-smallest-primes lower-bound)
  (if (even? lower-bound)
      (search-for-primes (+ lower-bound 1) 0)
      (search-for-primes lower-bound 0)))

(define (search-for-primes lower-bound found-primes)
  (if (< found-primes 3) 
      (if (timed-prime-test lower-bound)
          (search-for-primes (+ lower-bound 2) (inc found-primes))
          (search-for-primes (+ lower-bound 2) found-primes))))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- (min n 4294967087) 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (carmichael n)
  (cond ((= n 1) 561)
        ((= n 2) 1105)
        ((= n 3) 1729)
        ((= n 4) 2465)
        ((= n 5) 2821)
        (else 6601)))

(define (test-carmichael n)
  (display (carmichael n))
  (fast-prime? (carmichael n) 100))