#lang sicp
;probiere nit schem
;abcfefghijklmnopqrrstuvwxyz
;abcdef
;
;(#%require racket/random)
; to store local state, we can use a define inside the body of the procedure
(define (new-withdraw1)
  (define balance 100)
 (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds.")))


; or a let expression, which is easier bc the outer definition doesn't have to be a procedure
(define new-withdraw2
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds."))))

; or with a formal parameter
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds.")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (display "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;; 3.1
(define (make-accumulator sum)
  (lambda (add)
    (set! sum (+ sum add))
    sum))

;; 3.2
(define (make-monitored f)
  (let ((counter 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) counter)
            ((eq? input 'reset-count)
             (set! counter 0))
            (else (begin
                    (set! counter (+ counter 1))
                    (f input)))))))

;; 3.3 & 3.4
(define (make-account-pw balance password)
  (define incorrect-tries 0)
  (define (call-the-cops)
    (display "Cops called.")
    (newline))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (make-joint new-pw)
    (lambda (m pw)
      (if (eq? pw new-pw)
          (dispatch m password)
          (dispatch m '()) ) ) )
  (define (dispatch m pw)
    (if (eq? pw password)
        (begin
          (set! incorrect-tries 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'balance) balance)
                ((eq? m 'make-joint) make-joint)
                (else (display "Unknown request: MAKE-ACCOUNT"
                               m))))
        (begin
          (set! incorrect-tries (+ incorrect-tries 1))
          (if (>= incorrect-tries 3)
            (call-the-cops) '())
          "Incorrect password.")))
  dispatch)


;;;; The Benefits of Introducing Assignment
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  ;(= (gcd (rand) (rand)) 1))
  #t)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

;; 3.5
#|
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low ((random range)))))

(define (estimate-pi-3.5)
  (define (predicate)
    (<= (+ (square (random-in-range -0.5 0.5))
           (square (random-in-range -0.5 0.5)))
        1))
  (monte-carlo 1000 predicate))

;; 3.6
(define rand
  (let ((seed 0))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! seed (random-update seed))
             seed)
            ((eq? m 'reset)
             (lambda (new-value)
               (set! seed new-value)))
            (else (display "Unknown dipatch: RAND")
                  m)))))
          |#    
            
;; 3.7
(define (memq? item list)
  (cond ((null? list) false)
        ((eq? item (car list)) true)
        (else (memq? item (cdr list)))))
;; makes a joint account so that two people can access one account with two different passwords
; each can only use his password, not the other ones
(define (make-joint account password new-password)
  ((account 'make-joint password) new-password))

#|
;test cases
(define peter-acc (make-account-pw 100 'abc))
(define paul-acc (make-joint peter-acc 'abc 'def))
(peter-acc 'balance 'abc)
(paul-acc 'balance 'def)
((peter<>>>>>>>>>>>-acc 'withdraw 'abc) 19)
(peter-acc 'balance 'abc)
(paul-acc 'balance 'def)
((paul-acc 'deposit 'def) 10)
(peter-acc 'balance 'abc)
(paul-acc 'balance 'def)
|#

;; Ex 3.8
(define ex3.8 1)
(define (f a)
  (if (= a 0)
      (set! ex3.8 0)
      '() )
  (* a ex3.8) )

;; Ex 3.10
; The objects have the same behaviour. The only difference to the
; first procedure is that the current lokal state of balance is stored
; one environment level "deeper". (In E2 (blance 100) -> E1 (initial 100) -> gloabl)
; This is handy if you want to be able to reset an objec to its initial configuration

          
            
      