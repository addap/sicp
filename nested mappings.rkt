#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (ordered-pairs n)
  (accumulate
   append nil (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))


(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(define (prime? n) (= n (smallest-divisor n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; 2.41
(define (sum-triples n sum)
  (define (distinct-triples n)
    (flatmap (lambda (i)
               (flatmap (lambda (j)
                      (map (lambda (k)
                             (list i j k))
                           (enumerate-interval 1 (- j 1))))
                    (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))
  (map permutations (filter (lambda (seq)
                              (= sum (+ (car seq) (cadr seq) (caddr seq))))
                            (distinct-triples n))))

; 2.42
;list-ref with sequence starting at 1
(define (list-ref1 items n)
  (if (= n 1)
      (car items)
      (list-ref1 (cdr items) (- n 1))))

;insert item into sequence starting at 1
(define (insert1 item n sequence)
  (if (= n 1)
      (cons item sequence)
      (cons (car sequence) (insert1 item (- n 1) (cdr sequence)))))
                                                 

(define (queens board-size)
  (define empty-board nil)
  (define (safe? column positions)
    (define (iter current-column to-check)
      (let ((row (list-ref1 positions current-column))
            (column-difference (- column current-column)))                
        (cond ((= column-difference 0) #t)
              ((or (= row to-check)
                   (= (+ row column-difference) to-check)
                   (= (- row column-difference) to-check))
               #f)
              (else (iter (+ current-column 1) to-check)))))
    (iter 1 (list-ref1 positions column)))
  (define adjoin-position insert1)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


