(define (an-integer-starting-from low)
  (amb low
       (an-integer-starting-from (+ low 1))))

;; Ex 4.35
(define (an-integer-between low high)
  (if (> low high)
      (amb)
      (amb low (an-integer-between (+ low 1) high))))


;; Ex 4.36
;; Because when using an-integer-starting-from, evaluating try-again would only test another alternative for the last assigned value. It would increase infinitely and never change the first two variables.
;; You could do it by rearranging i & j and adding another constraint on k to not exceed (sqrt i^2 j^2)
(define (all-pythagorean-triples)
  (let* ((j (an-integer-starting-from 1))
         (i (an-integer-between 1 j))
         (ij (+ (square i) (square j)))
         (k (an-integer-between j (sqrt ij))))
    (require (= (square k) ij))
    (list i j k)))

;; Ex 4.37
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

;; Ben Bitdiddle's code is faster because due to the (require (>= hsq ksq)) because it causes backtracking on i or j
;; so that for those values you never try different values for k

;; Ex 4.39
;; I think the order of the restrictions matters

(define (distinct? elements)
  (if (null? elements)
      #t
      (and (not (member (car elements) (cdr elements)))
           (distinct? (cdr elements)))))

;; Ex 4.40
(define (multiple-dwelling)
  (let* ((fletcher (amb 1 2 3 4 5))
         (smith (amb 1 2 3 4 5)))
    (require (distinct? (list fletcher smith)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (distinct? (list fletcher smith cooper)))
      (require (not (= (abs (- cooper fletcher)) 1)))
      (require (not (or (= fletcher 5)
                        (= fletcher 1))))
      (require (not (= cooper 1)))
      (let ((miller (amb 1 2 3 4 5)))
        (require (distinct? (list fletcher smith cooper miller)))
        (require (> miller cooper))
        (let ((baker (amb 1 2 3 4 5)))
          (require (distinct? (list fletcher smith cooper miller baker)))
          (require (not (= baker 5)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;; Ex 4.42
(define (liars)
  (let* ((betty (amb 1 3))
         (ethel (amb 1 5))
         (joan (amb 2 3))
         (kitty (amb 2))
         (mary (amb 4)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list betty ethel joan kitty mary)))

;; Ex 4.43
(define (yachts)
  (let* ((moore-daughter 'mary-ann)
         (downing-daughter (amb 'gabrielle 'lorna 'rosalind))
         (hall-daughter (amb 'gabrielle 'lorna 'rosalind))
         (barnacle-daughter 'melissa)
         (parker-daughter (amb 'gabrielle 'lorna 'rosalind))
         (moore-yacht 'lorna)
         (downing-yacht 'melissa)
         (hall-yacht 'rosalind)
         (barnacle-yacht 'gabrielle)
         (parker-yacht 'mary-ann))
    (require (distinct? (list moore-daughter downing-daughter hall-daughter barnacle-daughter parker-daughter)))
    (require (distinct? (list moore-daughter moore-yacht)))
    (require (distinct? (list downing-daughter downing-yacht)))
    (require (distinct? (list hall-daughter hall-yacht)))
    (require (distinct? (list barnacle-daughter barnacle-yacht)))
    (require (distinct? (list parker-daughter parker-yacht)))
    (require (eq? barnacle-yacht parker-daughter))))


