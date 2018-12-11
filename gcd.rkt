#lang racket
(define (gdc a b)
  (if (= b 0)
      a
      (gdc b (remainder a b))))

#|
normal-order evaluation

(gdc 206 40)
(if (= 40 0)
(gdc 40 (remainder 206 40))
(gdc 40 (remainder 206 40))
(if (= (remainder 206 40) 0)
(if (= 6 0))
(gdc (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40))) 0))
(if (= 4 0))
(gdc ...


applicative-order evaluation

(gdc 206 40)
(if (= 40 0))
(gdc 40 (remainder 206 40))
(gdc 40 6)
(if (= 6 0))
(gdc 6 (remainder 40 6))
(gdc 6 4)
(if ...
|#