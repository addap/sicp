#lang sicp
(#%require (only racket/base random))
(#%require sicp-pict)
(#%require racket/draw)
(#%require racket)
(#%require racket/gui/base)

(define (make-cell status)
  status)
(define (live? cell)
  (eq? cell 'live))
(define live 1)
(define dead 0)

(define (item sequence n)
  (if (<= n 1)
      (car sequence)
      (item (- n 1) (cdr sequence))))

(define (make-random-cell . x)
  (if (< (random 100) 50) live dead))

(define (make-random-row length)
  (if (<= length 0)
      '()
      (cons (make-random-cell) (make-random-row (- length 1)))))

(define (make-random-grid length height)
  (if (<= height 0)
      '()
      (cons (make-random-row length)
            (make-random-grid length (- height 1)))))

(define (next-step grid)
  ;hx-grid, horizontally extended grid, means that the first and last elements are copied
  ;to the end and beginning, respectively, of the row-lists
  ;vhx-grid, also copy the last and first row the the beginning and end
  ;it's one way of having easy access to the 8 adjacent cells of the one to be checked
  ;I map over the original grid to produce one with same layout but use the data form the
  ;extended lists
  ;
  ;another way would be to have 9 copies of the list, all shifted by one in each direction
  ;and map over them to produce the new one. I think that would be more efficient
  ;otherwise try it with quad trees.
  ;
  (let* ((hx-grid (map (lambda (row) 
                         (append (list (last row))
                                 row
                                 (list (car row))))
                       grid))
         (vhx-grid (append (list (last hx-grid))
                           hx-grid
                           (list (car hx-grid))))
         (length (length grid)))
    ;(display hx-grid)
    ;(display vhx-grid)
    ;map over the originial grid, use row number to determine which rows to pick from the extended list
    (map (lambda (row row-number)
           ;(display (sublist vhx-grid row-number 3))
           ;(newline)
           ;map over each row, fetching 3 x-rows and then 3 cells each
           (map (lambda (cell cell-number)
                  (let ((sum (accumulate + 0 (flatmap (lambda (x-row) ;;<--- fix sublist arguments have to go up
                                                        ;(display (sublist x-row cell-number 3))
                                                        ;(newline)
                                                        (sublist x-row cell-number 3))
                                                      (sublist vhx-grid row-number 3)))))
                    #|
(display cell)
                    (display ": ")
                    (display sum)
                    (newline)
|#
                    (cond ((= sum 3) live)
                          ((= sum 4) cell)
                          (else dead))))
                row
                (enumerate-interval 1 length)))
           grid
           (enumerate-interval 1 length))))

(define (print-next grid)
  (print-grid (next-step grid)))

(define (next-n grid n)
  (if (> n 0)
      (let ((next (next-step grid)))
        (display-next next)
        (next-n next (- n 1)))
      (display "finished")))


(define (repeated proc n)
  (lambda (arg)
    (if (= n 1)
        (proc arg)
        (proc ((repeated proc (- n 1)) arg)))))

(define  (enumerate-interval start stop)
  (if (> start stop)
      '()
      (cons start (enumerate-interval (+ start 1) stop))))

(define (cycle-forward sequence)
  (append (cdr sequence) (list (car sequence))))
(define (cycle-backward sequence)
  (append (list (last sequence)) (butlast sequence)))
(define (last sequence)
  (if (null? (cdr sequence))
      (car sequence)
      (last (cdr sequence))))
(define (butlast sequence)
  (if (null? (cdr sequence))
      '()
      (cons (car sequence) (butlast (cdr sequence)))))

    
;;*************************************
;; VIEW functions
#|
(define (make-painter grid)
  (merge (map (lambda (x) (merge x beside)) (translate-grid grid))
         below))

(define (translate-grid grid)
  (map (lambda (row)
         (map (lambda (cell) (if (live? cell) black white))
              row))
       grid))

(define (print-grid grid)
  (paint (make-painter (translate-grid grid))))
|#

(define (print-grid canvas dc)
  (send dc set-brush "white" 'solid)
  (send dc draw-rectangle
        0 0
        (* 10 grid-length) (* 10 grid-length))
  (send dc set-pen "white" 1 'transparent)
  (send dc set-brush "black" 'solid)
  (for-each (lambda (row y-offset)
              (for-each (lambda (cell x-offset)
                          (if (= cell live)
                              (send dc draw-rectangle
                                    (* cell-length-px x-offset) (* cell-length-px y-offset)
                                    cell-length-px cell-length-px)
                              0))
                        row
                        (enumerate-interval 0 (- grid-length 1))))
            active-grid
            (enumerate-interval 0 (- grid-length 1)))
  )

(define (life)
  (send frame show #t))


(define (display-next grid)
  (define active-grid grid)
  (send canvas on-paint))
  


;; Deprecated
#|
;goes through the sequence and constructs pairs of two consecutive elements
;then maps beside/below over the pairs
;repeat until there is only one element in the sequence, the merged painters
(define (merge sequence op)
  (define (make-pairs sequence)
    (if (>= (length sequence) 2)
        (cons (cons (car sequence) (cadr sequence)) (make-pairs (cddr sequence)))
        '()))
  (if (= (length sequence) 1)
      (car sequence)
      (merge (map (lambda (pair) (op (car pair) (cdr pair)))
                  (make-pairs sequence))
             op)))

(define testpainters (list black white black white))
 |#
        
(define (accumulate-noinit op set)
  (if (null? (cdr set))
      (car set)
      (op (car set) (accumulate-noinit op (cdr set)))))

(define (accumulate op init set)
  (if (null? set)
      init
      (op (car set)
          (accumulate op init (cdr set)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (car seqs))
            (accumulate-n op init (cdr seqs)))))
                      
(define (flatmap op set)
  (accumulate append nil (map op set)))

(define (sublist sequence start length)
  (cond ((or (<= length 0) (null? sequence)) '())
        ((= start 1)
         (cons (car sequence) (sublist (cdr sequence) start (- length 1))))
        (else (sublist (cdr sequence) (- start 1) length))))

;; TESTING of linked lists
(define linked-list (cons (make-cell 'live) 3))
(define (foo x)
  (+ x x)
  (- x 5))

;; TESTS

(define cell-length-px 10)
(define grid-length 64)
(define grid-length-px (* cell-length-px grid-length))
(define m (make-random-grid grid-length grid-length))
(define active-grid m)
(define frame (new frame%
                   [label "Example"]
                   [width (+ grid-length-px 20)]
                   [height (+ grid-length-px 40)]))
(define canvas (new canvas% [parent frame]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-pen "white" 1 'transparent)
                       (send dc set-brush "black" 'solid)
                       (print-grid canvas dc))]))

;(define target (make-bitmap grid-length-px grid-length-px))
;(define dc (new bitmap-dc% [bitmap target]))
     

