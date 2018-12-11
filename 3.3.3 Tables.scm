
;; Ex 3.24
(define (make-table same-key?)
  (let ((table (list 'table)))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            #f ) ) )
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records))) ) )
    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value)
                            (cdr table) ) ) ) ) )
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (display "error: Unknown operation: TABLE") (display m)) ) )
    dispatch ) )

; test case
(define t1 (make-table equal?))
(define t2 (make-table (lambda (x y) (= (+ x y) 0))))
((t1 'insert!) '(a b) 2)
((t1 'insert!) 4 '(2 j))
((t2 'insert!) 1 'a)
((t2 'insert!) 7 5)


;; Ex 3.25
; wouldn't that already be possible if we just use a one-dimesnional table and the key list as a single key?
; 
; in this implementation every table has a value associated with it,
; and a list of records, which are a key consed with the next subtable
(define (make-multi-table)
  (let ((table (list (cons 'table '()))))
    (define (set-value! value) (set-cdr! (car table) value))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records))) ) )
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (and record (cdr record)) ) )
    (define (lookup-multi keys) ; if null return value of table itself
      (if (null? keys)
          (cdar table)
          (let ((subtable (lookup (car keys))))
            (and subtable ((subtable 'lookup) (cdr keys))) ) ) )
    ;; rest of internal procedures
    (define (insert-multi keys value)
      (if (null? keys)
          (set-value! value)
          (let ((subtable (lookup (car keys))))
            (if subtable
                ((subtable 'insert) (cdr keys) value)
                (let ((new-table (make-multi-table)))
                  (set-cdr! table
                            (cons (cons (car keys) new-table)
                                  (cdr table)))
                  ((new-table 'insert) (cdr keys) value) ) ) ) ) )            
    ;; interface
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup-multi)
            ((eq? m 'insert) insert-multi)
            ((eq? m 'print) table)
            (else (display "error: Unknown operation on TABLE: ") (display m)) ) )
    dispatch ) )

#|
(define t (make-multi-table))
((t 'insert) '(a) 1)
((t 'insert) '(a b) 'c)
((t 'insert) '(1f) 5)
((t 'insert) '(a c) '4)
;all works yay
|#

;; Ex 3.26
; For that I would implement the list of records as a binary tree
(define (entry tree) (car tree))
(define (set-entry! tree value) (set-car! tree value))
(define (left-branch tree) (cadr tree))
(define (set-left-branch! tree value) (set-car! (cdr tree) value))
(define (right-branch tree) (caddr tree))
(define (set-right-branch! tree value) (set-car! (cddr tree) value))
(define (make-tree entry left right)
  (list entry left right))
(define (empty-tree)
  (make-tree '() '() '()))
(define (empty-tree? tree)
  (null? (entry tree)))
; just integers as keys for now
(define (compare-keys a b) (- a b))

(define (make-multi-binary-table)
  ; internal storage for the table
  (let ((table (cons (cons 'table 'value-placeholder)
		     (empty-tree))))
    (define (set-value! value) (set-cdr! (car table) value))
    ; assoc takes a key and a list of records (e.g. the cdr of a table)
    ; and returns the record-tree whose entry contains the key
    ; or if it reaches an empty tree, it returns that
    (define (assoc key record-tree)
      (if (empty-tree? record-tree)
	  record-tree
	  (let ((comp-keys (compare-key key (car (entry record-tree)))))
	    (cond ((= comp-key 0) record-tree)
		  ((< comp-key 0) (assoc key (left-branch record-tree)))
		  (else (assoc key (right-branch record-tree)))))))
    ; lookup takes a key and returns the subtable that is stored under that key
    ; or false if the key doesn't exist
    (define (lookup key)
      (let ((record-tree (assoc key (cdr table))))
        (if (empty-tree? record-tree)
	    #f
	    (cdr (entry record-tree)))))
    ; lookup-multi takes a list of keys and if the list is nil returns the
    ; current table's value, and if it isn't calls lookup-multi on the
    ; subtable returned by lookup and on the next key
    ; if no subtable is found it returns false
    (define (lookup-multi keys)
      (if (null? keys)
          (cdar table)
          (let ((subtable (lookup (car keys))))
            (and subtable ((subtable 'lookup) (cdr keys))))))
    ; insert-multi takes a list of keys and a value and inserts the value in the
    ; corresponding level/dimension of the table
    (define (insert-multi keys value)
      (if (null? keys)
          (set-value! value)
          (let ((record-tree (assoc (car keys) (cdr table))))
            (if (empty-tree? record-tree)
		; create new subtable at entry and empty-trees at the branches
		(let ((new-table (make-multi-binary-table)))
                  (set-entry! record-tree (cons (car keys) new-table))
		  (set-left-branch! record-tree (empty-tree))
		  (set-right-branch! record-tree (empty-tree))
		  ((new-table 'insert) (cdr keys) value))
		(((cdr (entry record-tree)) 'insert) (cdr keys) value)))))
    ;; interface
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup-multi)
            ((eq? m 'insert) insert-multi)
            ((eq? m 'print) table) ; need proper print proedure
            (else (display "error: Unknown operation on TABLE: ") (display m)) ) )
    dispatch))

;; tests
(define t (make-multi-binary-table))
((t 'insert) '(1) 'as)
((t 'insert) '(2) 'ed)
((t 'insert) '(1.5) 'df)
((t 'insert) '(1 3) 'sd)
((t 'insert) '(4 5) 3)
((t 'lookup) '(1))
((t 'lookup) '(2))
((t 'lookup) '(1.5)
((t 'lookup) '(1 3))
((t 'lookup) '(4 5)



;; Ex 3.27
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))
(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
	   ((= n 1) 1)
	   (else (+ (memo-fib (- n 1))
		    (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-multi-binary-table)))
    (lambda (x)
      (let ((previously-computed-result
	     (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))
