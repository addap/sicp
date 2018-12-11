;; Ex 4.55
;; 1
(supervisor ?x (Bitdiddle Ben))
;; 2
(job ?x (accounting . ?y))
;; 3
(address ?x (Slumerville ?y))


;; Ex 4.56
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y))

(and (salary ?ben (Bitdiddle Ben))
     (salary ?x ?y)
     (lisp-value < ?x ?ben))

(and (supervisor ?name ?supervisor)
     (job ?supervisor (?division . ?T))
     (not (same ?division 'computer)))

(rule (same ?x ?x))
;; Ex 4.57
(rule (can-replace ?person1 ?person2)
      (and (or (and (job ?person1 ?job)
                    (job ?person2 ?job))
               (and (job ?person3 ?job)
                    (job ?person1 ?job)
                    (job ?person2 ?job2)
                    (can-do-job ?job ?job2)))
           (not (same ?person1 ?person2))))

(can-replace ?x (Cy D. Fect))
(and (can-replace ?x ?y) 
     (salary ?x ?sal-x)
     (salary ?y ?sal-y)
     (lisp-value > ?sal-x ?sal-y))

;; Ex 4.60
;; lives-near is a commutative relation that's why this happens. It regards all possible instantiations for the pattern so naturally it finds both variants of the fact.
;; One could specify that a rule is commutative, and the the evaluator would keep track about which dictionaries it has already output to check of there are duplicates.


(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

;; Ex 4.61
(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

;; (?x next-to ?y in (1 (2 3) 4))
;; (1 next-to (2 3) in (1 (2 3) 4))
;; ((2 3) next-to 4 in (1 (2 3) 4))

;; Ex 4.62
(rule (last-pair (?l) (?l)))
(rule (last-pair (?h . ?r) (?l))
      (last-pair ?r (?l)))


;; Ex 4.64
;; He swapped the two queries in the and. As a result ?middle-manager is not bound in the frame when the rule gets evaluated so we simple evaluate the rule (outranked-by ?middle-manager ?boss) which calls itself again, hence infinite loop
;; In the original version the recursive outranked rule would only get input frames where middle-manager is already bound thus ensuring that the recursion ends at some point
;; (Could we argue wih structural recursion here? That we have a sort of path from ?staff-person to ?boss and every step we shorten that by one)
;; Or is it enough to say that our database has a finite amount of statements?
;; Both of these arguments require that we have no cycles in the supervisor relation

;; Ex 4.65
;; Because he is the supervisor of three people one of whom has another three subordinates and another one the last, which makes him a wheel four times

;; Ex 4.66
;; He could first make all the instantiations of the query, then filter out all the duplicates and at last extract the variable values from the unique frames.


;;; IMPLEMENTATION
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-diver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to database.")
           (query-driver-loop))
          (else
           (newline)
           (display ouput-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (qeval query frame-stream)
  (let ((qproc (get (type query))))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))
(put 'and conjoin)

(define association-list '())
(define (put key proc)
  (set! association-list
        (cons (cons key proc)
              association-list)))
(define (get key)
  (let ((assoc (find (lambda (e) (eq? (car e) key))
                     association-list)))
    (if assoc
        (cdr assoc)
        (error "No procedure stored under that name" key))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))
(put 'or disjoin)

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
;; (define (negate operands frame-stream)
;;   (stream-filter
;;    (lambda (frame)
;;      (stream-null? (qeval (negated-query operands)
;;                           (singleton-stream frame))))
;;    frame-stream))

(put 'not negate)


(define (lisp-value call frame-stream)
  (stream-filter
   (lambda (frame)
     (execute (instantiate call frame (lambda (v f)
                                        (error "LISP-VALUE: Unbound pattern variable" v)))))
   frame-stream))

(put 'list-value lisp-value)


(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream)
  frame-stream)
(put 'always-true always-true)

