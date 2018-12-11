(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ;;; Ex 4.4 a
        ;; ((and? exp) (eval-and exp env))
        ;; ((or? exp) (eval-or exp env))
        ;;; Ex 4.4 b
        ((and? exp) (eval (and->let exp) env))
        ((or? exp) (eval (or->let exp) env))
        ;;; Ex 4.6
        ((named-let? exp) (eval (named-let->let* exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ;;; Ex 4.7
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ;;; Ex 4.20
        ((letrec? exp) (eval (letrec->let exp) env))
        ((application? exp)
         (my/apply (actual-value (operator exp) env)
                   (operands exp)
                   env))
        (else
         (error "EVAL: Unknown expression type" exp))))

(define (my/apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure
                                    (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              (list-of-delayed-args arguments env)
                              (procedure-environment procedure))))
        (else
         (error "APPLY: Unkown procedure" procedure))))

(define (list-of-arg-values args env)
  (map (lambda (arg) (actual-value arg env))
       args))

(define (list-of-delayed-args args env)
  (map (lambda (arg) (delay-it arg env))
       args))

(define (actual-value exp env)
  (memoized/force-it (eval exp env)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-expression obj)
                       (thunk-environment obj)))
        (else obj)))
(define (memoized/force-it obj)
  (cond ((evaluated-thunk? obj)
         (thunk-expression obj))
        ((thunk? obj)
         (let ((value (actual-value (thunk-expression obj)
                                    (thunk-environment obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) value)
           (set-cdr! (cdr obj) '())
           value))
        (else obj)))

(define (thunk? exp) (tagged-list? exp 'thunk))
(define (evaluated-thunk? exp) (tagged-list? exp 'evaluated-thunk))
(define (thunk-expression thunk) (cadr thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (thunk-environment thunk) (caddr thunk))
  
(define (list-of-values operands env)
  (map (lambda (operand) (eval operand env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

(define (eval-sequence seq env)
  (if (last-exp? seq)
      (eval (first-exp seq) env)
      (begin
        (eval (first-exp seq) env)
        (eval-sequence (rest-exp seq) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; Syntax rules
(define (self-evaluating? exp)
  (or (number? exp) (string? exp)
      (eq? #f exp) (eq? #t exp)
      (null? exp)))

(define variable? symbol?)

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (null? (cdddr exp))
      #f
      (cadddr exp)))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-recipient-clause? clause)
  (eq? '=> (cadr clause)))
(define (cond-recipient clause) (caddr clause))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        (error "COND: Need at least one clause.")
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (cond ((cond-else-clause? first)
                 (cond ((null? rest) (sequence->exp (cond-actions first)))
                       ((cond-recipient-clause? first) (error "COND: else clause must not be a recipient clause"))
                       (else (error "COND: else clause must be the last clause" clauses))))
                ;;; Ex 4.5
                ((cond-recipient-clause? first)
                 (make-let (list (list 'val
                                       (cond-predicate first)))
                           (list
                            (make-if 'val
                                     (list (cond-recipient first)
                                           'val)
                                     (expand-clauses rest)))))
                (else (make-if (cond-predicate first)
                               (sequence->exp (cond-actions first))
                               (expand-clauses rest)))))))
  (expand-clauses (cond-clauses exp)))


;;; Ex 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (and-expressions exp) (cdr exp))
(define (last-and-exp? exps) (null? (cdr exps)))
(define (or? exp) (tagged-list? exp 'or))
(define (or-expressions exp) (cdr exp))
(define (last-or-exp? exps) (null? (cdr exps)))

;;; Ex 4.4 a
;; (define (eval-and exp env)
;;   (define (eval-and-exps exps env)
;;     (if (last-and-exp? exps)
;;         (eval (car exps) env)
;;         (if (eval (car exps) env)
;;             (eval-and-exps (cdr exps) env)
;;             #f)))
;;   (let ((exps (and-expressions exp)))
;;     (if (null? exps)
;;         #t
;;         (eval-and-exps exps env))))

;; (define (eval-or exp env)
;;   (define (eval-or-exps exps env)
;;     (if (last-or-exp? exps)
;;         (eval (car exps) env)
;;         (if (eval (car exps) env)
;;             #t
;;             (eval-or-exps (cdr exps) env))))
;;   (let ((exps (or-expressions exp)))
;;     (if (null? exps)
;;         #f
;;         (eval-or-exps exps env))))

;;; Ex 4.4 b
(define (and->if exp)
  (define (expand-expressions exps i)
    (if (last-and-exp? exps)
        (list (number->symbol i))
        (make-let (list (list 'val
                              (list (number->symbol i))))
                  (list
                   (make-if 'val
                            (expand-expressions (cdr exps) (1+ i))
                            #f)))))
  (let ((exps (and-expressions exp)))
    (if (null? exps)
        #t
        (make-let (make-bindings exps)
                  (list
                   (expand-expressions exps 1))))))

(define (or->if exp)
  (define (expand-expressions exps i)
    (if (null? exps)
        #f
        (make-let (list (list 'val
                              (list (number->symbol i))))
                  (list
                   (make-if 'val
                            'val
                            (expand-expressions (cdr exps) (+ i 1)))))))
  (make-let (make-bindings (or-expressions exp))
            (list
             (expand-expressions (or-expressions exp) 1))))

(define (number->symbol x) (string->symbol (number->string x)))
(define (make-bindings exps)
  "Returns a list of bindings suitable for let from a list of expressions. E.g. (1 (* 2 3)) -> ((|1| 1) (|2| (* 2 3)))"
  (map (lambda (exp i)
         (list (number->symbol i)
               (make-lambda '() (list exp))))
       exps
       (enumerate-interval 1 (length exps))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a
            (enumerate-interval (+ a 1) b))))

;;; Ex 4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (make-let bindings body)
  (cons 'let (cons bindings
                   body)))

(define (let->combination exp)
  (let ((parameters (map car (let-bindings exp)))
        (arguments (map cadr (let-bindings exp))))
    (cons (make-lambda parameters
                       (let-body exp))
          arguments)))

;;; Ex 4.7
(define (let*? exp) (tagged-list? exp 'let*))
(define (make-let* bindings body)
  (cons 'let* (cons bindings body)))

(define (let*->nested-lets exp)
  (define (expand-lets bindings)
    (if (null? bindings)
        (let-body exp)
        (list
         (make-let (list (car bindings))
                   (expand-lets (cdr bindings))))))
  (let ((bindings (let-bindings exp)))
    (car (expand-lets bindings))))

;;; Ex 4.8
(define (named-let? exp) (tagged-list? exp 'named-let))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

(define (named-let->let* exp)
  (let ((parameters (map car (named-let-bindings exp)))
        (arguments (map cadr (named-let-bindings exp)))
        (name (named-let-name exp))
        (func (generate-uninterned-symbol)))
    (make-let* (list (list name ''*unassigned*)
                     (list func
                           (make-lambda parameters
                                        (named-let-body exp))))
               (list
                (list 'set! name func)
                (cons name arguments)))))


;;; 4.1.3 Evaluator Data Structures
;; Ex 4.16
(define (scan-out-defines body)
  (define defined-symbols
    (map definition-variable
         (filter (lambda (exp)
                   (definition? exp))
                 body)))
  ;; replace define with set! in body (filter does not crate copys)
  ;; todo reimplement without set
  (if (null? defined-symbols)
      body
      (list
       (make-let (map (lambda (var)
                        (list var ''*unassigned*
                              ))
                      defined-symbols)
                 (map (lambda (exp)
                        (if (definition? exp)
                            (list 'set!
                                  (definition-variable exp)
                                  (definition-value exp))
                            exp))
                      body)))))

(define (make-procedure parameters body env)
  (list 'procedure parameters
        (scan-out-defines body)
        env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (frame-bindings frame) (cdr frame))

(define (make-frame variables values)
  (cons '*frame*
        (map (lambda (var val)
               (list var val))
             variables values)))
(define (add-binding! frame var val)
  (set-cdr! frame (cons (list var val)
                        (cdr frame))))

(define (extend-environment-assoc alist base-env)
  (if (alist? alist)
      (cons (cons '*frame* alist)
            base-env)
      (error "Malformed association list" alist)))
(define (extend-environment variables values base-env)
  (if (= (length variables) (length values))
      (cons (make-frame variables values)
            base-env)
      (error (if (< (length variables) (length values))
                 "Too many arguments supplied"
                 "Too few arguments supplied")
             variables values)))

(define (lookup-variable-value var env)
  (let ((val (env-loop env var
                       (lambda (binding) (cadr binding))
                       (lambda (env) (lookup-variable-value var (enclosing-environment env))))))
    (if (eq? val '*unassigned*)
        (error "Trying to use unassigned variable" var)
        val)))
(define (set-variable-value! var val env)
  (env-loop env var
            (lambda (binding) (set-car! (cdr binding) val))
            (lambda (env) (set-variable-value! var val (enclosing-environment env)))))
(define (define-variable! var val env)
  (env-loop env var
            (lambda (binding) (set-car! (cdr binding) val))
            (lambda (env) (add-binding! (first-frame env) var val))))

(define (env-loop env var success failure)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((binding (assoc var (frame-bindings (first-frame env)))))
        (if binding
            (success binding)
            (failure env)))))


(define (setup-environment)
  (extend-environment-assoc primitive-procedures
                            the-empty-environment))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (map (lambda (binding) (list (car binding)
                               (list 'primitive (cadr binding))))
       (list (list 'car car)
             (list 'cdr cdr)
             (list 'cons cons)
             (list 'null? null?)
             (list 'eq? eq?)
             (list '+ +)
             (list '- -)
             (list '* *)
             (list '/ /)
             (list '= =))))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  "Uses the underlying scheme's apply. Careful not to shadow it"
  (apply (primitive-implementation proc)
         args))


;;; repl
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (repl)
  (prompt-for-input input-prompt)
  (let* ((input (read))
         (output (actual-value input the-global-environment)))
    (if (eq? output 'exit)
        "Exiting"
        (begin
          (announce-output output-prompt)
          (user-print output)
          (repl)))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; Ex 4.20
(define (letrec? exp) (tagged-list? exp 'letrec?))
(define (letrec->let exp)
  (let ((dummy-bindings (map (lambda (name)
                               (list name ''*unassigned*))
                             (map car (let-bindings exp))))
        (dummy-setters (map (lambda (binding)
                              (cons 'set! binding))
                            (let-bindings exp))))
    (append `(let ,dummy-bindings)
            dummy-setters
            (let-body exp))))

;; Ex 4.27
;; after the definition of w count is 1 because the outer id is actually executed. But the argument to id - (id 10) - is delayed until w is forced so after that count is 2

;; Ex 4.28
;; We need to use actual-value to be able to force the operator if it was delayed. This is neccessary when we look at higher order functions. When we pass a function to another function, the function argument gets delayed. If we now want to apply this function we first need to force it

;; Ex 4.29
;; If the same object would be forced many times, for example when you evaluate (square (square ... (square (id 1)) ... )), then memoizing the result speeds things up a lot. Also since (id 1) is only evaluated once count is 1 while it's a higher power of two when we don't memoize.

;; Ex 4.30
;; a. Because newline and display are not and don't return thunks so their side effects happen directly
;; b. (p1 1) returns (1 2)
;;    (p2 1) return 1 since when e is evaluated it just returns a thunk so nothing happens. When we use Cy's eval-sequence instead is evaluates to (1 2)
;; c. That's true because e.g. newline and display probably just return a symbol and forcing that does not do anything
;; d. I'm not sure if what Cy is doing in his examples makes sense because it's hard to reason about side effects in a lazyly evaluated language. Therefore I would probably go the haskell route and disallow sequences and use Monads if I really need them.
