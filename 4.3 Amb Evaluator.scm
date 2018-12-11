(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evalurating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence
                       (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((let*? exp) (analyze (let*->nested-lets exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "ANALYZE: Unknown expression type" exp))))

(define (analyze-self-evalurating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (let ((old-val (lookup-variable-value var env)))
        (vproc env
               (lambda (value fail2)
                 (set-variable-value! var value env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-val env)
                            (fail2))))
               fail)))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (value fail2)
               (define-variable! var value env)
               (succeed 'ok fail2))
             fail))))

;; Ex 4.52
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (value fail2)
               (if (true? value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-if-fail exp)
  (let ((fproc (analyze (if-fail-first exp)))
        (sproc (analyze (if-fail-second exp))))
    (lambda (env succeed fail)
      (fproc env
             succeed
             (lambda ()
               (sproc env
                      succeed
                      fail))))))

(define (analyze-lambda exp)
  (let ((bproc (analyze-sequence (lambda-body exp)))
        (params (lambda-parameters exp)))
    (lambda (env succeed fail)
      (succeed (make-procedure params bproc env)
               fail))))

(define (analyze-sequence exp)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail) (proc1 env
                                      (lambda (proc1-val fail2)
                                        (proc2 env succeed fail2))
                                      fail)))
  (let ((procs (map analyze exp)))
    (if (null? procs)
        (error "ANALYZE-SEQUENCE: Empty sequence")
        (fold-left sequentially
                     (car procs)
                     (cdr procs)))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (func-val fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application func-val args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error "EXECUTE-APPLICATION: Unknown procedure type" proc))))


(define (list-of-values operands env)
  (if (null? operands)
      '()
      (cons (eval (first-operand operands) env)
            (list-of-values (rest-operands operands) env))))

(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

;; Amb Syntax
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

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

;; Ex 4.52
(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-first exp) (cadr exp))
(define (if-fail-second exp) (caddr exp))

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
        body
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
             (list '= =)
             (list '> >)
             (list '< <)
             (list 'not not)
             (list 'abs abs)
             (list 'member member)
             (list 'even? even?)
             (list 'list list))))

(define custom-procedures
  '((define (an-element-of list)
      (if (null? list)
          (amb)
          (amb (car list)
               (an-element-of (cdr list)))))
    (define (require p)
      (if (not p)
          (amb)))
    (define (an-integer-between low high)
      (if (> low high)
          (amb)
          (amb low (an-integer-between (+ low 1) high))))
    (define (distinct? elements)
      (if (null? elements)
          #t
          (if (member (car elements) (cdr elements))
              #f
              (distinct? (cdr elements)))))
    (define (an-integer-starting-from low)
      (amb low
           (an-integer-starting-from (+ low 1))))
    (define (a-pythagorean-triple-between low high)
      (let ((i (an-integer-between low high)))
        (let ((j (an-integer-between i high)))
          (let ((k (an-integer-between j high)))
            (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k)))))
    (define (all-pythagorean-triples)
      (let* ((j (an-integer-starting-from 1))
             (i (an-integer-between 1 j))
             (ij (+ (square i) (square j)))
             (k (an-integer-between j (sqrt ij))))
        (require (= (square k) ij))
        (list i j k)))
    (define (multiple-dwelling)
      (let* ((fletcher (amb 1 2 3 4 5))
             (smith (amb 1 2 3 4 5)))
        (require (distinct? (list fletcher smith)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((cooper (amb 1 2 3 4 5)))
          (require (distinct? (list fletcher smith cooper)))
          (require (not (= (abs (- cooper fletcher)) 1)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
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
    (define (liars)
      (let* ((betty (amb 1 3))
             (ethel (amb 1 5))
             (joan (amb 2 3))
             (kitty (amb 2))
             (mary (amb 4)))
        (require (distinct? (list betty ethel joan kitty mary)))
        (list betty ethel joan kitty mary)))
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
        (require (eq? barnacle-yacht parker-daughter))
        (list moore-daughter downing-daughter hall-daughter barnacle-daughter parker-daughter)))
    ))

(define the-global-environment (setup-environment))
(for-each (lambda (procedure)
              (ambeval procedure
                       the-global-environment
                       (lambda (v f) v)
                       (lambda () ())))
            custom-procedures)

(define (apply-primitive-procedure proc args)
  "Uses the underlying scheme's apply. Careful not to shadow it"
  (apply (primitive-implementation proc)
         args))


;;; repl
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define new-problem-prompt ";;; Starting new problem")
(define no-alternatives ";;; There are no more alternatives of")

(define (repl)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? 'try-again input)
          (try-again)
          (begin
            (newline)
            (announce-output new-problem-prompt)
            (ambeval input
                     the-global-environment
                     (lambda (value next-alternative)
                       (announce-output output-prompt)
                       (user-print value)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output no-alternatives)
                       (user-print input)
                       (repl)))))))
  (internal-loop
   (lambda ()
     (newline)
     (user-print "There is no current problem")
     (repl))))

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

;; Ex 4.53
;; It would return a list of all the possible pythagorean triples because the amb causes every triple to fail until all are generated and then we jump to the second case in the if-fail

;; Ex 4.54
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (value fail2)
               (if (true? value)
                   (succeed 'ok fail2)
                   (fail2)))
             fail))))
