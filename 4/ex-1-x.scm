;; 4.1
;; Original implementation of `list-of-values` that is order dependent
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; Left to right
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      ((lambda (first-val)
	 (cons first-val
	       (list-of-values-lr (rest-operands exps) env)))
       (eval (first-operand exps) env))))

;; Right to left
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      ((lambda (rest-vals)
	 (cons (eval (first-operand exps) env)
	       rest-vals))
       (list-of-values-rl (rest-operands exps) env))))

;; 4.2
;; a). Procedure application clause appearing before assignment clauses causes
;;     issues because procedure application checks for `pair?` and the
;;     assigment clause would satisfy that predicate.

;; b).
(define (call? exp)
  (tagged-list? exp 'call))


;; 4.3
(define eval-table make-table)
(define get (eval-table 'lookup-proc))
(define put (eval-table 'insert-proc))

(put 'op 'quote text-of-quotation)
(put 'op 'set! eval-assignment)
(put 'op 'define eval-definition)
(put 'op 'if eval-if)
(put 'op 'lambda (lambda (exp env)
		   (make-procedure (lambda-parameters exp)
				   (lambda-body exp)
				   env)))
(put 'op 'begin (lambda (exp env)
		  (eval-sequence (begin-actions exp) env)))
(put 'op 'cond (lambda (exp env)
		 (eval (cond-if exp) env)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((get 'op (car exp)) => (lambda (proc)
				  (proc exp env)))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; 4.4
;; Representing and/or expressions
(define (eval-and exps env)
  (let ((val (eval (first-exp exps) env)))
    (cond ((last-exp? exps) val)
	  (else (if (true? val)
		    (eval-and (rest-exps exps) eval)
		    val)))))

(define (eval-or exps env)
  (let ((val (eval (first-exp exps) env)))
    (cond ((last-exp? exps) val)
	  (else (if (false? val)
		    (eval-or (rest-exps exps) eval)
		    val)))))

;; Deriving and/or expressions
(define (and-clauses exp) (cdr exp))
(define (or-clauses exp) (cdr exp))

(define (and->if exp)
  (expand-and-exp (and-exp exp)))
(define (or->if exp)
  (expand-or-exp (or-exp exp)))

(define (expand-and-exp exp)
  (if (null? exp)
      '#t
      (make-if (first-exp exp)
	       (expand-and-exp (rest-exp exp))
	       '#f)))

(define (expand-or-exp exp)
  (if (null? exp)
      '#f
      (make-if (first-exp exp)
	       '#t
	       (expand-or-exp (rest-exp exp)))))

;; 4.5
(define (expand-clasues clauses)
  (if (null? clauses)
      '#f
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clauses? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    ()))))
