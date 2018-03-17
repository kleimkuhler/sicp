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
