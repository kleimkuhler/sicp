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
