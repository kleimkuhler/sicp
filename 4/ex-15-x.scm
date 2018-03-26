;; 4.16
;; a.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (let ((val (car vals)))
	       (if (equal? val '*unassigned*)
		   (error "Unbound variable" var)
		   val)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;; b.
;; Consider with `filter`
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (make-set! exp)
  (cons 'set! (cons (definition-variable exp) (definition-value exp))))

(define (scan-out-defines body)
  (define (names defines)
    (map (lambda (exp) (list (definition-variable exp) '*unassigned)) defines))
  (define (values defines)
    (map (lambda (exp) (make-set! exp)) defines))
  (define (defines->let exps defines non-defines)
    (cond ((null? exps)
	   (if (null? defines)
	       body
	       (cons (make-let (names defines)
			       (values defines))
		     (reverse non-defines))))
	  ((definition? (car exps))
	   (defines->let (cdr exps) (cons (car exps) defines) non-defines))
	  (else
	   (defines->let (cdr exps) defines (cons (car exps) non-defines)))))
  (defines->let body '() '()))

;; c.
;; Belongs in `make-procedure`
