;; 4.11
(define (make-frame variables values)
  (cons 'table
	(map cons variables values)))

(define (frame-variables frame)
  (map car (cdr frame)))

(define (frame-values frame)
  (map cdr (cdr frame)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame)))
  frame)

;; 4.12
;; Continuations?!
(define (env-loop var env err succ)
  (define (scan vars vals)
    (cond ((null? vals) (err))
	  ((eq? var (car vars)) (succ vars vals))
	  (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
	(scan (frame-variables frame)
	      (frame-values frame)))))

(define (lookup-variable-value var env)
  (define (err)
    (lookup-variable-value var (enclosing-environment env)))
  (env-loop var env err car))

(define (set-variable-value! var val env)
  (define (err)
    (set-variable-value! var val (enclosing-environment env)))
  (env-loop var env err (lambda (vars vals) (set-car! vals val))))

(define (define-variable! var val env)
  (define (err)
    (add-binding-to-frame! var val env))
  (env-loop var env err (lambda (vars vals) (set-car! vals val))))
