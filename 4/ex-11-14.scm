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
  (define (scan pre-vars pre-vals vars vals)
    (cond ((null? vars) (err))
	  ((eq? var (car vars)) (succ pre-vars pre-vals vars vals))
	  (else (scan vars vals (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let* ((frame (first-frame env))
	     (vars (frame-variables frame))
	     (vals (frame-values frame)))
	(scan ('() '() vars vals)))))

(define (lookup-variable-value var env)
  (define (err)
    (lookup-variable-value var (enclosing-environment env)))
  (define (succ) (caddr car))
  (env-loop var env err succ))

(define succ-set-frame-binding!
  (lambda (pre-vars pre-vals vars vals)
    (set-car! vals val)))

(define (set-variable-value! var val env)
  (define (err)
    (set-variable-value! var val (enclosing-environment env)))
  (env-loop var env err succ-set-frame-binding!))

(define (define-variable! var val env)
  (define (err)
    (add-binding-to-frame! var val env))
  (env-loop var env err succ-set-frame-binding!))

;; 4.13
(define (make-unbound var env)
  (define (err) #t)
  (define (succ)
    (lambda (pre-vars pre-vals vars vars)
      (begin (set-cdr! pre-vars (cdr vars))
	     (set-cdr! pre-vals (cdr vals)))))
  (env-loop var env err succ))

;; Meetup: Discussion on primitive expressions searching all the way back to
;;         the-global-environment.

;; 4.14
;; System implementation of map is different from Eva's.
