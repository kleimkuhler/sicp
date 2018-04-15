;; 4.45-4.49 on 4-12:1

;; 4.50
(define (shuffle seq)
  (define (iter seq acc)
    (if (null? seq)
	acc
	(let* ((ran (random (length seq)))
	       (elem (list-ref seq ran)))
	  (iter (remove seq elem)
		(cons elem acc)))))
  (iter seq '()))

;; 4.51
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (set-variable-value! var val env)
	       (succeed 'ok fail2))
	     fail))))

;; 4.52
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if (true? pred-value)
		   (cproc env succeed fail2)
		   (aproc env succeed fail2)))
	     fail))))

(define (analyze-if-fail exp)
  (let ((sproc (analyze (if-fail-success-procedure exp)))
	(fproc (analyze (if-fail-fail-procedure exp))))
    (lambda (env succeed fail)
      (sproc env
	     (lambda (value fail2)
	       (succeed value fail2))
	     (lambda ()
	       (fproc env succeed fail))))))

;; 4.53
;; (let ((pairs '()))
;;   (if-fail (let ((p (prime-sum-pairs '(1 3 5 8) '(20 35 110))))
;; 	     (permanent-set! pairs (cons p pairs))
;; 	     (amb))
;; 	   pairs))
;; > '((8 35) (3 110) (3 20))

;; 4.54
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if (not (true? pred-value))
		   (fail2)
		   (succeed 'ok fail2)))
	     fail))))
