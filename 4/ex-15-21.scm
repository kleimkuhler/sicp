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

;; 4.18
;; Procedure should work because of `delay`

;; 4.19
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;; Errors because this is generally just a poorly written procedure

;; Best comparison of 4.16 vs 4.18. These are not fundamentally different; 4.18
;; enforces the restriction that variable values can be evaluated without using
;; any of the variable's values

;; 4.20
;; make-begin?
(define (letrec->let exp)
  (make-let (create-variables exp)
	    (cons (set-variables exp) (letrec-body exp))))

;; 4.21
;; Solution 1
;; ((lambda 👎
;;    ((lambda (fib)
;;       (fib fib n))
;;     (lambda (g k)
;;       (cond
;;        [(= k 0) 0]
;;        [(= k 1) 1]
;;        [else (+ (g g (- k 1)) (g g (- k 2)))]))))
;;  10)

;; Solution 2
;; Tail-call optimized
;; ((lambda 👎
;;    ((lambda (fib)
;;       (fib fib 0 1 0))
;;     (lambda (iter a b k)
;;       (if (= k n)
;; 	  a
;; 	  (iter iter b (+ a b) (+ k 1))))))
;;  10)

;; This exercise is essentially Y combinator
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define fact
  (lambda (f)
    (lambda (n)
      (if (= n 0)
	  1
	  (* n (f (- n 1)))))))
