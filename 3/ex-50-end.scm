(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; 3.50
(define (my-stream-map proc . args)
  (if (stream-null? args)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car args))
       (apply stream-map
	      (cons proc (map stream-cdr args))))))

;; 3.51
;; (define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0
;; ; Value: x

;; (stream-ref x 5)
;; 12345
;; ; Value: 5

;; (stream-ref x 7)
;; 67
;; ; Value: 7

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					 fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;; 3.53
;; 2^n

;; 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

;; 3.55
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

;; 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((< s2car s1car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car (merge (stream-cdr s1)
					    (stream-cdr s2)))))))))

;; Two merges needed

;; 3.57
;; expand is the decimal expansion of two the division of two numbers (radix is base)

;; 3.58
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (div-streams s integers))

;; Formulating iterations as stream processes
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)

;; pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(define (stream-limit s tolerance)
  (if (< (abs (- (stream-ref s 0) (stream-ref s 1))) tolerance)
      (stream-ref s 1)
      (stream-limit (stream-cdr s) tolerance)))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

;; 3.67
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x))
			    (stream-cdr t))
		(stream-map (lambda (x) (list x (stream-car t)))
			    (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)

(define (rc r c dt)
  (lambda (i initial-value)
    (add-streams (scale-stream i r)
		 (integral (scale-stream i (/ 1 c)) initial-value dt))))

;; 3.74
;; (define zero-crossings
;;   (stream-map sign-change-detector sense-data (cdr sense-data)))

;; (define zero-crossings
;;   (stream-map sign-change-detector sense-data (cons 0 sense-data)))

;; 3.75
;; Rolling average needs to have the last average value in the calculation or
;; the number of data points it is calculated from

;; 3.76
(define (smooth stream)
  (stream-map (lambda (x y) (/ (+ x y) 2))
	      input-stream (stream-cdr input-stream)))

;; Reading
(define (integral-delayed integrand-delayed initial-value dt)
  (define int
    (cons-stream initial-value
		 (let ((integrand (force integrand-delayed)))
		   (add-streams (scale-stream integrand dt)
				int))))
  int)

;; Let's this be possible!
(define (solve f y0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; Reimplementation of stream withdraw to consider
(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
		    (stream-cdr amount-stream))))
