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
