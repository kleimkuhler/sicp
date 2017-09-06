#lang scheme

#|
All functions are defined below. No executions take place
when run; use the REPL to test
|#

;;; shared functions
(define (square x) (* x x))

(define (even? n)
    (= (remainder n 2) 0))

;;; 1.3
(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (sum-of-squares-larger a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (< b c)) (sum-of-squares a c))
        ((and (< c a) (< c b)) (sum-of-squares a b))))

;;; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;; 1.5
;;; iterative approach to square root without any iterative constructs
(define (sqrt-iter guess oldguess x) 
   (if (good-enough? guess oldguess) 
       guess 
       (sqrt-iter (improve guess x) guess 
                  x)))

(define (good-enough? guess oldguess) 
   (< (abs (- guess oldguess)) 
      (* guess 0.001)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))

;;; 1.6
;;; else-clause evaluates indefinitely because of applicative-order evaluation
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-1.6 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-1.6 (improve guess x)
                     x)))

(define (sqrt-1.6 x)
  (sqrt-iter-1.6 1.0 x))

;;; 1.7
;;; fixed good-enough? to work for small/large numbers

;;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;; 1.11
(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3)))))))

;;; 1.16
(define (expt b n)
  (define (expt-iter b n acc)
    (cond ((= n 0) acc)
          ((even? n) (expt-iter (square b) (/ n 2) acc))
          (else (expt-iter b (- n 1) (* acc b)))))
  (expt-iter b n 1))

;;; 1.18
(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (mult cand plier)
  (define (mult-iter cand plier acc)
    (cond ((= plier 0) acc)
          ((even? plier) (mult-iter (double cand) (halve plier) acc))
          (else (mult-iter cand (- plier 1) (+ acc cand)))))
  (mult-iter cand plier 0))

;;; 1.19
;;; this was just realizing this was solving a systems of equations
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 (* p q)) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
