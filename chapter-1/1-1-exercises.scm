#lang racket

#|
All functions are defined below. No executions take place
when run; use the REPL to test
|#

;;; shared functions
(define (even? n)
    (= (remainder n 2) 0))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

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
       (sqrt-iter (improve guess x) guess x)))

(define (good-enough? guess oldguess) 
   (< (abs (- guess oldguess)) 
      (* guess 0.001)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-old-1 x)
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
(define (double-1-18 x) (+ x x))

(define (halve x) (/ x 2))

(define (mult cand plier)
  (define (mult-iter cand plier acc)
    (cond ((= plier 0) acc)
          ((even? plier) (mult-iter (double-1-18 cand) (halve plier) acc))
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

;;; Following along with 1.2.6
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;; 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))))

(define (report-prime elapsed-time)
  (display " * ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (even? start)
      (search-for-primes (+ start 1) end)
      (cond ((< start end) (timed-prime-test start)
                           (search-for-primes (+ start 2) end)))))

;;; 1.23
(define (next x)
  (if (= x 2) 3 (+ x 2)))

;;; 1.27
(define (mod-expt-equal? n a)
  (= (remainder (expt a n) n) a))

(define (congruent-modulo? n)
  (define (cong-mod n a)
    (cond ((= a n) #t)
          ((mod-expt-equal? n a) (cong-mod n (+ a 1)))
          (else #f)))
  (cong-mod n 1))

;;; 1.27 alt (reworking functions above)
(define (fermat-test-carmichael n a)
   (= (expmod-mr a n n) a))

(define (fermat-full n)
   (define (iter a)
     (cond ((= a 1) #t)
           ((not (fermat-test-carmichael n a)) #f)
           (else (iter (- a 1)))))
   (iter (- n 1)))

;;; 1.28
(define (square-check? m x)
  (if (and (not (or (= x 1) (= x (- m 1))))
           (= (remainder (square x) m) 1))
      0
      (remainder (square x) m)))

(define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-check? m (expmod-mr base (/ exp 2) m)))
        (else
         (remainder (* base (expmod-mr base (- exp 1) m)) m))))

;;; 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((even? k) 2)
             (else 4))
       (y k)))
  (* (sum-iter term a inc n)
     (/ h 3)))

;;; 1.30
(define (sum-integers a b)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (sum-iter identity a inc b))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;;; 1.31
;;; iterative solution
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a ) result))))
  (iter a 1))

;;; recursive solution
(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a) (product-recur term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

;;; original solution
(define (pi-product n)
  (define (pi-term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1.0))
        (/ (+ x 1) (+ x 2.0))))
  (* 4 (product pi-term 1 inc n)))

;;; solution after learning wallis product had formula to calculate
;;;   term n
(define (wallis-product n)
  (define (pi-term x)
    (/ (* 4.0 (square x)) (- (* 4.0 (square x)) 1)))
  (define (pi-next x) (+ x 1))
  (* 2 (product pi-term 1 pi-next n)))

;;; 1.32
;;; iterative solution to a generic accumulator
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; recursive solution
(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner null-value term (next a) next b))))

;;; wallis-product using accumulate instead of product of values
(define (wallis-product-accumulate n)
  (define (pi-term x)
    (/ (* 4.0 (square x)) (- (* 4.0 (square x)) 1)))
  (define (pi-next x) (+ x 1))
  (* 2 (accumulate * 1 pi-term 1 pi-next n)))

;;; 1.33
;;; iterative solution to a filtered accumulator
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) (combiner null-value result)))))
  (iter a null-value))

(define (filtered-sum-evens a b)
  (filtered-accumulate even? + 0 identity a inc b))

(define (sum-squared-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (product-relative-primes n)
  (define (gcd a b)
   (if (= b 0)
       a
       (gcd b (remainder a b))))
  (define (coprime? x)
    (= (gcd n x) 1))
  (filtered-accumulate coprime? * 1 identity 1 inc (- n 1)))

;;; following along with 1.3
(define (f-define x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f-lambda x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-old-2 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;; 1.35
(define (fixed-point-gr)
  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

;;; 1.36
(define (fixed-point-pretty f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-x^x)
  (fixed-point-pretty (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (fixed-point-x^x-average)
  (fixed-point-pretty (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))

;;; 1.37
;;; recursive solution with n & d procedures
(define (cont-frac-recur n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))

;;; recursive solution with n & d as arguments
;;; I feel unclear on why the exercise asks n & d to be passed in as
;;;   procedures that always return 1.0, instead of how this does it
(define (cont-frac-recur-test n d k)
  (define (frac i)
    (if (< i k)
        (/ n (+ d (frac (+ i 1))))
        (/ n d)))
  (frac 1))

;;; iterative solution
;;; this solutions needs to sum up to i = 0 because of the iterative
;;;   nature, it builds from the leaf nodes into the root
(define (cont-frac-iter n d k)
  (define (frac i result)
    (if (= i 0)
        result
        (frac (- i 1) (/ (n i) (+ (d i) result)))))
  (frac (- k 1) (/ (n k) (d k))))

;;; 1.38
(define (e k)
  (+ 2 (cont-frac-iter
        (lambda (i) 1.0)
        (lambda (i)
          (if (not (= 0 (remainder (+ i 1) 3)))
              1
              (* 2 (/ (+ i 1) 3))))
        k)))

;;; 1.39
(define (tan-cf x k)
  (cont-frac-iter
   (lambda (i)
     (if (= i 1)
         x
         (- (square x))))
   (lambda (i)
     (- (* i 2.0) 1.0))
   k))

;;; following along 1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-old-3 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-old-4 x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-avg-damp x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (sqrt-newton-trans x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

;;; 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (cubert a b c)
  (newtons-method (cubic a b c) 1.0))

#|
;;; lambda implementation
((lambda (a b c)
     (newtons-method (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)) 1.0))
   1 2 3)
|#

;;; 1.41
(define (double f)
  (lambda (x) (f (f x))))

;;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;;; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;;; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.0)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;;; 1.45
