#lang racket

;; --------------------------------------------------
;; Common defines used among exercises
(define (gcd a b)
  (if (= b 0.0)
      (abs a)
      (gcd b (remainder a b))))

(define (average a b)
  (/ (+ a b) 2))

;; --------------------------------------------------
;; Selector and Constructor procedures used in examples

(define (numer x) (car x))
(define (denom x) (cdr x))

;; Following along with reading
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (* n -1) g) (/ (* d -1) g))
        (cons (/ n g) (/ d g)))))

(define one-third (make-rat 1 3))
(define one-half (make-rat 1 2))

;; 2.2
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-point x y)
  (cons x y))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (make-segment a b) (cons a b))

(define (midpoint-segment seg)
  (cons (average (x-point (start-segment seg)) (x-point (end-segment seg)))
        (average (y-point (start-segment seg)) (y-point (end-segment seg)))))

;; 2.3
;; Corner implementation
;; Commented out for 3 point implementation
;; (define (make-rect a b) (cons a b))
;; (define (rect-width r)
;;    (abs (- (x-point (car r)) (x-point (cdr r)))))
;; (define (rect-height r)
;;    (abs (- (y-point (car r)) (y-point (cdr r)))))

;; 3 point implementation
(define (make-rect a w h) (cons a (cons w h)))
(define (rect-width r)
   (car (cdr r)))
(define (rect-height r)
   (cdr (cdr r)))

;; Generic perimeter and area solutions
(define (rect-perimeter r)
   (* 2 (+ (rect-width r) (rect-height r))))
(define (rect-area r)
   (* (rect-width r) (rect-height r)))

;; Following along with reading
(define (cons-proc-dispatch x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car-proc-dispatch z) (z 0))
(define (cdr-proc-dispatch z) (z 1))

;; 2.4
(define (cons-lambda x y)
  (lambda (m) (m x y)))

(define (car-lambda z)
  (z (lambda (p q) p)))

(define (cdr-lambda z)
  (z (lambda (p q) q)))

;; 2.5
;; This is essentially prime factorization problem
(define (largest-power z b)
  (define (iter z result)
    (if (= (remainder z b) 0)
        (iter (/ z b) (+ 1 result))
        result))
  (iter z 0))

(define (cons-product a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-product z)
  (largest-power z 2))

(define (cdr-product z)
  (largest-power z 3))

;; 2.6
(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
