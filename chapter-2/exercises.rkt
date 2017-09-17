#lang racket

;; --------------------------------------------------
;; Common defines used among exercises
(define (gcd a b)
  (if (= b 0)
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