#lang racket

;; --------------------------------------------------
;; Selector and Constructor procedures used in examples

(define (numer x) (car x))
(define (denom x) (cdr x))

;; Following along with reading
(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-third (make-rat 1 3))
(define one-half (make-rat 1 2))

;; 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (* n -1) g) (/ (* d -1) g))
        (cons (/ n g) (/ d g)))))