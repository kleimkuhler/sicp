#lang racket

;; 2.53
;; Notepad exercises

;; 2.54
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (eq? a b)))

;; 2.55
;; Not exactly an exercise but thought this was interesting
(define (ex-2.55) (car ''hello))

;; Following along with reading
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? a) (symbol? a))
(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (make-sum-prefix a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list '+ a b))))
(define (sum-prefix? a)
  (and (pair? a) (eq? (car a) '+)))
(define (addend-prefix a) (cadr a))

(define (make-product-prefix a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b) (* a b)))
        (else (list '* a b))))
(define (product-prefix? a)
  (and (pair? a) (eq? (car a) '*)))
(define (multiplier-prefix a) (cadr a))

;; 2.56
(define (make-exponentiation a b)
  (cond ((=number? b 0) 1)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (expt a b))
        (else (list '** a b))))
(define (exponentiation? a)
  (and (pair? a) (eq? (car a) '**)))
(define (base a) (cadr a))
(define (exponent a) (caddr a))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; 2.57
(define (augend-prefix a)
  (if (null? (cdddr a))
      (caddr a)
      (cons '+ (cddr a))))

(define (multiplicand-prefix a)
  (if (null? (cdddr a))
      (caddr a)
      (cons '* (cddr a))))

;; 2.58
(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list a '+ b))))
(define (sum? a)
  (and (pair? a) (eq? (cadr a) '+)))
(define (addend a) (car a))
(define (augend a) (caddr a))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b) (* a b)))
        (else (list a '* b))))
(define (product? a)
  (and (pair? a) (eq? (cadr a) '*)))
(define (multiplier a) (car a))
(define (multiplicand a) (caddr a))

;; Following along with reading
(define (element-of-set? a set)
  (cond ((null? set) #f)
        ((equal? a (car set)) #t)
        (else (element-of-set? a (cdr set)))))

(define (adjoin-set a set)
  (if (element-of-set? a set)
      set
      (cons a set)))

(define (intersection-set a b)
  (cond ((or (null? a) (null? b)) '())
        ((element-of-set? (car a) b) (cons (car a) (intersection-set (cdr a) b)))
        (else (intersection-set (cdr a) b))))

;; 2.59
(define (union-set a b)
  (if (null? a)
      b
      (union-set (cdr a) (adjoin-set (car a) b))))

;; alt that I like!
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (union-set-acc a b)
  (accumulate adjoin-set b a))

;; 2.60
;; element-of-set? remains
;; intersection-set can remain. Duplicates show when given in
;; first set. Technically the sets are equal whether there are
;; duplicates or not, so this shouldn't matter
(define (adjoin-set-dup a set) (cons a set))

(define (union-set-dup a b) (append a b))

;; 2.61
