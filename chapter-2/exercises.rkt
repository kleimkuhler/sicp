#lang racket

;; --------------------------------------------------
;; Common defines used among exercises
(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))
(define (average a b) (/ (+ a b) 2))
(define (inc x) (+ x 1))

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
(define church-zero
  (lambda (f) (lambda (x) x)))

(define church-one
  (lambda (f) (lambda (x) (f x))))

(define church-two
  (lambda (f) (lambda (x) (f (f x)))))

(define (church-add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (church-add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))


;; Following along with reading
;; Rp = (/ 1 (+ (/ 1 R1) (/ 1 R2)))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p2 p2 p3 p4))))

;; 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound int) (cdr int))
(define (lower-bound int) (car int))

;; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; 2.9
;; Tests the width of intervals to assert that
;; (= (+ (width a) (width b)) (width (add-interval a b)))
(define (width-old int)
  (/ (- (upper-bound int) (lower-bound int)) 2))

;; 2.10
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "Error: The denominator can not span 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;; 2.11
(define (mul-interval-bitdiddle x y)
  (let ((x-lo (lower-bound x))
        (x-hi (upper-bound x))
        (y-lo (lower-bound y))
        (y-hi (upper-bound y)))
    (cond ((and (>= x-lo 0)
                (>= x-hi 0)
                (>= y-lo 0)
                (>= y-hi 0))
           ;; (+.+)*(+.+)
           (make-interval (* x-lo y-lo) (* x-hi y-hi)))
          ((and (<= x-lo 0)
                (>= x-hi 0)
                (>= y-lo 0)
                (>= y-hi 0))
           ;; (-.+)*(+.+)
           (make-interval (* x-lo y-hi) (* x-hi y-hi)))
          ((and (<= x-lo 0)
                (<= x-hi 0)
                (>= y-lo 0)
                (>= y-hi 0))
           ;; (-.-)*(+.+)
           (make-interval (* x-lo y-hi) (* x-hi y-lo)))
          ((and (>= x-lo 0)
                (>= x-hi 0)
                (<= y-lo 0)
                (>= y-hi 0))
           ;; (+.+)*(-.+)
           (make-interval (* x-hi y-lo) (* x-hi y-hi)))
          ((and (>= x-lo 0)
                (>= x-hi 0)
                (<= y-lo 0)
                (<= y-hi 0))
           ;; (+.+)*(-.-)
           (make-interval (* x-hi y-lo) (* x-lo y-hi)))
          ((and (<= x-lo 0)
                (>= x-hi 0)
                (<= y-lo 0)
                (>= y-hi 0))
           ;; (-.+)*(-.+)
           (make-interval (min (* x-lo y-hi) (* x-hi y-lo))
                          (max (* x-lo y-lo) (* x-hi y-hi))))
          ((and (<= x-lo 0)
                (<= x-hi 0)
                (<= y-lo 0)
                (>= y-hi 0))
           ;; (-.-)*(-.+)
           (make-interval (* x-lo y-hi) (* x-lo y-lo)))
          ((and (<= x-lo 0)
                (>= x-hi 0)
                (<= y-lo 0)
                (<= y-hi 0))
           ;; (-.+)*(-.-)
           (make-interval (* x-hi y-lo) (* x-lo y-lo)))
          ((and (<= x-lo 0)
                (<= x-hi 0)
                (<= y-lo 0)
                (<= y-hi 0))
           ;; (-.-)*(-.-)
           (make-interval (* x-hi y-hi) (* x-lo y-lo))))))

;; Following along with reading
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; 2.12
(define (make-center-percent c p)
  (make-interval (* c (- 1.0 p)) (* c (+ 1.0 p))))

(define (percent i)
  (/ (width i) (center i)))

;; 2.13
;;  (/ (- (* (+ 1 pa) (+ 1 pb)) (* (- 1 pa) (- 1 pb)))
;;     (+ (* (+ 1 pa) (+ 1 pb)) (* (- 1 pa) (- 1 pb))))
;; where
;;   pa = percentage tolerance of interval a
;;   pb = percentage tolerance of interval b
;; Assuming small enough percentages a more general estimation
;; would be pa+pb

;; Following along with reading
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; 2.14 & 2.15
;; The transformation from par2 to par1 requires multiplying by
;; R1/R2 and R2/R2. However we prove that R1/R1 (and R2/R2) do
;; not evaluate to 1, just an approximation of 1. Therefore par2
;; is more accurate, but it is not possible to avoid this in
;; cases where repetition is required

;; Following along with reading
(define (list-ref-cust items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length-recur items)
  (if (null? items)
      0
      (+ 1 (length-recur (cdr items)))))
(define (length-iter items)
  (define (length-iter-ins a count)
    (if (null? a)
        count
        (length-iter-ins (cdr a) (+ 1 count))))
  (length-iter-ins items 0))

(define odds (list 1 3 5 7))

(define (append-cust list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append-cust (cdr list1) list2))))

;; 2.17
(define (last-pair-cust items)
  (if (null? (cddr items))
      (cdr items)
      (last-pair-cust (cdr items))))

;; 2.17 alt (need to explicited make result a list)
(define (last-pair-cust-alt items)
  (if (null? (cdr items))
      (list (car items))
      (last-pair-cust-alt (cdr items))))

;; 2.18
(define (reverse-cust items)
  (define (reverse-cust-ins items result)
    (if (null? items)
        result
        (reverse-cust-ins (cdr items) (cons (car items) result))))
  (reverse-cust-ins items '()))

;; 2.18 alt (recursive though)
(define (reverse-cust-alt items)
  (if (null? items)
      items
      (append (reverse-cust-alt (cdr items)) (list (car items)))))

;; 2.19
(define us-coins (list 50 25 10 5 1))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin-values)) 0)
        (else
         (+ (cc amount (cdr coin-values))
            (cc (- amount (car coin-values)) coin-values)))))

;; 2.20
(define (same-parity x . y)
  (define (same-parity? f z result)
    (if (null? z)
        result
        (if (f (car z))
            (same-parity? f (cdr z) (append result (list (car z))))
            (same-parity? f (cdr z) result))))
  (if (even? x)
      (same-parity? even? y (list x))
      (same-parity? odd? y (list x))))

;; 2.20 alt
(define (same-parity-alt x . y)
  (define (iter items result)
    (if (null? items)
        result
        (if (= (remainder x 2) (remainder (car items)  2))
            (iter (cdr items) (append result (list (car items))))
            (iter (cdr items) result))))
  (iter y (list x)))

;; Following along with reading
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;; 2.21
(define (square x) (* x x))
(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square
       items))

;; 2.22
(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items '()))

;; 2.23
(define (for-each-cust f items)
  (if (null? items)
      #t
      (and (f (car items))
           (for-each-cust f (cdr items)))))

;; 2.23 alt
(define (for-each-cust-alt f items)
  (cond ((null? items) #t)
        (else (f (car items))
              (for-each-cust-alt f (cdr items)))))
