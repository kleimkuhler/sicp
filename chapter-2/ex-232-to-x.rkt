#lang racket

;; Following along with reading
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; 2.33
(define (map-cust p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append-cust seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-cust sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;; 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   t)))

;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; op should be commutative for accumulate and fold-left to have
;; the same result for the same sequence

;; 2.39
(define (reverse-acc sequence)
  (accumulate (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (append (list y)  x)) '() sequence))

(define (reverse-left-alt sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
