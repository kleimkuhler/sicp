#lang racket

;; Following along with reading
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval lo hi)
  (if (> lo hi)
      '()
      (cons lo (enumerate-interval (+ lo 1) hi))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

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

;; 2.35 (alt with enumerate tree)
(define (count-leaves-alt t)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

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

;; On further thought, this is the better solution to
;; reverse-left since I am building from right to left
(define (reverse-left-alt sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;; Following along with reading
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; Elementary implementation of prime? for example
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ n 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Continuing reading
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs-old n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (remove item s)
  (filter (lambda (x) (not (= x item)))
          s))

;; Why '() when if > '() instead of (list '())
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (y) (cons x y))
                      (permutations (remove x s))))
               s)))

;; 2.40
(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list x y))
                  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; 2.41
;; Originally had car, cadr and caddr
(define (make-triple triple)
  (append triple (list (accumulate + 0 triple))))

(define (unique-ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (ordered-triple-sums n s)
  (define (triple-sum? triple)
    (= s (accumulate + 0 triple)))
  (map make-triple
       (filter triple-sum? (unique-ordered-triples n))))

;; 2.42
(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

;; safe v1
(define (safe-v1? k positions)
  (define (safe-row?)
    (null? (filter
            (lambda (pos)
              (= (caar positions) (car pos)))
            (cdr positions))))
  (define (safe-diag?)
    (null? (filter
            (lambda (pos)
              (= (abs (- (caar positions) (car pos)))
                 (abs (- (cdar positions) (cdr pos)))))
            (cdr positions))))
  (and (safe-row?) (safe-diag?)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe-v1? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
