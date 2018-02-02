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

;; op should be associative for accumulate and fold-left to have
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

;; 2.44
;; Commented out because beside/below is not defined
;; (define (up-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (up-split painter (- n 1))))
;;         (below painter (beside smaller smaller)))))

;; 2.45
;; Comment out because beside/below is not defined
;; (define (right-split (split beside below)))
;; (define (up-split (split below beside)))
;;
;; (define (split large small)
;;   (lambda (painter n)
;;     (if (= n 0)
;;         painter
;;         (let ((smaller ((split large small) painter (- n 1))))
;;           (large painter (small smaller smaller)))))

;; 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (cons (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (cons (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s)
  (cons (* s (xcor-vect v))
        (* s (ycor-vect v))))

;; 2.47
(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (origin-frame-c frame)
  (car frame))
(define (edge1-frame-c frame)
  (cadr frame))
(define (edge2-frame-c frame)
  (cddr frame))

;; Following along with reading
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; 2.48
(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;; 2.49
(define (outline->painter frame)
  (let ((tl (add-vect (origin-frame frame)
                      (edge1-frame frame)))
        (tr (add-vect (origin-frame frame)
                      (add-vect (edge1-frame frame)
                                (edge2-frame frame))))
        (br (add-vect (origin-frame frame)
                      (edge2-frame frame)))
        (bl (origin-frame frame)))
    (segments->painter
     (list
      (make-segment tl tr)
      (make-segment tr br)
      (make-segment br bl)
      (make-segment bl tl)))))

(define (x->painter frame)
  (let ((tl (add-vect (origin-frame frame)
                      (edge1-frame frame)))
        (tr (add-vect (origin-frame frame)
                      (add-vect (edge1-frame frame)
                                (edge2-frame frame))))
        (br (add-vect (origin-frame frame)
                      (edge2-frame frame)))
        (bl (origin-frame frame)))
    (segments->painter
     (list
      (make-segment tl br)
      (make-segment tr bl)))))

(define (diamond->painter frame)
  (let ((tl (add-vect (origin-frame frame)
                      (scale-vect (edge1-frame frame) .5)))
        (tr (add-vect (origin-frame frame)
                      (add-vect (edge1-frame frame)
                                (scale-vect (edge2-frame frame) .5))))
        (br (add-vect (origin-frame frame)
                      (add-vect (edge2-frame frame)
                                (scale-vect (edge1-frame frame) .5))))
        (bl (add-vect (origin-frame frame)
                      (scale-vect (edge2-frame frame) .5))))
    (segments->painter
     (list
      (make-segment tl tr)
      (make-segment tr br)
      (make-segment br bl)
      (make-segment bl tl)))))

;; 2.50 - 2.52
;; I solved these out on paper but the actual implementations
;; felt like busy work since it really was not that complicated.
;; There appears to be a packet that has the actual draw-line
;; function would could be helpful but for working through this
;; book to learn concepts, this did not feel like time well spent.
