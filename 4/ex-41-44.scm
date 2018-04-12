;; 4.41
(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
	  seq))

(define (permutations seq)
  (if (null? seq)
      (list '())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x seq))))
	       seq)))

(define (distinct? items)
  (cond ((null? items) #t)
	((null? (cdr items)) #t)
	((member (car items) (cdr items)) #f)
	(else (distinct? (cdr items)))))

(define (md-satisfies? seq)
  (apply
   (lambda (baker cooper fletcher miller smith)
     (and
      (not (= baker 5))
      (not (= cooper 1))
      (not (= fletcher 5))
      (not (= fletcher 1))
      (> miller cooper)
      (not (= (abs (- smith fletcher)) 1))
      (not (= (abs (- fletcher cooper)) 1))
      (distinct? (list baker cooper fletcher miller smith))))
   seq))

(define (multiple-dwellings)
  (filter md-satisfies? (permutations '(1 2 3 4 5))))

;; 4.42
;; amb solution
(define (require pred)
  (if (not pred) (amb)))

(define (xor x y)
  (and (or x y) (not (and x y))))

(define (liars-amb)
  (let ((betty (amb 1 2 3 4 5))
	(ethel (amb 1 2 3 4 5))
	(joan (amb 1 2 3 4 5))
	(kitty (amb 1 2 3 4 5))
	(mary (amb 1 2 3 4 5)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list betty ethel joan kitty mary)))

;; standard solution
(define (l-satisfies? seq)
  (apply
   (lambda (betty ethel joan kitty mary)
     (and
      (xor (= kitty 2) (= betty 3))
      (xor (= ethel 1) (= joan 2))
      (xor (= joan 3) (= ethel 5))
      (xor (= kitty 2) (= mary 4))
      (xor (= mary 4) (= betty 1))
      (distinct? (list betty ethel joan kitty mary))))
   seq))

(define (liars-stand)
  (filter l-satisfies? (permutations '(1 2 3 4 5))))

;; 4.43
(define (relations)
  (let ((moore 'mary)
	(barnacle 'melissa)
	(downing (amb 'gabrielle 'lorna 'rosalind))
	(hall (amb 'gabrielle 'lorna))
	(parker (amb 'lorna 'rosalind)))
    (distinct? (list moore barnacle downing hall parker))
    (list moore barnacle downing hall parker)))

;; Reimplementing Queens solution
;; Sequence operations
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter pred seq)
  (cond ((null? seq) '())
	((pred (car seq))
	 (cons (car seq)
	       (filter pred (cdr seq))))
	(else (filter pred (cdr seq)))))

(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc (car seq)
	    (accumulate proc init (cdr seq)))))

(define (map proc seq)
  (accumulate (lambda (first rest) (cons (proc first) rest)) '() seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; Queens specific procedures
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

;; Do not need k
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

;; Do not need k
(define (safe? k positions)
  (define (next-column-safe? new-row positions offset)
    (if (null? positions)
	#t
	(let ((cur-row (car positions)))
	  (if (or (= cur-row new-row)
		  (= (+ cur-row offset) new-row)
		  (= (- cur-row offset) new-row))
	      #f
	      (next-column-safe? new-row (cdr positions) (+ offset 1))))))
  (next-column-safe? (car positions) (cdr positions) 1))
