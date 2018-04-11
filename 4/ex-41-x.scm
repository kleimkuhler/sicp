;; 4.41, 4.42, 4.43, 4.44 filled in as work happens on those

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

(define (multiple-dwellings) ())

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
