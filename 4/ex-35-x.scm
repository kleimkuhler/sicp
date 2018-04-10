(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
	 (j (an-integer-between i high))
	 (k (an-integer-between j high)))
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))

;; 4.35
(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ low 1) high)))

;; 4.36
(define (all-pythagorean-triples)
  ())

;; 4.37
;; Ben's solution has a smaller search space
