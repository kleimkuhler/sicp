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

;; 4.39
;; Order of restrictions should only matter if the restriction has a different
;; runtime complexity than others. `distinct` is the only one with O(n^2)
;; runtime so it should go last.

;; 4.40
;; Key note: most of the restrictions depend on only one or two of the
;; person-floor variables, and can thus be imposed before floors have been
;; selected for all the people.
(define (multiple-dwellings)
  (let ((cooper (amb 2 3 4 5))
	(miller (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 1 2 3 4 5)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
	(require (not (= (abs (- smith fletcher)) 1)))
	(let ((baker (amb 1 2 3 4)))
	  (distinct? (list baker cooper fletcher miller smith))
	  (list (list 'baker baker)
		(list 'cooper cooper)
		(list 'fletcher fletcher)
		(list 'miller miller)
		(list 'smith smith)))))))
