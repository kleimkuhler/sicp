;; 4.32
;; Stream of streams, trees, etc.

;; 4.33
(define (make-list lst)
  (if (null? lst)
      '()
      (list 'cons
	    (list 'quote (car lst))
	    (mamke-list (cdr lst)))))

(define (text-of-quotation-lazy exp)
  (let ((text (cadr exp)))
    (if (pair? text)
	(eval (make-list text) env)
	text)))

;; 4.34
;; Lot of time when I'd like to continue reading. Page 35 of notebook for
;; Meetup
