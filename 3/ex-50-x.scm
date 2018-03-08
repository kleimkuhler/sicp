(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; 3.50
(define (my-stream-map proc . args)
  (if (stream-null? args)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car args))
       (apply stream-map
	      (cons proc (map stream-cdr args))))))

;; 3.51
;; (define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0
;; ; Value: x

;; (stream-ref x 5)
;; 12345
;; ; Value: 5

;; (stream-ref x 7)
;; 67
;; ; Value: 7
