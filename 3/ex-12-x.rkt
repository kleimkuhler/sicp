#lang racket

(require compatibility/mlist)

;; 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (last-pair! x)
  (if (null? (mcdr x)) x (last-pair! (mcdr x))))

(define (append! x y)
  ;; Equivalent to mappend!
  (set-mcdr! (last-pair! x) y)
  x)

;; 3.13
(define (make-cycle x)
  (set-mcdr! (last-pair! x) x)
  x)

;; 3.14
(define (mreverse x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

;; 3.16
(define (count-pairs! x)
  (if (not (mpair? x))
      0
      (+ (count-pairs! (mcar x))
         (count-pairs! (mcdr x))
         1)))

;; (count-pairs! three-count)
;; 3
(define three-count (mlist 'a 'b 'c))

;; (count-pairs! four-count)
;; 4
(define four-count (mlist 'a (mlist 'b 'c)))

;; (count-pairs! sevent-count)
;; 7
(define seven-count (mcons three-count three-count))
