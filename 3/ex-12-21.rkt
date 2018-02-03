#lang racket

(require compatibility/mlist)

;; 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (last-mpair x)
  (if (null? (mcdr x)) x (last-mpair (mcdr x))))

(define (append! x y)
  ;; Equivalent to mappend!.
  (set-mcdr! (last-mpair x) y)
  x)

;; 3.13
(define (make-cycle x)
  (set-mcdr! (last-mpair x) x)
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
(define (count-mpairs-16 x)
  (if (not (mpair? x))
      0
      (+ (count-mpairs-16 (mcar x))
         (count-mpairs-16 (mcdr x))
         1)))

;; (count-mpairs-16 three-count)
;; 3
(define three-count (mlist 'a 'b 'c))

;; (count-mpairs-16 four-count)
;; 4
(define four-count (mlist 'a (mlist 'b 'c)))

;; (count-mpairs-16 seven-count)
;; 7
(define seven-count (mcons three-count three-count))

;; (count-mpairs-16 infinite-count)
(define infinite-count (mlist 'a 'b))
(set-mcdr! (last-mpair infinite-count) infinite-count)

;; 3.17
(define (count-mpairs-17 x)
  ;; Count pairs in x and handle cycles.
  (let ((seen '()))
    (define (iter x)
      (if (or (not (mpair? x)) (memq x seen))
          0
          (begin
            (set! seen (cons x seen))
            (+ (iter (mcar x))
               (iter (mcdr x))
               1))))
    (iter x)))

;; 3.18
(define (cycle?-18 x)
  ;; Return true if x is a cyle.
  (let ((seen '()))
    (define (iter x)
      (cond ((not (mpair? x)) #f)
            ((memq x seen) #t)
            (else
             (begin
               (set! seen (cons x seen))
               (iter (mcdr x))))))
    (iter x)))

;; 3.19
(define (cycle?-19 x)
  ;; Return true if x is a cycle using constant space based off Floyd's
  ;; Tortoise and Hare cycle-finding algorithm.
  (define (advance x count)
    (cond ((= count 0) x)
          ((not (mpair? x)) x)
          (else (advance (mcdr x) (- count 1)))))
  (define (iter x y)
    (cond ((not (mpair? y)) #f)
          ((eq? x y) #t)
          (else
           (iter (advance x 1)
                 (advance y 2)))))
  (iter x (mcdr x)))

;; Following along with reading
(define (mcons-cust x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-mcar-cust) set-x!)
          ((eq? m 'set-mcdr-cust) set-y!)
          (else (error "Undefined operation: CONS" m))))
  dispatch)
(define (mcar-cust z) (z 'car))
(define (mcdr-cust z) (z 'cdr))
(define (set-mcar-cust z new-value)
  ((z 'set-mcar-cust) new-value))
(define (set-mcdr-cust z new-value)
  ((z 'set-mcdr-cust) new-value))

;; A queue needs the following:
;; Constructor
;; Selectors:
;; * empty-queue?
;; * front-queue
;; Mutators:
;; * insert-queue!
;; * delete-queue!
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (make-queue) (mcons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (not (empty-queue? queue))
      (mcar (front-ptr queue))
      (error ("FRONT-PTR called with an empty queue" queue))))

(define (insert-queue! queue item)
  (let ((pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue pair)
           (set-rear-ptr! queue pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) pair)
           (set-rear-ptr! queue pair)
           queue))))
(define (delete-queue! queue)
  (cond ((not (empty-queue? queue))
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)
        (else
         (error ("DELETE-QUEUE! called with an empty queue" queue)))))

;; 3.21
(define (print-queue queue)
  (define (iter queue result)
    (if (null? queue)
        (mreverse result)
        (iter (mcdr queue) (mcons (mcar queue) result))))
  (iter (front-ptr queue) '()))
