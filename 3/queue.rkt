#lang racket

(provide delete-queue!
         empty-queue?
         front-queue
         insert-queue!
         make-queue
         print-queue)

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (not (empty-queue?))
          (mcar front-ptr)
          (error ("FRONT-QUEUE: Called with empty queue" front-ptr))))
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))
        front-ptr))
    (define (delete-queue!)
      (cond ((not (empty-queue?))
             (set! front-ptr (mcdr front-ptr))
             front-ptr)
            (else
             (error ("DELETE-QUEUE: Called with empty queue" front-ptr)))))
    (define (print-queue)
      (define (iter queue result)
        (if (null? queue)
            (reverse result)
            (iter (mcdr queue) (cons (mcar queue) result))))
      (iter front-ptr '()))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error ("MAKE-QUEUE: Unknown request" m)))))
    dispatch))

(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (front-queue queue) ((queue 'front-queue)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))
(define (print-queue queue) ((queue 'print-queue)))
