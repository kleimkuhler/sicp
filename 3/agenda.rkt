#lang racket

(require compatibility/mlist)

(provide after-delay
         current-time
         make-agenda
         propagate)

;; Time segments
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

;; Agenda
(define (make-agenda) (mlist 0))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  #t)

(define (remove-first-agenda-item! agenda)
  #t)

(define (first-agenda-item agenda)
  #t)

(define (after-delay agenda delay action)
  (add-to-agenda! (+ delay (current-time agenda))
                  action
                  agenda))

(define (propagate agenda)
  (if (empty-agenda? agenda)
      'done
      (let ((first-item (first-agenda-item agenda)))
        (first-item)
        (remove-first-agenda-item! agenda)
        (propagate))))
