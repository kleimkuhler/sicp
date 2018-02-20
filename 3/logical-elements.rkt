#lang racket

(require "wire.rkt")
(require "agenda.rkt")

;; Provide operators
(provide logical-and
         logical-not
         logical-or)

;; Provide gates
(provide and-gate
         inverter
         or-gate)

;; Delays
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;; Operators
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1)) 1 0))

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1 0))

;; Gates
(define (inverter input output agenda)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value))
                   agenda)))
  (add-action! input invert-input) 'ok)

(define (and-gate s1 s2 output agenda)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal s1) (get-signal s2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value))
                   agenda)))
  (add-action! s1 and-action-procedure)
  (add-action! s2 and-action-procedure)
  'ok)

;; 3.28
(define (or-gate s1 s2 output agenda)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal s1) (get-signal s2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value))
                   agenda)))
  (add-action! s1 or-action-procedure)
  (add-action! s2 or-action-procedure))

;; 3.29
;; A or B => not (not (A or B))
;;        => not ((not A) and (not B))
;; Delay is inverter + and-gate + inverter
(define (nand-or-gate s1 s2 output)
  (let ((c1 (make-wire)) (c2 (make-wire)) (c3 (make-wire)))
    (inverter s1 c1)
    (inverter s2 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output)))
