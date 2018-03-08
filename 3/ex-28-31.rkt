#lang racket

(require "agenda.rkt")
(require "logical-elements.rkt")
(require "wire.rkt")

;; Module defined values
(define the-agenda (make-agenda))

;; Half-adder circuit
(define (half-adder a b s c agenda)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d agenda)
    (and-gate a b c agenda)
    (inverter c e agenda)
    (and-gate d e s agenda)
    'ok))

;; Full-adder circuit
(define (full-adder a b c-in sum c-out agenda)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1 agenda)
    (half-adder a s sum c2 agenda)
    (or-gate c1 c2 c-out agenda)
    'ok))

;; 3.30
;; Ripple-carry-adder circuit
(define (ripple-carry-adder a b s c agenda)
  (let ((c-in (make-wire)))
    (if (null? (cdr a))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in agenda))
    (full-adder (car a) (car b) c-in (car s) c agenda)))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; 3.31
;; An action-procedure is added to a wire via the `after-delay` procedure.
;; `accept-action-procedure` needs to run the new procedure right away because
;; running it is what actually adds the action to the-agenda. The agenda's
;; `propagate` is responsible for actually executing the action.
