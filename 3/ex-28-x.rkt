#lang racket

(require "agenda.rkt")
(require "logical-elements.rkt")
(require "wire.rkt")

;; Module defined values
(define the-agenda (make-agenda))

;; Half-adder circuit
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;; Full-adder circuit
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; 3.30
;; Ripple-carry-adder circuit
(define (ripple-carry-adder a b s c)
  (let ((c-in (make-wire)))
    (if (null? (cdr a))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
    (full-adder (car a) (car b) c-in (car s) c)))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
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
