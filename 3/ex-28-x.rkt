#lang racket

(get-signal <wire>)
(set-signal! <wire> <new value>)
(add-action! <wire> <procedure of no arguments>)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1)) 1 0))
(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1 0))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (and-gate s1 s2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal s1) (get-signal s2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! s1 and-action-procedure)
  (add-action! s2 and-action-procedure)
  'ok)

;; 3.28
(define (or-gate s1 s2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal s1) (get-signal s2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
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

;; Following along with reading
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-cout)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; 3.30
