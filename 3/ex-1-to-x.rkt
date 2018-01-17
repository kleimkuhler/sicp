#lang racket

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;; Diff: `let` is used to establish an environment with a local variable
;; `balance`
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; Diff: since a parameter is already local to a definition there is no need
;; for `let`
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;; Commented: Reworked in 3.3
;; Diff: Dispatch a procedure for handling multiple actions on an account
;; (define (make-account balance)
;;   (define (withdraw amount)
;;     (if (>= balance amount)
;;         (begin (set! balance (- balance amount))
;;                balance)
;;         "Insufficient funds"))
;;   (define (deposit amount)
;;     (set! balance (+ balance amount))
;;     balance)
;;   (define (dispatch m)
;;     (cond ((eq? m 'withdraw) withdraw)
;;           ((eq? m 'deposit) deposit)
;;           (else (error "Unknown request: MAKE-ACCOUNT"
;;                        m))))
;;   dispatch)

;; 3.1
(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))

;; 3.2
(define (make-monitored f)
  (let ((calls 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) calls)
            ((eq? m 'reset-count) (set! calls 0))
            (else
             (begin (set! calls (+ calls 1))
                    (f m)))))
    mf))

;; 3.3 & 3.4
(define (make-account password balance)
  (define consecutive-attempts 0)
  (define call-the-cops "911 what's your emergency?")
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  (define (authenticate pwd m)
    (if (eq? password pwd)
        (begin (set! consecutive-attempts 0)
               (dispatch m))
        (begin (set! consecutive-attempts (+ consecutive-attempts 1))
               (if (= consecutive-attempts 7)
                   (lambda (x) call-the-cops)
                   (lambda (x) "Incorrect password")))))
  authenticate)

;; Following along with reading

;; (define rand (let ((x random-init))
;;                (lambda ()
;;                  (set! x (rand-update x))
;;                  x)))

;; Monte Carlo method consists of choosing sample experiments at random from a
;; large set and then making deductions on the basis of the probabilities
;; estimated from tabulating the results
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

;; Estimate pi (6/pi^2)

;; (define (estimate-pi trials)
;;   (sqrt (/ 6 (monte-carlo trials cesaro-test))))
;; (define (cesaro-test)
;;   (= (gcd (rand) (rand)) 1))

;; 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (in-circle? x y)
  (<= (+ (expt (- x 5) 2)
         (expt (- y 7) 2))
      (expt 3 2)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((area (* (abs (- x1 x2)) (abs (- y1 y2))))
        (radius (/ (abs (- x1 x2)) 2))
        (experiment (lambda () (P (random-in-range x1 x2)
                                  (random-in-range y1 y2)))))
    (/ (* area (monte-carlo trials experiment)) (* radius radius))))

;; 3.6

;; (define rand
;;   (let ((x random-init))
;;     (define (dispatch message)
;;       (cond ((eq? message 'generate)
;;              (begin (set! x (rand-update x))
;;                     x))
;;             ((eq? message 'reset)
;;              (lambda (val) (set! x val)))
;;             (else (error "Unknown request: RAND"
;;                          message))))
;;     dispatch))
