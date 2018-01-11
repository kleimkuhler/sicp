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

;; 3.3
(define (make-account password balance)
  (define (check-password pwd) (eq? password pwd))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)
