#lang racket

;; 3.7
(define (make-account pwd balance)
  ;; Make new bank account
  (let ((pwds '(pwd)))
    (define (withdraw amount)
      ;; Withdraw `amount` from account balance
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      ;; Deposit `amount` into account balance
      (set! balance (+ balance amount))
      balance)
    (define (make-joint add-pwd)
      ;; Add additional password to account
      (set! pwds (cons add-pwd pwds)))
    (define (dispatch message)
      ;; Dispatch account procedure for message
      (cond ((eq? message 'withdraw) withdraw)
            ((eq? message 'deposit) deposit)
            ((eq? message 'make-joint) make-joint)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         message))))
    (define (authenticate secret message)
      ;; Dispatch authentication before account procedure
      (if (member secret pwds)
          (dispatch message)
          (lambda (x) "Incorrect password")))
    authenticate))

;; Fails late instead of early
(define (make-joint account pwd add-pwd)
  ;; Make a joint account with parent account `account` and additional password
  ((account pwd 'make-joint) add-pwd)
  account)

;; 3.8
(define f1
  (let ((called #f))
    (define (dispatch x)
      (if (not called)
          (begin (set! called #f)
                 x)
          0))
    dispatch))

(define f2
  (let ((called #f))
    (lambda (x) (if (not called)
                    (begin (set! called #t)
                           x)
                    0))))
