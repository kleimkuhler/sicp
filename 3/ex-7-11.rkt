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

;; 3.9
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;; Show environment structure of (factorial 6) for environment model of
;; evaluation (on page 25 notebook)

;; 3.10
;; Drawing of figure 3.10 on page 26 of notebook
;; (let) is syntactic sugar for
;;   (let ((<var> <exp>)) <body>)
;;   ((lambda (<var>) <body>) <exp>)
;; Using a `let` here would introduct an additional environment. This
;; additional environment points to E1 which points to global

;; 3.11
;; (define acc (make-account 50))
;; ((acc 'deposit) 40)
;; 90
;; ((acc 'withdraw) 60)
;; 30

;; The local state for `acc` is kept in the global environment
;; The local state for `acc2` is kept in the global environment

;; The parts of the environment structure that the two accounts share are the
;; `make-account` parameters and bodies that they point to. The body of each
;; account is not evaluated yet but I do think they would both point to the
;; same binding for internal procedures
