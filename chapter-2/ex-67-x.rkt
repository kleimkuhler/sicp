#lang racket

;; Following along with reading
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

;; Generic procdedures that must do something slightly differently depending on
;; whether they are called with a leaf or a general tree
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons  x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; 2.67
(define sample-tree
               (make-code-tree (make-leaf 'A 4)
                               (make-code-tree
                                (make-leaf 'B 2)
                                (make-code-tree
                                 (make-leaf 'D 1)
                                 (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; Decodes to: (A D A B B C A)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol-alt (car message) tree)
              (encode (cdr message) tree))))

;; 2.68
(define (encode-symbol symbol tree)
  (define (encode-symbol-1 symbol node result)
    (cond ((leaf? node) result)
          ((memq symbol (left-branch node))
           (encode-symbol-1 symbol (left-branch node) (append result (list '0))))
          (else
           (encode-symbol-1 symbol (right-branch node) (append result (list '1))))))
  (if (memq symbol (symbols tree))
      (encode-symbol-1 symbol tree '())
      (error "bad symbol: ENCODE-SYMBOL" symbol)))

;; 2.68 alt
(define (encode-symbol-alt symbol tree)
  (define (encode-symbol-alt-1 current-branch result)
    (cond ((and (leaf? current-branch) (eq? (symbol-leaf current-branch) symbol))
           (reverse result))
          ((memq symbol (symbols (left-branch  current-branch)))
           (encode-symbol-alt-1 (left-branch current-branch) (cons 0 result)))
          ((memq symbol (symbols (right-branch current-branch)))
           (encode-symbol-alt-1 (right-branch current-branch) (cons 1 result)))
          (else
           (error "bad symbol: ENCODE-SYMBOL" symbol))))
  (encode-symbol-alt-1 tree '()))

;; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs)
                                                    (cadr pairs))
                                    (cddr pairs)))))

;; 2.70
(define song
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1)
                                 (JOB 2) (NA 16) (YIP 9))))

;; (length (encode '(GET A JOB
;;                       SHA NA NA NA NA NA NA NA NA
;;                       GET A JOB
;;                       SHA NA NA NA NA NA NA NA NA
;;                       WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
;;                       SHA BOOM)
;;                 song))
;; The above yields 84 bits
;; Fixed length would be 3 * 36 (words) = 108 bits

;; 2.71
;; n = 5 tree => 1 bit for most frequent & 4 bit for most infrequent
;; For a 2^n-1 frequency, you will need n-1 bits for most infrequent symbol

;; 2.72
;; Page 14
;; Most frequent = O(n) because you must search the list of symbols once
;; - O(1) retrieval
;; Least frequent = O(nn) because you must search the list of symbols once
;; per node with at most n-1 searches.
;; - O(1) retrieval

;; Following along with reading
(define (square x) (* x x))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))


(define (real-part-rec z) (car z))
(define (imag-part-rec z) (cdr z))
(define (magnitude-rec z)
  (sqrt (+ (square (real-part-rec z))
           (square (imag-part-rec z)))))
(define (angle-rec z)
  (atan (imag-part-rec z)
        (real-part-rec z)))
(define (make-from-real-imag-rec x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rec r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-pol z)
  (* (magnitude-pol z) (cos (angle-pol z))))
(define (imag-part-pol z)
  (* (magnitude-pol z) (sin (angle-pol z))))
(define (magnitude-pol z) (car z))
(define (angle-pol z) (cdr z))
(define (make-from-real-imag-pol x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-pol r a)
  (attach-tag 'polar (cons r a)))

;; (define (real-part z)
;;   (cond ((rectangular? z)
;;          (real-part-rec (contents z)))
;;         ((polar? z)
;;          (real-part-pol (contents z)))
;;         (else (error "Unknown type: REAL-PART" z))))
;; (define (imag-part z)
;;   (cond ((rectangular? z)
;;          (imag-part-rec (contents z)))
;;         ((polar? z)
;;          (imag-part-pol (contents z)))
;;         (else (error "Unknown type: IMAG-PART" z))))
;; (define (magnitude z)
;;   (cond ((rectangular? z)
;;          (magnitude-rec (contents z)))
;;         ((polar? z)
;;          (magnitude-pol (contents z)))
;;         (else (error "Unknown type: MAGNITUDE" z))))
;; (define (angle z)
;;   (cond ((rectangular? z)
;;          (angle-rec (contents z)))
;;         ((polar? z)
;;          (angle-pol (contents z)))
;;         (else (error "Unknown type: ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rec x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-pol r a))

;; Data directed programming is the technique of designing programs to work
;; with a table directly
;; (put <op> <type> <item>)
;; (get <op> <type>)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  ;; (put 'real-part '(rectangular) real-part)
  ;; (put 'imag-part '(rectangular) imag-part)
  ;; (put 'magnitude '(rectangular) magnitude)
  ;; (put 'angle '(rectangular) angle)
  ;; (put 'make-from-real-imag 'rectangular
  ;;      (lambda (x y) (tag (make-from-real-imag x y))))
  ;; (put 'make-from-mag-ang 'rectangular
  ;;      (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; same for polar package

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (error "No method for these types: APPLY-GENERIC"
;;                  (list op type-tags))))))

;; (define (real-part z) (apply-generic 'real-part z))
;; (define (imag-part z) (apply-generic 'imag-part z))
;; (define (magnitude z) (apply-generic 'magnitude z))
;; (define (angle z) (apply-generic 'angle z))

;; 2.73
;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum (make-product
;;                     (multiplier exp)
;;                     (deriv (multiplicand exp) var))
;;                    (make-product
;;                     (deriv (multiplier exp) var)
;;                     (multiplicand exp))))
;;         <more rules added here>
;;         (else (error "unkown expression type: DERIV" exp))))

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         (else ((get 'deriv (operator exp))
;;                (operands exp) var))))
;; (define (operator exp) (car exp))
;; (define (operands exp) (cdr exp))

;; a).
;;   Our table tag-type is based off the operator. The predicates number? and
;;   same-variable? do not have operators, therefore there is no way to look-up
;;   a proc in the table

;; b).
;; (define (install-deriv-sum)
;;   ;; internal procedues
;;   (define (deriv exp var)
;;     (make-sum (deriv (addend exp) var)
;;               (deriv (augend exp) var)))

;;   ;; interface to the rest of the system
;;   (define (tag x) (attach-tag 'sum x))
;;   (put 'deriv '(sum) deriv)
;;   'done

;; (define (install-deriv-product)
;;   ;; internal procedures
;;   (define (deriv exp var)
;;     (make-sum (make-product
;;                (multiplier exp)
;;                (deriv (multiplicand exp) var))
;;               (make-product
;;                (deriv (multiplier exp) var)
;;                (multiplicand exp))))

;;   ;; interface to the rest of the system
;;   (define (tag x) (attach-tag 'product x))
;;   (put 'deriv '(product) deriv)
;;   'done)

;; 2.74
;; Paper exercises page 15

;; Following along with reading
;; (define (make-from-real-imag x y)
;;   (define (dispatch op)
;;     (cond ((eq? op 'real-part) x)
;;           ((eq? op 'imag-part) y)
;;           ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
;;           ((eq? op 'angle) (atan y x))
;;           (else (error "Uknown op: MAKE-FROM-REAL-IMAG" op))))
;;   dispatch)

;; Discuss
;; 2.75 & 2.76
;; (define (make-from-mag-ang r a)
;;   (define (dispatch op)
;;     (cond ((eq? op 'real-part) (* r (cos a)))
;;           ((eq? op 'imag-part) (* r (sin a)))
;;           ((eq? op 'magngitude) r)
;;           ((eq? op 'angle) a)
;;           (else (error "Unkown op: MAKE-FROM-MAG-ANG" op)))))
