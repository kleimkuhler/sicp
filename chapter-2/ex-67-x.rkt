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

