#lang racket

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? exp) (symbol? exp))
  (define (same-variable? exp1 exp2)
    (and (variable? exp1) (variable exp2) (eq? exp1 exp2)))
  ;; representation of term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coef term))
        term-list
        (cons term term-list)))
  (define (the-empty-term) 0)
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr terml-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coef term) (cadr term))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1) (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1) (rest-terms L2)))))))))
  ;; 2.88
  (define (sub-terms L1 L2)
    (cond ((and (empty-termlist? L1) (empty-termlist? L2)) '())
          ((empty-termlist? L1)
           (let ((t2 (first-term L2)))
             (adjoin-term
              (make-term (order t2) (sub 0 (coeff t2)))
              (sub-terms L1 (rest-terms L2)))))
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     (make-term (order t2) (sub 0 (coeff t2)))
                     (sub-terms L1 (rest-terms L2)))
                   (else
                    (adjoin-term
                     (make-term (order t1) (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1) (rest-terms L2))))))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  ;; 2.88
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p2) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: SUB-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest L))))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  ;; 2.87
  (put '=zero? 'polynomial
       (lambda (p) (null? (term-list p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
