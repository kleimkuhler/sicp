#lang racket

(define (install-polynomial-dense-package)
  ;; term-list implementation independent internal procedures
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? exp) (symbol? exp))
  (define (same-variable? exp1 exp2)
    (and (variable? exp1) (variable exp2) (eq? exp1 exp2)))
  (define (empty-termlist) '())
  (define (rest-terms term-list) (cdr terml-list))
  (define (empty-termlist? term-list) (null? term-list))
  p(define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coef term) (cadr term))
  
  ;; term-list implementation dependent internal procedures
  (define (adjoin-term term term-list)
    (if (= (order term) (length term-list))
        (cons (coeff term) term-list)
        (adjoin-term term
                     (cons 0 term-list))))
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))

  ;; internal helper procedures
  (define (negate-terms term-list)
    (map (lambda (t)
           (make-term (order t)
                      (negate (coeff t))))
         term-list))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1
                                 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2
                                 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term (make-term (order t1)
                                            (add (coeff t1) (coeff t2)))
                                 (add-terms (rest-terms L1)
                                            (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (empty-termlist) (empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((result-of-result
                       (div-terms (add-terms L1
                                             (negate-terms
                                              (mul-terms (make-term new-o new-c)
                                                         L2)))
                                  L2)))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

  ;; internal procedures
  (define (make-poly variable term-list) (cons variable term-list))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (div-terms (term-list p1)
                                 (term-list p2))))
          (list (make-poly (variable p1)
                           (car result))
                (make-poly (variable p2)
                           (cadr result))))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'dense p))
  (put '=zero? 'dense
       (lambda (p) (null? (term-list p))))
  (put 'make 'dense
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'negate 'dense
       (lambda (p) (tag (negate p))))
  (put 'add '(dense dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(sparse sparse)
       (lambda (p1 p2) (tag (add-poly p1
                                      (make-poly (variable p2)
                                                 (negate-terms (term-list p2)))))))
  (put 'mul '(dense dense)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(dense dense)
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  'done)

(define (install-polynomial-sparse-package)
  ;; term-list implementation independent internal procedures
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? exp) (symbol? exp))
  (define (same-variable? exp1 exp2)
    (and (variable? exp1) (variable exp2) (eq? exp1 exp2)))
  (define (empty-termlist) '())
  (define (rest-terms term-list) (cdr terml-list))
  (define (empty-termlist? term-list) (null? term-list))
  p(define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coef term) (cadr term))

  ;; term-list implementation dependent internal helper procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (car term-list))

  ;; internal helper procedures
  (define (negate-terms term-list)
    (map (lambda (t)
           (make-term (order t)
                      (negate (coeff t))))
         term-list))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1
                                 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2
                                 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term (make-term (order t1)
                                            (add (coeff t1) (coeff t2)))
                                 (add-terms (rest-terms L1)
                                            (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (empty-termlist) (empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((result-of-result
                       (div-terms (add-terms L1
                                             (negate-terms
                                              (mul-terms (make-term new-o new-c)
                                                         L2)))
                                  L2)))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

  ;; internal procedures
  (define (make-poly variable term-list) (cons variable term-list))
  (define (negate-poly p)
    (make-poly (variable p)
               (map (lambda (t)
                      (make-term (order t)
                                 (negate (coeff t))))
                    (term-list p))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (div-terms (term-list p1)
                                 (term-list p2))))
          (list (make-poly (variable p1)
                           (car result))
                (make-poly (variable p2)
                           (cadr result))))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put '=zero? 'sparse
       (lambda (p) (null? (term-list p))))
  (put 'make 'sparse
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'negate 'sparse
       (lambda (p) (tag (negate p))))
  (put 'add '(sparse sparse)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(sparse sparse)
       (lambda (p1 p2) (tag (add-poly p1
                                      (make-poly (variable p2)
                                                 (negate-terms (term-list p2)))))))
  (put 'mul '(sparse sparse)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(spare sparse)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  'done)

(define (sparse-poly? p)
  ;; If half the length of terms is less than the length of terms without
  ;; zeroes, then sparse poly is optimal
  (if (< (/ (length p) 2) (length (filter (not zero?) p)) 2))
      #t
      #f))

(define (make-polynomial var terms)
  (if (sparse-poly? terms)
      ((get 'make 'sparse) var terms)
      ((get 'make 'dense) var terms)))

;; Note on the above:
;; The solution follows the conventions outlined in 2.4. This case of
;; representing polynomials in two different forms is equivalent to
;; representing complex numbers in two different forms.
;;
;; The difference in this situation is that there are procedures that are the
;; same, but rely on internal procedures that are implemented differently
;; between the two. This causes a duplication in code such as: (add-poly),
;; (mul-poly), (add-terms), (mul-terms), etc.
;;
;; A solution I see in this is to create a new table internal to the polynomial
;; package that stores the procedures specific to the different forms, but
;; allows a sharing of procedures that do not. An internal (apply-generic)
;; could then select only from that table instead of the parent
;; operation-and-type table.

;; 2.92
;; To impose an ordering here, a solution could be to implement the tower
;; method that was discussed and treat x as 1 and all other variables 0.
