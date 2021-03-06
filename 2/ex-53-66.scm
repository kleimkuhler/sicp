;; 2.53
;; Notepad exercises

;; 2.54
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
      (eq? a b)))

;; 2.55
;; Not exactly an exercise but thought this was interesting
(define (ex-2.55) (car ''hello))

;; Following along with reading
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? a) (symbol? a))
(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (make-sum-prefix a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list '+ a b))))
(define (sum-prefix? a)
  (and (pair? a) (eq? (car a) '+)))
(define (addend-prefix a) (cadr a))

(define (make-product-prefix a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b) (* a b)))
        (else (list '* a b))))
(define (product-prefix? a)
  (and (pair? a) (eq? (car a) '*)))
(define (multiplier-prefix a) (cadr a))

;; 2.56
(define (make-exponentiation a b)
  (cond ((=number? b 0) 1)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (expt a b))
        (else (list '** a b))))
(define (exponentiation? a)
  (and (pair? a) (eq? (car a) '**)))
(define (base a) (cadr a))
(define (exponent a) (caddr a))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (display "Error: unknown expression type -- DERIV" exp))))

;; 2.57
(define (augend-prefix a)
  (if (null? (cdddr a))
      (caddr a)
      (cons '+ (cddr a))))

(define (multiplicand-prefix a)
  (if (null? (cdddr a))
      (caddr a)
      (cons '* (cddr a))))

;; 2.58
(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list a '+ b))))
(define (sum? a)
  (and (pair? a) (eq? (cadr a) '+)))
(define (addend a) (car a))
(define (augend a) (caddr a))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b) (* a b)))
        (else (list a '* b))))
(define (product? a)
  (and (pair? a) (eq? (cadr a) '*)))
(define (multiplier a) (car a))
(define (multiplicand a) (caddr a))

;; Following along with reading
(define (element-of-set-unordered? a set)
  (cond ((null? set) #f)
        ((equal? a (car set)) #t)
        (else (element-of-set-unordered? a (cdr set)))))

(define (adjoin-set a set)
  (if (element-of-set-unordered? a set)
      set
      (cons a set)))

(define (intersection-set a b)
  (cond ((or (null? a) (null? b)) '())
        ((element-of-set-unordered? (car a) b) (cons (car a) (intersection-set (cdr a) b)))
        (else (intersection-set (cdr a) b))))

;; 2.59
(define (union-set a b)
  (if (null? a)
      b
      (union-set (cdr a) (adjoin-set (car a) b))))

;; alt that I like!
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (union-set-acc a b)
  (accumulate adjoin-set b a))

;; 2.60
;; element-of-set-unordered? remains
;; intersection-set can remain. Duplicates show when given in
;; first set. Technically the sets are equal whether there are
;; duplicates or not, so this shouldn't matter
(define (adjoin-set-dup a set) (cons a set))

(define (union-set-dup a b) (append a b))

;; Following along with reading
(define (element-of-set-ordered? a set)
  (cond ((null? set) #f)
        ((equal? a (car set)) #t)
        ((< a (car set)) #f)
        (else (element-of-set-ordered? a (cdr set)))))

(define (intersection-set-ordered a b)
  (if (or (null? a) (null? b))
      '()
      (let ((c (car a)) (d (car b)))
        (cond ((= c d)
               (cons c (intersection-set-ordered (cdr a)
                                                 (cdr b))))
              ((< c d) (intersection-set-ordered (cdr a) b))
              ((> c d) (intersection-set-ordered a (cdr b)))))))
;; 2.61
;; Implement with element-of-set-ordered?
;; I really really don't like this but this is what first came to mind D:
(define (adjoin-set-ordered-eos a set)
  (define (insert a initial set)
    (if (< a (car set))
        (append (append initial (list a)) set)
        (insert a (append initial (list (car set))) (cdr set))))
  (if (element-of-set-ordered? a set)
      set
      (insert a '() set)))

(define (adjoin-set-ordered a set)
  (cond ((null? set) (list a))
        ((= a (car set)) set)
        ((< a (car set)) (cons a set))
        (else (cons (car set) (adjoin-set-ordered a (cdr set))))))

;; 2.62
;; This is not theta(n)
(define (union-set-ordered-acc a b)
  (accumulate adjoin-set-ordered b a))

;; Realize this is a merge from mergesort and you get theta(n)
;; time
(define (union-set-ordered a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((= (car a) (car b))
         (cons (car a) (union-set-ordered (cdr a) (cdr b))))
        ((< (car a) (car b))
         (cons (car a) (union-set-ordered (cdr a) b)))
        (else
         (cons (car b) (union-set-ordered a (cdr b))))))

;; Following along with reading
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree-element-of-set? a set)
  (cond ((null? set) #f)
        ((= a (entry set)) #t)
        ((< a (entry set))
         (tree-element-of-set? a (left-branch set)))
        (else
         (tree-element-of-set? a (right-branch set)))))

(define (tree-adjoin-set a set)
  (cond ((null? set) (make-tree a '() '()))
        ((= a (entry set)) set)
        ((< a (entry set))
         (make-tree (entry set)
                    (adjoin-set a (left-branch set))
                    (right-branch set)))
        (else
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set a (right-branch set))))))

;; 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;; Balanced tree (same answer)
;; (define tree-1 ('(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))))
;; Unbalanced tree (same answer)
;; (define tree-2 '(4 (2 (1 () ()) (3 () ())) (7 (6 (5 () ()) ()) (8 () ()))))

;; List 1 2 3 from figure 2.16
;; (define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;; (define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
;; (define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

;; Master Theorem
;; tree->list-1: 2T(n/2) + O(n/2) -> O(nlogn)
;; tree->list-2: 2T(n/2) + O(1) -> O(n)

;; Following along with reading
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))


;; 2.64
;; (partial-tree '(1 3 5 7 9 11) 6)
;;   (partial-tree '(1 3 5 7 9 11) 2)
;;     (partial-tree '(1 3 5 7 9 11) 0) -> cons ('() '(1 3 5 7 9 11))
;;   -> left-tree = '(); non-left-elts = '(1 3 5 7 9 11); right-size = 3
;;     -> this-entry = 1; right-result = (partial-tree '(3 5 7 9 11) 3)
;;       -> (cons (make-tree '(1) '() '(3)) '(5 7 9 11))
;; The final line of this is correct. In the tree constructed, the left branch
;; should be '(1 () (3 () ()))
;; Same runtime as above implementation 2 -> O(n)

;; 2.65
;; union-set and interesection-set of lists are O(n). Because we can create an
;; ordered list with 2.64 in O(n), we can use that to then implement the
;; existing union-set and intersection-set
(define (union-set-bb-tree a b)
  (list->tree (union-set-ordered
               (tree->list-2 a)
               (tree->list-2 b))))
(define (intersection-set-bb-tree a b)
  (list->tree (intersection-set-ordered
               (tree->list-2 a)
               (tree->list-2 b))))
