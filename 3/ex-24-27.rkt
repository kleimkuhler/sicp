#lang racket

(require compatibility/mlist)

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

;; one-dimensional table
(define (lookup-1 key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        #f)))
(define (insert!-1 key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                   (mcons (mcons key value)
                          (mcdr table)))))
  'ok)
(define (make-table-1)
  (mlist '*table*))

;; local two-dimensional table
(define (make-table-2 same-key?)
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record (mcdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcons (mcdr subtable))))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value))
                              (mcdr local-table)))))
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;; 3.24
;; Added an additional parameter to `assoc` to check key equality

;; 3.25
(define (make-table same-key?)
  (let ((table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup keys)
      (define (iter subtable keys)
        (cond ((null? keys) subtable)
              ((not (mpair? subtable)) #f)
              (else
               (let ((new-subtable (assoc (car keys) subtable)))
                 (if new-subtable
                     (iter (mcdr new-subtable) (cdr keys))
                     #f)))))
      (iter (mcdr table) keys))
    (define (insert! keys value)
      (define (iter subtable keys value)
        (cond ((null? keys) (set-mcdr! subtable value))
              ((not (mpair? (mcdr subtable)))
               (set-mcdr! subtable (mlist (mcons (car keys) '())))
               (iter (mcar (mcdr subtable)) (cdr keys) value))
              (else
               (let ((new-subtable (assoc (car keys) (mcdr subtable))))
                 (if new-subtable
                     (iter new-subtable (cdr keys) value)
                     (begin
                       (set-mcdr! subtable
                                  (mcons (mlist (car keys)) (mcdr subtable)))
                       (iter (mcar (mcdr subtable)) (cdr keys) value)))))))
      (iter table keys value))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;; 3.26
;; Make use of adjoin-set from binary tree exercises
;; (define (adjoin-set x set)
;;   (cond ((null? set) (make-tree x '() '()))
;;         ((= x (entry set)) set)
;;         ((< x (entry set))
;;          (make-tree (entry set)
;;                     (adjoin-set x (left-branch set))
;;                     (right-branch set)))
;;         ((> x (entry set))
;;          (make-tree (entry set)
;;                     (left-branch set)
;;                     (adjoin-set x (right-branch set))))))

;; What is interesting about the above is a new tree is generated each time
;; a new x is adjoined to the set. This is a different approach then the last
;; few sections which have introduced mutability.

;; (define (adjoin-set x set)
;;   (cond ((null? set) (make-tree x '() '()))
;;         ((= x (entry set)) set)
;;         ((< x (entry set))
;;          (let ((left (left-branch set)))
;;            (set! left (adjoin-set x left))))
;;         (else
;;          (let ((right (right-branch set)))
;;            (set! right (adjoin-set x right))))))

;; (define (make-tree-table)
;;   (let ((table (list '*table*)))
;;     (define (lookup keys)
;;       (define (iter keys subtable)
;;         (if (null? keys)
;;             subtable
;;             (let ((new-subtable (assoc (car keys) subtable)))
;;               (if new-subtable
;;                   (iter (cdr keys) new-subtable)
;;                   #f))))
;;       (iter keys (cdr table)))
;;     (define (insert! keys value)
;;       (define (iter keys subtable)
;;         (if (null? (cdr keys))
;;             (adjoin-set (car keys) value subtable)
;;             (let ((new-subtable (adjoin-set (car keys) '() subtable)))
;;               (iter (cdr keys) new-subtable))))
;;       (iter keys table))
;;     (define (dispatch m)
;;       (cond ((eq? m 'lookup-proc) lookup)
;;             ((eq? m 'insert-proc!) insert!)
;;             (error "Unknown operation: TREE-TABLE" m)))
;;     dispatch))

;; 3.27
(define (lookup x table)
  ((table 'lookup-proc) x))
(define (insert! x value table)
  ((table 'insert-proc!) x value))

(define (memoize f)
  (let ((table (make-table equal?)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))
