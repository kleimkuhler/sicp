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
