(define (make-serializer)
  (let ([mutex (make-mutex)])
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ([val (apply p args)])
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ([cell (list #f)])
    (define (mutex m)
      (cond [(eq? m 'acquire)
             (if (test-and-set! cell)
                 (mutex 'acquire))]
            [(eq? m 'release) (clear! cell)]))
    mutex))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

(define (clear! cell)
  (set-car! cell #f))

;; 3.47
(define (make-semaphore n)
  (let ([access-mutex (make-mutex)]
        [procs 0])
    (define (semaphore m)
      (cond [(eq? m 'acquire)
             (access-mutex 'acquire)
             (if (< procs n)
                 (begin (set! procs (add1 procs))
                        (access-mutex 'release))
                 (begin (access-mutex 'release)
                        (semaphore 'acquire)))]
            [(eq? m 'release)
             (access-mutex 'acquire)
             (set! procs (sub1 procs))
             (access-mutex 'release)]))
    semaphore))
