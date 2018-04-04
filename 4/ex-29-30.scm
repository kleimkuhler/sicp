;; 4.29
;; a.
;; the example program would run more slowly without memoization

;; b.
;; with memoization
;; > count
;; 1
;; without memoization
;; > count
;; 2

;; 4.30
;; a.
;; `display` is a primitive function so each call in for-each will evaluate the
;; thunk `x`

;; b.
;; Original
;; (p1 1) => (1 . 2)
;; (p2 1) => 1 (set! x (cons x '(2)) is delayed)
;; Cy
;; (p1 1) => (1 . 2)
;; (p2 1) => (1 . 2)

;; c. & d.
;; Prefer original style because of the assumption that "the value of an
;; expression in a sequence other than the last one is not used".

