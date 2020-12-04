(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b) (cons a (delay b)))))

(define lazy-car car)

(define (lazy-cdr ls)
  (force (cdr ls)))

(define (lazy-head xs k)
  (if (= k 0)
      '()
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (lazy-ref xs k)
  (if (= k 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))

(define (factorial n)
  (if (= n 0)
      1
      (* (factorial (- n 1)) n)))

(define (factorial-list n)
  (lazy-cons (factorial n) (factorial-list (+ n 1))))

(define (lazy-factorial n)
  (lazy-ref (factorial-list 0) n))