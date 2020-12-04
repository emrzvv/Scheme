#|(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b) (cons a (delay b)))))

(define lazy-car car)

(define (lazy-cdr ls)
  (force (cdr ls)))

(define (lazy-ref lst n)
  (if (zero? n)
      (lazy-car lst)
      (lazy-ref (lazy-cdr lst) (- n 1))))

(define (trib-gen a b c)
  (lazy-cons a (trib-gen b c (+ a b c))))

(define (lazy-trib n)
  (lazy-ref (trib-gen 0 0 1) n))|#

(define (default-trib n)
  (cond  ((<= n 1) 0) ((= n 2) 1) 
         (else (+ (default-trib (- n 1))
                  (default-trib (- n 2))
                  (default-trib (- n 3))))))

(define memoized-trib
  (let ((memory '()))
    (lambda (n)
      (let ((memoized (assq n memory)))
        (if (not (equal? memoized #f))
            (cadr memoized)
            (let ((next-value (cond ((<= n 1) 0)
                                    ((= n 2) 1)
                                    (else (+ (memoized-trib (- n 3)) (memoized-trib (- n 2)) (memoized-trib (- n 1)))))))
              (set! memory (cons (list n next-value) memory))
              next-value))))))

(default-trib 30)
(display #\newline)
(memoized-trib 100)