(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . fs)
  (define (loop x fs)
    (if (null? fs)
        x
        (loop ((car fs) x) (cdr fs))))
  (lambda (x) (loop x (reverse fs))))
