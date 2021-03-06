(define (make-multi-vector sizes . f)
  (let ((multi-vector (make-vector (+ (apply * sizes) 1))))
    (if (not (null? f))
        (vector-fill! multi-vector (car f))
        (vector-fill! multi-vector 0)
        )
    (vector-set! multi-vector 0 (cons 'multi-vector sizes)) multi-vector))

(define (multi-vector? m)
  (and (vector? m)
       (list? (vector-ref m 0))
       (equal? (car (vector-ref m 0)) 'multi-vector)))

(define (multi-vector-ref m idxs)
  (vector-ref m (apply * (map (lambda (x) (+ x 1)) idxs))))

(define (multi-vector-set! m idxs val)
  (vector-set! m (apply * (map (lambda (x) (+ x 1)) idxs)) val))