(define-syntax when
  (syntax-rules ()
    ((_ cond x . xs) (if cond (begin x . xs)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond x . xs) (if (not cond) (begin x . xs)))))

(define x 1)
(when   (> x 0) (display "x > 0")  (newline))
(unless (= x 0) (display "x != 0") (newline))
