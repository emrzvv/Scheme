#|(define-syntax for
  (syntax-rules (in as)
    ((_ x in xs . f)
     (map (lambda (x) . f) xs))

    ((_ xs as x . f)
     (map (lambda (x) . f) xs))))|#

(define-syntax for
  (syntax-rules (in as)
    ((_ x in xs . f)
     (for-each (lambda (x) . f) xs))

    ((_ xs as x . f)
     (for-each (lambda (x) . f) xs))))


#|(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))|#