(define t_1 '(1 2 3 4 5 5 4 3 2 1))
(define t_2 '(a b c d a e f g z z z a))

(define (count x xs)
  (if (null? xs)
      0
      (if (eq? (car xs) x)
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs))
          )
      )
  )