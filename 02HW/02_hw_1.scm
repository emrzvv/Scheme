(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))
      ))

(define (my-flatten mxs)
  (if (null? mxs)
      '()
      (if (list? (car mxs))
          (append (my-flatten (car mxs)) (my-flatten (cdr mxs)))
          (cons (car mxs) (my-flatten (cdr mxs)))
          )
      )
  )

(define (my-element? x xs)
  (or (and (not (null? xs)) (= x (car xs)))
      (and (not (null? xs)) (my-element? x (cdr xs)))))

(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs))
          )
      ))

(define (move_right op x xs)
  (if (null? xs)
      x
      (move_right op (op x (car xs)) (cdr xs))
      ))

(define (my-fold-left op xs) (move_right op (car xs) (cdr xs)))

(define (my-fold-right op xs) (move_right op (car (reverse xs)) (cdr (reverse xs))))