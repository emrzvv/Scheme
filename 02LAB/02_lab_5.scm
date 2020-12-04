(define (any? pred? xs)
  (or (and (not (null? xs)) (pred? (car xs)))
      (and (not (null? xs)) (any? pred? (cdr xs)))))

(define (all? pred? xs)
  (and (or (null? xs) (pred? (car xs)))
       (or (null? xs) (all? pred? (cdr xs)))))