(define ans '())

(define as '(1 2 3 4 5 6 7 8 9 10))

(define (my_delete pred? ls)
  (if (null? ls)
      '()
      (if (pred? (car ls))
          (my_delete pred? (cdr ls))
          (cons (car ls) (my_delete pred? (cdr ls)))
          )
      )
  )