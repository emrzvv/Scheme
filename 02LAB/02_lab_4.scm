(define (intersperse e xs)
  (if (= 1 (length xs))
      xs
      (if (null? xs)
          '()
          (cons (car xs) (cons e (intersperse e (cdr xs))))
          )
      )
  )