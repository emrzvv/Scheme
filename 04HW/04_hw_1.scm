(define (memoized-factorial n)
  (let ((memory '()))
    (let ((saved (assq n memory)))
      (if saved
          (cadr saved)
          (let ((val (if (<= n 1) 1 (* (memoized-factorial (- n 1)) n))))
            (set! memory (cons (list n val) memory))
            val)))))
