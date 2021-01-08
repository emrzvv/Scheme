(define (flatten xs)
  (let loop ((xs xs) (acc '()))
    (cond
     ((null? xs) acc)
     ((pair? xs) (loop (car xs) (loop (cdr xs) acc)))
     (else (cons xs acc)))))
