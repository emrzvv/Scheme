(define-syntax repeat
  (syntax-rules (until)
    ((repeat (body ...) until cond?)
     (let loop ()
       body ...
       (if (not cond?) (loop))))))

(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))
