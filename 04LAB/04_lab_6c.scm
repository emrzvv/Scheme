(define-syntax while
  (syntax-rules ()
    ((_ cond? body ...) (let loop ()
                         (if cond?
                             (begin
                               body ...
                               (loop)))))))

(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))
