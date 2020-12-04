(define-syntax trace-ex
  (syntax-rules ()
    ((_ subject)
     (begin
       (let* ((out subject))
         (display 'subject)
         (display " => ")
         (display out)
         (newline)
         out)))))

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss))))
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))