(define-syntax cout
  (syntax-rules ()
    ((cout x ...)
     (letrec
         ((rec (lambda (xs i)
                 (cond ((not (null? xs))
                        (if (even? i)
                            (cond ((equal? (car xs) 'endl) (newline))
                                  ((equal? (car xs) "\n") (newline))
                                  (else (display (car xs)))))
                        (rec (cdr xs) (+ i 1)))))))
       (rec (list 'x ... '()) 1)))))

;(cout << "a = " << 1 << endl << "b = " << 2)
(define x 1)

(define y 2)