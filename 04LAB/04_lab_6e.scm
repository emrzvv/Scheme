(define-syntax cout
  (syntax-rules (<< endl)
    ((_ ) (begin))
    ((_ << endl . expr) (begin
                         (newline)
                         (cout . expr)))
    ((_ << exp . expr) (begin
                        (display exp)
                        (cout . expr)))))

;(cout << "a = " << 1 << endl << "b = " << 2)
(define x 1)

(define y 2)