(define-syntax my-if
  (syntax-rules()
    ((_ pred? true false)
     (force (or (and pred? (delay true)) (delay false))))))

(my-if #t 1 (/ 1 0))
(my-if #f (/ 1 0) 1)