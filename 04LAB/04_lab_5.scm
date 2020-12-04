(define-syntax my-let
  (syntax-rules ()
    ((my-let () body ...)
     ((lambda () body ...)))
    
    ((my-let ((var val) ...) body ...)
     ((lambda (var ...) first . last) val ...))
    
    ((my-let name ((var val) ...) body ...)
     ((lambda ()
        (define name (lambda (var ...) body ...))
        (name val ...))))))


(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () body ...)
     ((lambda () body ...)))
    
    ((my-let* ((var val) rest ...) body ...)
     ((lambda (var) (my-let* (rest ...) body ...)) val))
    ))

(my-let ((x 2) (y 3))
        (* x y)) ;6

(my-let ((x 2) (y 3))
        (my-let ((x 7)
                 (z (+ x y)))
                (* z x))) ;35

(my-let ((x 2) (y 3))
        (my-let* ((x 7)
                  (z (+ x y)))
                 (* z x))) ;70

(my-let*
 ((x 1)
  (y (* x 2))
  (z (* y 3)))
 (+ x y z)) ;9

