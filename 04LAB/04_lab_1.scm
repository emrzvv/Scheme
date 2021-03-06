(load "debug.scm")

(define point #f)

(define (use-assertions)
  (call-with-current-continuation (lambda (x) (set! point x))))

(use-assertions)

(define-syntax assert
  (syntax-rules ()
    ((_ ps) (let ((p? ps))
              (if (not p?)
                  (begin
                    (display "FAILED: ")
                    (display 'ps)
                    (newline)
                    (point))
                  p?)))))

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))
