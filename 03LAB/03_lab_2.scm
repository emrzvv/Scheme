(load "03_lab_1.scm")

(define-syntax test
  (syntax-rules ()
    ((_ func_ans real_ans) (list 'func_ans real_ans))))

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  0) ; Ошибка здесь!
    (else     1)))

(define (run-tests tests)
  (let loop ((t tests)
             (all_ok #t))
    (if (null? t)
        all_ok
        (if (equal? (eval (car (car t)) (interaction-environment))
                    (cadr (car t)))
            (begin
              (display (car (car t)))
              (display " ok\n")
              (loop (cdr t) all_ok))
            (begin
              (display "FAIL\n")
              (display "    Expected: ")
              (display (cadr (car t)))
              (newline)
              (display "    Returned: ")
              (display (eval (car (car t)) (interaction-environment)))
              (newline)
              (loop (cdr t) #f))
            ))))

(define (iterate tests)
  (if (null? tests)
      '()
      (begin (trace-ex (eval (car (car tests)) (interaction-environment))) (iterate (cdr tests)))
      ))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))