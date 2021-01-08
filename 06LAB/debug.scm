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

(define-syntax test
  (syntax-rules ()
    ((_ func_ans real_ans) (list 'func_ans real_ans))))

(define (run-tests tests . args)
  (let loop ((t tests)
             (all_ok #t))
    (if (null? t)
        all_ok
        (if (equal? (eval (car (car t)) (interaction-environment))
                    (cadr (car t)))
            (begin
              (if (null? args)
                  (display (car (car t))))
              (display " [OK]\n")
              (loop (cdr t) all_ok))
            (begin
              (display (car (car t)))
              (display " [FAIL]\n")
              (display "    Expected: ")
              (display (cadr (car t)))
              (newline)
              (display "    Returned: ")
              (display (eval (car (car t)) (interaction-environment)))
              (newline)
              (loop (cdr t) #f))
            ))))