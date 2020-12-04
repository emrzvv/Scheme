(load "debug.scm")

(define (a-op-b? op expr)
  (and (list? expr)
       (= (length expr) 3)
       (symbol? (car expr))
       (equal? op (car expr))
       (or (number? (cadr expr))
           (symbol? (cadr expr))
           (list? (cadr expr)))
       (or (number? (caddr expr))
           (symbol? (caddr expr))
           (list? (caddr expr)))
       ))

(define (a+b? expr) (a-op-b? '+ expr))
(define (a-b? expr) (a-op-b? '- expr))
(define (a^b? expr) (a-op-b? 'expt expr))
(define (a^2? expr) (and (a^b? expr) (equal? (caddr expr) 2)))
(define (a^3? expr) (and (a^b? expr) (equal? (caddr expr) 3)))

(define (factorize expression)
  (cond
    ((and (a-b? expression) (a^2? (cadr expression)) (a^2? (caddr expression)))
     (let ((x (cadr (cadr expression))) (y (cadr (caddr expression))))
       (list '* (list '- x y) (list '+ x y))))

    ((and (a-b? expression) (a^3? (cadr expression)) (a^3? (caddr expression)))
     (let ((x (cadr (cadr expression))) (y (cadr (caddr expression))))
       (list '* (list '- x y) (list '+ (list 'expt x 2) (list '* x y) (list 'expt y 2)))))

    ((and (a+b? expression) (a^3? (cadr expression)) (a^3? (caddr expression)))
     (let ((x (cadr (cadr expression))) (y (cadr (caddr expression))))
       (list '* (list '+ x y) (list '+ (list 'expt x 2) (list '- (list '* x y)) (list 'expt y 2)))))

    (else #f)
    ))

(define main-tests
  (list (test (factorize '(- (expt x 2) (expt y 2))) '(* (- x y) (+ x y)))
        (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2))) '(* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1))))
        (test (factorize '(+ (expt x 3) (expt y 3))) '(* (+ x y) (+ (expt x 2) (- (* x y)) (expt y 2))))
        (test (eval (list (list 'lambda 
                                '(x y) 
                                (factorize '(- (expt x 2) (expt y 2))))
                          1 2)
                    (interaction-environment)) -3)
        ))








