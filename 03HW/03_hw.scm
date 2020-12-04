(load "debug.scm")

(define (my-flatten mxs)
  (if (null? mxs)
      '()
      (if (list? (car mxs))
          (append (my-flatten (car mxs)) (my-flatten (cdr mxs)))
          (cons (car mxs) (my-flatten (cdr mxs)))
          )
      )
  )

(define (set-if-x mxs new_x)
  (if (list? mxs)
      (if (equal? 'x (car mxs))
          (set-car! mxs new_x)
          #f)
      (if (equal? 'x mxs)
          (set! mxs new_x)
          #f)))

(define (my-flatten-change mxs new_x)
  (if (list? mxs)
      (if (null? mxs)
          mxs
          (if (list? (car mxs))
              (cons (my-flatten-change (car mxs) new_x) (my-flatten-change (cdr mxs) new_x))
              (begin (set-if-x mxs new_x) (cons (car mxs) (my-flatten-change (cdr mxs) new_x)))
              ))
      (begin (set-if-x mxs new_x) mxs)))

(define (has-x? expr)
  (if (null? expr)
      #f
      (or (equal? 'x (car expr)) (has-x? (cdr expr)))))

(define (has-zero? expr)
  (if (null? expr)
      #f
      (or (equal? 0 (car expr)) (equal? '0 (car expr)) (equal? '(0) (car expr)) (has-zero? (cdr expr)))))

(define (variable? e)
  (symbol? e))

(define (neg-variable? e)
  (equal? e '(- x)))

(define (neg-expr? e)
  (equal? (car e) '-))

(define (num-power? e)
  (and (equal? (car e) 'expt) (number? (cadr e))))

(define (x-power? e)
  (and (equal? (car e) 'expt) (equal? (cadr e) 'x)))

(define (sum? e)
  (equal? (car e) '+))

(define (sub? e)
  (and (> (length e) 2) (equal? (car e) '-)))

(define (mult? e)
  (equal? (car e) '*))

(define (div? e)
  (equal? (car e) '/))

(define (expt? e)
  (equal? (car e) 'expt))

(define (exp? e)
  (equal? (car e) 'exp))

(define (log? e)
  (equal? (car e) 'log))

(define (sin? e)
  (equal? (car e) 'sin))

(define (cos? e)
  (equal? (car e) 'cos))

(define (tg? e)
  (equal? (car e) 'tg))

(define (ctg? e)
  (equal? (car e) 'ctg))

(define (make-sum a b)
  (cond
    ((equal? a 0) b)
    (else (list '+ a b))
    ))

(define (make-sub a b)
  (list '- a b))

(define (make-negative e)
  (list '- e))

(define (make-mult a b)
  (cond
    ((equal? a 1) b)
    ((equal? b 1) a)
    ((equal? a 0) 0)
    (else (list '* a b)))
  )

(define (make-div a b)
  (list '/ a b))

(define (make-expt a b)
  (list 'expt a b))

(define (make-exp e)
  (list 'exp e))

(define (make-log e)
  (list 'log e))

(define (make-cos x)
  (list 'cos x))

(define (make-sin x)
  (list 'sin x))

(define (derivative expr)
  ;(trace-ex expr)
  (cond
    ((number? expr) 0)
    ((variable? expr) 1)
    ((sum? expr) (make-sum (derivative (cadr expr)) (derivative (caddr expr))))
    ((sub? expr) (make-sub (derivative (cadr expr)) (derivative (caddr expr))))
    ((mult? expr) (if (= (length expr) 3)
                      (make-sum (make-mult (derivative (cadr expr)) (caddr expr)) (make-mult (cadr expr) (derivative (caddr expr))))
                      (list (derivative (make-mult (make-mult cadr caddr) cadddr)))
                      ))
    ((div? expr) (make-div (make-sub (make-mult (derivative (cadr expr)) (caddr expr))
                                     (make-mult (cadr expr) (derivative (caddr expr))))
                           (make-expt (caddr expr) 2)))
    ((neg-variable? expr) (- 1))
    ((neg-expr? expr) (make-negative (derivative (cadr expr))))
    ((exp? expr) (make-mult expr (derivative (cadr expr)))) 
    ((expt? expr) (cond
                    ((has-x? (my-flatten (list (cadr expr)))) (make-mult (caddr expr)
                                                                         (make-mult (make-expt (cadr expr) (make-sub (caddr expr) 1))
                                                                                    (derivative (cadr expr))))) ; x в основании
                    ((has-x? (my-flatten (list (caddr expr)))) (make-mult expr
                                                                          (make-mult (make-log (cadr expr)) (derivative (caddr expr))))) ; x в степени
                    ))
    ((log? expr) (make-mult (make-div 1 (cadr expr)) (derivative (cadr expr))))
    ((sin? expr) (make-mult (make-cos (cadr expr)) (derivative (cadr expr))))
    ((cos? expr) (make-negative (make-mult (make-sin (cadr expr)) (derivative (cadr expr)))))
    ((tg? expr) (make-mult (make-div 1 (make-expt (make-cos x) 2)) (derivative (cadr expr))))
    ((ctg? expr) (make-negative (make-mult (make-div 1 (make-expt (make-sin x) 2)) (derivative (cadr expr)))))
    
    (else (display "undefined\n"))
    ))

(define (simplify-mult expr)
  (if (null? expr)
      '()
      (cond
        ((list? (car expr)) (cons (simplify-mult (car expr)) (simplify-mult (cdr expr))))
        ((equal? '* (car expr)) (if (has-zero? (simplify-mult (cdr expr)))
                                    0
                                    (cons '* (simplify-mult (cdr expr)))))
        ((equal? 1 (car expr)) (simplify-mult (cdr expr)))
        ((equal? 0 (car expr)) '(0))
        (else (cons (car expr) (simplify-mult (cdr expr))))
      )))

(define (simplify-sum expr)
  (if (null? expr)
      '()
      (cond
        ((list? (car expr)) (cons (simplify-sum (car expr)) (simplify-sum (cdr expr))))
        ((equal? '+ (car expr)) (cons '+ (simplify-sum (cdr expr))))
        ((equal? '- (car expr)) (cons '- (simplify-sum (cdr expr))))
        ((equal? 0 (car expr)) (simplify-sum (cdr expr)))
        (else (cons (car expr) (simplify-sum (cdr expr))))
        )))

(define (simplify expr)
  (simplify-mult (simplify-sum expr)))

(define tests ;каждый тест считает значение производной в точке x0 = 5
  (list (test ((lambda (x) (eval (my-flatten-change (derivative '(* (- 4) x)) x)(interaction-environment))) 5) -4)
        (test ((lambda (x) (eval (my-flatten-change (derivative 2) x)(interaction-environment))) 5) 0)
        (test ((lambda (x) (eval (my-flatten-change (derivative 'x) x)(interaction-environment))) 5) 1)
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* 1 x)) x)(interaction-environment))) 5) 1)
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* (- 1) x)) x)(interaction-environment))) 5) -1)
        (test ((lambda (x) (eval (my-flatten-change (derivative '(- (* 2 x) 3)) x)(interaction-environment))) 5) 2)
        (test ((lambda (x) (eval (my-flatten-change (derivative '(expt x 10)) x)(interaction-environment))) 5) 19531250)
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* 2 (expt x 5))) x)(interaction-environment))) 5) 6250)
        (test ((lambda (x) (eval (my-flatten-change (derivative '(expt x (- 2))) x)(interaction-environment))) 5) (/ (- 2) 125))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(expt 5 x)) x)(interaction-environment))) 5) (* (expt 5 5) (log 5)))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(cos x)) x)(interaction-environment))) 5) (- (sin 5)))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(sin x)) x)(interaction-environment))) 5) (cos 5))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(exp x)) x)(interaction-environment))) 5) (exp 5))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* 2 (exp x))) x)(interaction-environment))) 5) (* 2 (exp 5)))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* 2 (exp (* 2 x)))) x)(interaction-environment))) 5) (* 4 (exp (* 2 5))))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(log x)) x)(interaction-environment))) 5) (/ 1 5))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* 3 (log x))) x)(interaction-environment))) 5) (/ 3 5))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(+ (expt x 3) (expt x 2))) x)(interaction-environment))) 5) 85)
        (test ((lambda (x) (eval (my-flatten-change (derivative '(- (* 2 (expt x 3)) (* 2 (expt x 2)))) x)(interaction-environment))) 5) 130)
        (test ((lambda (x) (eval (my-flatten-change (derivative '(/ 3 x)) x)(interaction-environment))) 5) (/ (- 3) 25))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(/ 3 (* 2 (expt x 2)))) x)(interaction-environment))) 5) (/ (- 3) 125))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* (* 2 (sin x)) (cos x))) x)(interaction-environment))) 5) (* 2 (cos 10)))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* (* 2 (exp x)) (* (sin x) (cos x)))) x)(interaction-environment))) 5) (* (exp 5) (+ (sin 10) (* 2 (cos 10)))))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(sin (* 2 x))) x)(interaction-environment))) 5) (* 2 (cos 10)))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(cos (* 2 (expt x 2)))) x)(interaction-environment))) 5) (* (- 20) (sin 50)))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(sin (log (expt x 2)))) x)(interaction-environment))) 5) (* (/ 2 5) (cos (log 25))))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(+ (sin (* 2 x)) (cos (* 2 (expt x 2))))) x)(interaction-environment))) 5) (* 2 (- (cos 10) (* 10 (sin 50)))))
        (test ((lambda (x) (eval (my-flatten-change (derivative '(* (sin (* 2 x)) (cos (* 2 (expt x 2))))) x)(interaction-environment))) 5) (- (* 11 (cos 60)) (* 9 (cos 40))))
        ))

