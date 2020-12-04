(load "debug.scm")

(define i-env interaction-environment)

(define (define-type type constructors)
  (define (define-constructor constructor)
    (eval `(define ,constructor (list ',(car constructor) '(cdr constructor))) (i-env)))
  
  (define (define-predicate)
    (eval `(define (,(string->symbol (string-append (symbol->string type) "?")) p) (and
                                                                                    (pair? p)
                                                                                    (> (length p) 0)
                                                                                    (assoc (car p) ',constructors) #t)) (i-env)))

  (define-predicate)
  (for-each define-constructor constructors))

(define-syntax match
  (syntax-rules ()
    ((_ f) #f)
    ((_ f ((type args ...) expr) exprs ...)
     (cond
       ((equal? (car f) 'type) (eval (cons '(lambda (args ...) expr) (cdr f)) (i-env)))
       (else (match f exprs ...))))))

(define-syntax define-data
  (syntax-rules ()
    ((_ type constructors) (define-type `type `constructors))))

(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

(and (figure? s)
     (figure? r)
     (figure? t)
     (figure? c))

(define pi (acos -1)) 
  
(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))
  
(perim s)
(perim r)
(perim t)