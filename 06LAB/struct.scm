(load "debug.scm")

(define i-env interaction-environment)

(define (reset ls length)
  (if (< 0 length)
      (cons (car ls) (reset (cdr ls) (- length 1)))
      '()))

(define-syntax define-struct
  (syntax-rules ()
    ((_ input-struct input-fields)
     (let ((struct 'input-struct)
           (fields 'input-fields))
       ; constructor
       ;(trace-ex `(define (,(string->symbol (string-append "make-" (symbol->string struct))) ,@fields) (list ',struct ,@fields)))
       (eval `(define (,(string->symbol (string-append "make-" (symbol->string struct))) ,@fields) (list ',struct ,@fields)) (i-env))

       ; predicate
       ;(trace-ex `(define (,(string->symbol (string-append (symbol->string struct) "?")) p) (and (pair? p) (equal? (car p) ',struct))))
       (eval `(define (,(string->symbol (string-append (symbol->string struct) "?")) p) (and (pair? p) (equal? (car p) ',struct))) (i-env))

       (define i 1)

       ; struct_name-field_name var_name
       (define (define-get field)
         ;(trace-ex `(define ( ,(string->symbol (string-append (symbol->string struct) "-" (symbol->string field))) p) (list-ref p ,i)))
         (eval `(define ( ,(string->symbol (string-append (symbol->string struct) "-" (symbol->string field))) p) (list-ref p ,i)) (i-env))
         (set! i (+ i 1)))

       (define j 1)

       ;
       (define (define-set field)
         #|(trace-ex `(define-syntax ,(string->symbol (string-append "set-" (symbol->string struct) "-" (symbol->string field) "!"))
                      (syntax-rules ()
                        ((_ var val)
                         (set! var (append (reset var ,j) (list val) (list-tail var (+ ,j 1))))
                         ))))|#
         (eval `(define-syntax ,(string->symbol (string-append "set-" (symbol->string struct) "-" (symbol->string field) "!"))
                  (syntax-rules ()
                    ((_ var val)
                     (set! var (append (reset var ,j) (list val) (list-tail var (+ ,j 1))))
                     )))
               (i-env))
         (set! j (+ j 1)))

       
       (for-each define-get fields)
       (for-each define-set fields)
       ))))

#|(define-struct pos (row col)) ; Объявление типа pos
(define p (make-pos 1 2))     ; Создание значения типа pos

(pos? p)

(pos-row p)
(pos-col p)

(set-pos-row! p 3) ; Изменение значения в поле row
(set-pos-col! p 4) ; Изменение значения в поле col

(pos-row p)
(pos-col p)|#
