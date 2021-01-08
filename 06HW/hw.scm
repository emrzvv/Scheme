(define break #f)
(call-with-current-continuation (lambda (k) (set! break k)))


(define (make-source sequence . end)
  (define xs (cond ((list? sequence) sequence)
                   ((vector? sequence) (vector->list sequence))
                   ((string? sequence) (string->list sequence))))
  (define eot (if (pair? end)
                  (car end)
                  #f)) 
  (list->vector (cons 2 (cons eot xs))))

(define (peek vs)
  (if (= (vector-ref vs 0) (vector-length vs))
      (vector-ref vs 1)
      (vector-ref vs (vector-ref vs 0))))

(define (next vs)
  (if (= (vector-ref vs 0) (vector-length vs))
      (vector-ref vs 1)
      (begin
        (vector-set! vs 0 (+ 1 (vector-ref vs 0))) 
        (vector-ref vs (- (vector-ref vs 0) 1)))))

(define (space? sym) 
  (or (eq? #\tab sym)
      (eq? #\space sym)
      (eq? #\newline sym)))

(define (int? num)
  (or (eq? #\1 num)
      (eq? #\2 num)
      (eq? #\3 num)
      (eq? #\4 num)
      (eq? #\5 num)
      (eq? #\6 num)
      (eq? #\7 num)
      (eq? #\8 num)
      (eq? #\9 num)
      (eq? #\0 num)
      (eq? #\. num)))

(define (bracket? sym)
  (or (eq? #\( sym)
      (eq? #\) sym)))

(define (op? sym)
  (or (eq? #\- sym)
      (eq? #\+ sym)
      (eq? #\* sym)
      (eq? #\/ sym)
      (eq? #\^ sym)))

(define (AddOp? sym)
  (or (eq? #\- sym)
      (eq? #\+ sym)))

(define (MulOp? sym)
  (or (eq? #\/ sym)
      (eq? #\* sym )))

(define (E? sym)
  (or
   (eq? sym #\e)
   (eq? sym #\E)))

; лексический анализ

(define (tokenize str)
  (define src (make-source str))
  (define (lexer xs var num)
    (cond ((and (pair? num) (or (not (peek src)) (and
                                                  (not (int? (peek src)))
                                                  (not (E? (peek src)))
                                                  (not (and (AddOp? (peek src))
                                                            (E? (car num)))))))
           (lexer (cons (string->number (list->string (reverse num))) xs) var '()))
          ((and (pair? var) (or (not (peek src)) (not (char-alphabetic? (peek src)))))
           (lexer (cons (string->symbol (list->string (reverse var))) xs) '() num))
          ((not (peek src)) (reverse xs))
          ((and (E? (peek src))
                (pair? num))
           (lexer xs var (cons (next src) num)))
          ((and (AddOp? (peek src))
                (pair? num))
           (lexer xs var (cons (next src) num)))
          ((char-alphabetic? (peek src))
           (lexer xs (cons (next src) var) num))
          ((int? (peek src))
           (lexer xs var (cons (next src) num)))
          ((bracket? (peek src))
           (lexer (cons (string (next src)) xs) var num))
          ((op? (peek src))
           (lexer (cons (string->symbol (string (next src))) xs) var num))
          ((space? (peek src))
           (begin
             (next src)
             (lexer xs var num)))
          (else #f)))
  (lexer '() '() '()))


