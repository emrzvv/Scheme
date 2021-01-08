(load "debug.scm")
(load "struct.scm")

(define tokens '('EOI 'NUM 'MINUS 'PLUS 'SLASH 'UNKNOWN))
(define separators '('NEWLINE 'SPACE 'TAB))

(define-struct token-lexeme (token lexeme))
(define error-token (make-token-lexeme 'UNKNOWN 'ERROR))

(define stack '())

(define move #t)
(define no-move #f)

; для check и scan:
; <frac> :: = <signed_number>/<unsigned_number>
; <signed_number> :: = <sign> <unsigned_number>
; <unsigned_number> :: = NUM <unsigned_number> | e
; <sign> :: = PLUS | MINUS | e

; для scan-many:
; <sequence> :: = <separators> <frac> <fracs>
; <fracs> :: = <separators> <frac> <fracs> | e
; <frac> :: = <signed_number>/<unsigned_number>
; <signed_number> :: = <sign> <unsigned_number>
; <unsigned_number> :: = NUM <unsigned_number> | e
; <sign> :: = PLUS | MINUS | e
; <separators> :: = <separator> <separators> | e
; <separator> :: = NEWLINE | SPACE | TAB

(define (remove-last lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (remove-last (cdr lst)))))

(define input-string "\n-12487/384378\t1/3\n890/213\n5/-2\t")
(define save-input-string "")
(define input-index 0)

(define (increase-input-index)
  (set! input-index (+ input-index 1)))

(define (decrease-input-index)
  (set! input-index (- input-index 1)))

(define (set-input-index! i)
  (set! input-index i))

(define (set-token-index index)
  (set! input-index index))

(define (get-token-index)
  input-index)

(define (is-number c)
  (and (>= (char->integer c) 48) (<= (char->integer c) 57)))

(define (is-minus c)
  (= (char->integer c) 45))

(define (is-plus c)
  (= (char->integer c) 43))

(define (is-slash c)
  (= (char->integer c) 47))

(define (is-newline c)
  (= (char->integer c) 10))

(define (is-space c)
  (= (char->integer c) 32))

(define (is-tab c)
  (= (char->integer c) 9))

(define (get-next-token move-flag)
  (define token (make-token-lexeme 'EOI ""))
  (define current-char 0)
  (if (= (string-length input-string) input-index)
      token
      (begin
        (set! current-char (string-ref input-string input-index))
        (cond
          ((is-number current-char) (set-token-lexeme-token! token 'NUM))
          ((is-minus current-char) (set-token-lexeme-token! token 'MINUS))
          ((is-plus current-char) (set-token-lexeme-token! token 'PLUS))
          ((is-slash current-char) (set-token-lexeme-token! token 'SLASH))
          ((is-newline current-char) (set-token-lexeme-token! token 'NEWLINE))
          ((is-space current-char) (set-token-lexeme-token! token 'SPACE))
          ((is-tab current-char) (set-token-lexeme-token! token 'TAB))
          (else (if (> (string-length input-string) input-index) (set-token-lexeme-token! token 'UNKNOWN))))
        (if (not (equal? (token-lexeme-token token) 'EOI))
            (begin
              (set-token-lexeme-lexeme! token current-char)
              (if (equal? move-flag #t) (increase-input-index))
              token)))))

(define (unsigned-number token)
  (cond 
    ((equal? (token-lexeme-token token) 'NUM) (cons token (unsigned-number (get-next-token move))))
    ((equal? (token-lexeme-token token) 'SLASH) (begin (decrease-input-index) '()))
    ((or (equal? (token-lexeme-token token) 'NEWLINE)
         (equal? (token-lexeme-token token) 'SPACE)
         (equal? (token-lexeme-token token) 'TAB)) '())
    ;((equal? (token-lexeme-token token) 'EOI) '())
    (else (list token))))

(define (sign token)
  (cond ((or (equal? (token-lexeme-token token) 'MINUS)
             (equal? (token-lexeme-token token) 'PLUS)) (begin (increase-input-index) (list token)))
        ((equal? (token-lexeme-token token) 'NUM) '())
        (else (begin (increase-input-index) token))))

(define (signed-number token)
  (define temp-stack '()) ; = sign + unsugned number
  (append temp-stack (sign token) (unsigned-number (get-next-token move))))

(define (slash-frac token)
  (if (equal? (token-lexeme-token token) 'SLASH)
      (list (get-next-token move))
      (list token)))

(define (fraction token)
  ;(set! stack (append stack (list token)))
  ;(display stack)
  ;(display input-index)
  (if (or (equal? (token-lexeme-token token) 'NUM)
          (equal? (token-lexeme-token token) 'MINUS)
          (equal? (token-lexeme-token token) 'PLUS))
      (set! stack (append stack (signed-number token) (slash-frac (get-next-token no-move)) (unsigned-number (get-next-token move))))
      (set! stack (append stack (list error-token)))))

(define (parse-frac)
  (define token (make-token-lexeme 'null 'null))
  (if (>= input-index 0)
      (begin  (set! token (get-next-token no-move))
              (fraction token))
      '()
      ))

(define (clear-stack)
  (begin (set! stack '())
         (set-input-index! 0)
         #|(set! input-string "")|#))

(define (analyse-stack-after-slash s unknowns numbers)
  ;(trace-ex s)
  (define current-token (token-lexeme-token (car s)))
  (if (= (length s) 1)
      (and (= unknowns 0) (> numbers 0))
      (cond ((equal? current-token 'NUM) (analyse-stack-after-slash (cdr s) unknowns (+ numbers 1)))
            ((equal? current-token 'EOI) #t)
            (else #f))))

(define (analyse-stack s unknowns numbers slashes)
  ;(trace-ex s)
  (define current-token (token-lexeme-token (car s)))
  (if (and (equal? current-token 'SLASH)
           (= unknowns 0)
           (> numbers 0)
           (= slashes 0)
           (not (= (length s) 2)))
      (analyse-stack-after-slash (cdr s) 0 0)
      (cond ((equal? current-token 'NUM) (analyse-stack (cdr s) unknowns (+ numbers 1) slashes))
            ((equal? current-token 'UNKNOWN) #f)
            ((or (equal? current-token 'PLUS) (equal? current-token 'MINUS)) (analyse-stack (cdr s) unknowns numbers slashes))
            (else #f))))

(define (check-stack s)
  (analyse-stack s 0 0 0))

(define (get-fraction-from-stack s)
  (if (null? s)
      ""
      (cond ((equal? (token-lexeme-token (car s)) 'SLASH)
             (string-append "/" (get-fraction-from-stack (cdr s))))
            ((equal? (token-lexeme-token (car s)) 'MINUS)
             (string-append "-" (get-fraction-from-stack (cdr s))))
            ((equal? (token-lexeme-token (car s)) 'PLUS)
             (string-append "+" (get-fraction-from-stack (cdr s))))
            ((equal? (token-lexeme-token (car s)) 'EOI) "")
            (else (string-append (number->string (- (char->integer (token-lexeme-lexeme (car s))) 48)) (get-fraction-from-stack (cdr s)))))))

(define frac-list '())

(define (separators token)
  ;(trace-ex token)
  ;(trace-ex stack)
  (define check-idx input-index)
  (cond ((or (equal? (token-lexeme-token token) 'NEWLINE)
             (equal? (token-lexeme-token token) 'SPACE)
             (equal? (token-lexeme-token token) 'TAB)) (separators (get-next-token move)))
        
        ((or (equal? (token-lexeme-token token) 'MINUS)
             (equal? (token-lexeme-token token) 'PLUS)
             (equal? (token-lexeme-token token) 'NUM)) (if (< input-index 0)
                                                           #f
                                                           (begin (decrease-input-index)
                                                                  (parse-frac)
                                                                  ;(trace-ex stack)
                                                                  ;(trace-ex input-string)
                                                                  ;(trace-ex input-index)
                                                                  (if (< input-index 0)
                                                                      #f
                                                                      (begin
                                                                        (set! save-input-string (substring save-input-string (- input-index 1) (string-length save-input-string)))
                                                                        (set! frac-list (append frac-list (list (scan-frac (get-fraction-from-stack stack) #f))))
                                                                        ;(trace-ex frac-list)
                                                                        (set! stack '())
                                                                        (set! input-string save-input-string)
                                                                        (set! input-index 0)
                                                                        ;(trace-ex stack)
                                                                        ;(trace-ex input-string)
                                                                        ;(trace-ex input-index)
                                                                        ;(display (get-next-token no-move))
                                                                        (if (>= check-idx (string-length input-string))
                                                                            #t
                                                                            (separators (get-next-token no-move)))))
                                                                  )))
        ((or (equal? (token-lexeme-token token) 'EOI)) (list token))))

(define (parse-fracs)
  (set! input-index 0)
  (set! frac-list '())
  (define token (make-token-lexeme 'null 'null))
  (set! token (get-next-token no-move))
  (set! save-input-string input-string)
  (separators token))

(define (scan-many-fracs line)
  (set! input-string line)
  (parse-fracs)
  (if (or (and (not (null? frac-list)) (equal? (car (reverse frac-list)) #f))
          (null? frac-list))
      #f
      frac-list))

; --- 1 task ---
(define (check-frac line . flag)
  (clear-stack)
  (define result #f)
  (begin
    (set! input-string line)
    (parse-frac)
    (set! result (check-stack stack))
    (if (null? flag) (clear-stack))
    
    result))



; --- 2 task ---

(define (scan-frac line . c)
  (define first 0)
  (define second 0)

  (define (get-numbers fraction)
    (define is-negative #f)
    (define (loop s token lexeme number)
      ;(trace-ex s)
      ;(trace-ex token)
      ;(trace-ex lexeme)
      ;(trace-ex number)
      (if (null? s)
          0
          (cond ((equal? token 'MINUS) (begin (set! is-negative #t) (loop (cdr s)
                                                                          (token-lexeme-token (cadr s))
                                                                          (token-lexeme-lexeme (cadr s))
                                                                          number)))
                ((and (equal? token 'NUM)
                      (= (length s) 1)) (token-lexeme-lexeme (car s)))
                ((equal? token 'PLUS) (loop (cdr s)
                                            (token-lexeme-token (cadr s))
                                            (token-lexeme-lexeme (cadr s))
                                            number))
                ((equal? token 'NUM) (loop (cdr s)
                                           (token-lexeme-token (cadr s))
                                           (token-lexeme-lexeme (cadr s))
                                           (+ (- (char->integer lexeme) 48) (* number 10))))
                ((equal? token 'SLASH) (begin (if (equal? is-negative #t)
                                                  (set! first (* (- 1) number))
                                                  (set! first number))
                                              (loop (cdr s)
                                                    (token-lexeme-token (cadr s))
                                                    (token-lexeme-lexeme (cadr s))
                                                    second)))
                ((or (equal? token 'EOI)
                     (equal? token 'NEWLINE)
                     (equal? token 'SPACE)
                     (equal? token 'TAB)) (set! second number)))))
  
    (loop fraction (token-lexeme-token (car fraction)) (token-lexeme-lexeme (car fraction)) first))
  
  (if (equal? (check-frac line #f) #t)
      (begin (get-numbers stack)
             (if (null? c)
                 (clear-stack)
                 (begin (set! stack (remove-last stack))))
             ;(trace-ex first)
             ;(trace-ex second)
             (/ first second))
      #f))

(define check-frac-tests
  (list (test (check-frac "110/111") #t)
        (test (check-frac "-4/3") #t)
        (test (check-frac "+5/10") #t)
        (test (check-frac "5.0/10") #f)
        (test (check-frac "FF/10") #f)
        (test (check-frac "1/2/3") #f)
        (test (check-frac "/123") #f)
        (test (check-frac "123/") #f)
        (test (scan-frac "/") #f)))

(define scan-frac-tests
  (list
   (test (scan-frac "110/111") (/ 110 111))
   (test (scan-frac "-4/3") (/ (- 4) 3))
   (test (scan-frac "+5/10") (/ 1 2))
   (test (scan-frac "5.0/10") #f)
   (test (scan-frac "FF/10") #f)
   (test (scan-frac "1/2/3") #f)
   (test (scan-frac "123/") #f)
   (test (scan-frac "/123") #f)
   (test (scan-frac "/") #f)))

(define scan-many-fracs-tests
  (list
   (test (scan-many-fracs "\n-12487/384378\t1/3\n890/213\n5/2\t") (list (/ (- 0 12487) 384378) (/ 1 3) (/ 890 213) (/ 5 2)))
   (test (scan-many-fracs "\n-12487/384378\t1/3\n890/213\n5/-2\t") #f)
   (test (scan-many-fracs "\t1/2 1/3\n\n10/8") (list (/ 1 2) (/ 1 3) (/ 5 4)))
   (test (scan-many-fracs "\t1/2 1/3\n\n2/-5") #f)
   (test (scan-many-fracs " /123 123/890 3/3/3") #f)))

(define run-all-tests
  (begin
    (display "1. check-frac-tests:\n\n")
    (run-tests check-frac-tests)
    (clear-stack)
    (display "2. scan-frac-tests:\n\n")
    (run-tests scan-frac-tests)
    (clear-stack)
    (display "3. scan-many-fracs-tests:\n\n")
    (run-tests scan-many-fracs-tests #f)))