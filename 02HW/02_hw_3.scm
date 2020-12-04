(define (del-forward-spaces line)
  (if (char-whitespace? (car line))
      (del-forward-spaces (cdr line))
      line
      ))

(define (del-backward-spaces line)
  (reverse (del-forward-spaces (reverse line))))

(define (string-trim-left line)
  (list->string (del-forward-spaces (string->list line))))

(define (string-trim-right line)
  (list->string (del-backward-spaces (string->list line))))

(define (string-trim line)
  (string-trim-right (string-trim-left line)))


(define (check-prefix a b)
  (or (and (null? a) (null? b))
      (and (null? a) (not (null? b)))
      (and (not (null? b)) (equal? (car a) (car b)) (check-prefix (cdr a) (cdr b)))))

(define (string-prefix? a b)
  (check-prefix (string->list a) (string->list b)))

(define (string-suffix? a b)
  (check-prefix (reverse (string->list a)) (reverse (string->list b))))


(define (check-infix a b)
  (or (null? a)
      (and (not (null? b))
           (equal? (car a) (car b))
           (check-infix (cdr a) (cdr b))
           )
      (and (not (null? b))
           (check-infix a (cdr b))
           )
      ))

(define (string-infix? a b) (check-infix (string->list a) (string->list b)))


(define (string-split x y) (str (split (string->list x) (string->list y) (length (string->list y) ))))

(define (str l) ; list to string
  (if (null? l)
      '()
      (cons (list->string (car l)) (str (cdr l)))))

(define (string-make x y z) ; returns string before first sep : (string-make '(#\a #\b #\- #\- #\> #\y #\- #\- #\> #\z) '(#\- #\- #\>) 3) --> (#\a #\b)
  (if (null? x)
      '()
      (if (null? (cdr x))
          x
          (if (string-prefix? (list->string y) (list->string x))
              '()
              (cons (car x) (string-make (cdr x) y z))))))

(define (split line sep lensep)
  (if (null? line)
      '()
      (if (null? (cdr line))
          (list line)
          (if (string-prefix? (list->string sep) (list->string line))
              (split (list-tail line lensep) sep lensep)
              (cons (string-make line sep lensep) (split (list-tail line (length (string-make line sep lensep))) sep lensep))))))
