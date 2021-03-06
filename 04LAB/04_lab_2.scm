(load "debug.scm")

(define (save-data k f)
  (let ((p (open-output-file k)))
    (begin
      (write f p)
      (close-output-port p))))

(define (load-data k)
  (let ((p (open-input-file k)))
    (let f ((x (read p)))
      (if (eof-object? x)
          (begin
            (close-input-port p)
            '())
          (cons x (f (read p)))))))

;(save-data "myfile.ss" '(1 2 3 4 5))
;(apply + (car (load-data "myfile.ss")))

(define (load-str k)
  (let ((p (open-input-file k)))
    (let f ((x (read-char p)))
      (if (eof-object? x)
          (begin
            (close-input-port p)
            '())
          (cons x (f (read-char p)))))))

(define (str n xs after-not-newline)
  ;(trace-ex xs)
  (if (null? xs)
      n
      (if (and (null? (cdr xs))
               (not (or (equal? (car xs) #\return)
                        (equal? (car xs) #\newline))))
          (+ 1 n)
          (if (or (equal? (car xs) #\return) (equal? (car xs) #\newline))
              (if after-not-newline
                  (str (+ 1 n) (cdr xs) #f)
                  (str n (cdr xs) #f))
              (str n (cdr xs) #t)))))

(str 0 (load-str "myfile.ss") #f)