(load "debug.scm")

(define (def vec ind)
  (if (equal? (vector-ref vec ind) 'end)
      (+ ind 1)
      (def vec (+ ind 1))))

(define (iff vec ind s)
  (if (equal? s 0)
      ind
      (if (equal? (vector-ref vec ind) 'if)
          (iff vec (+ 1 ind) (- s 1))
          (if (equal? (vector-ref vec ind) 'endif)
              (iff vec (+ 1 ind) (- s 1))
              (iff vec (+ 1 ind) s)))))

(define (interpret vec stack)
  (define (point ind stack return dictionary)
    (if (= ind (vector-length vec))
        stack
        (let ((elem (vector-ref vec ind)))
          (cond
            ((number? elem) (point (+ 1 ind) (cons elem stack) return dictionary))
            ((equal? elem 'drop) (point (+ 1 ind) (cdr stack) return dictionary))
            ((equal? elem 'swap) (point (+ 1 ind) (cons (cadr stack) (cons (car stack) (cddr stack))) return dictionary))
            ((equal? elem 'dup) (point (+ 1 ind) (cons (car stack) stack) return dictionary))
            ((equal? elem 'over) (point (+ 1 ind) (cons (cadr stack) stack) return dictionary))
            ((equal? elem 'rot) (point (+ 1 ind) (append (list (caddr stack) (cadr stack)) (cdddr stack)) return dictionary))
            ((equal? elem 'depth) (point (+ 1 ind) (cons (length stack) stack) return dictionary))
            ((equal? elem '=) (point (+ 1 ind) (cons (if (= (cadr stack) (car stack)) -1 0) (cddr stack)) return dictionary))
            ((equal? elem '>) (point (+ 1 ind) (cons (if (> (cadr stack) (car stack)) -1 0) (cddr stack)) return dictionary))
            ((equal? elem '<) (point (+ 1 ind) (cons (if (< (cadr stack) (car stack)) -1 0) (cddr stack)) return dictionary))
            ((equal? elem '+) (point (+ 1 ind) (cons (+ (cadr stack) (car stack)) (cddr stack)) return dictionary))
            ((equal? elem '-) (point (+ 1 ind) (cons (- (cadr stack) (car stack)) (cddr stack)) return dictionary))
            ((equal? elem '*) (point (+ 1 ind) (cons (* (cadr stack) (car stack)) (cddr stack)) return dictionary))
            ((equal? elem '/) (point (+ 1 ind) (cons (/ (cadr stack) (car stack)) (cddr stack)) return dictionary))
            ((equal? elem 'mod) (point (+ 1 ind) (cons (remainder (cadr stack) (car stack)) (cddr stack)) return dictionary))
            ((equal? elem 'neg) (point (+ 1 ind) (cons (- (car stack)) (cdr stack)) return dictionary))
            ((equal? elem 'and) (point (+ 1 ind) (cons (if (and (= (car stack) -1) (= (cadr stack) -1)) -1 0)) return dictionary))
            ((equal? elem 'or) (point (+ 1 ind) (cons (if (or (= (car stack) -1) (= (cadr stack) -1)) -1 0)) return dictionary))
            ((equal? elem 'define) (point (def vec ind) stack return (cons (list (vector-ref vec (+ ind 1)) (+ ind 2)) dictionary)))
            ((or (equal? elem 'end) (equal? elem 'exit)) (point (car return) stack (cdr return) dictionary))
            ((equal? elem 'if) (point (if (not (= (car stack) 0)) (+ ind 1) (iff vec (+ ind 1) 1)) (cdr stack) return dictionary))
            ((equal? elem 'endif) (point (+ ind 1) stack return dictionary))
            (else (point (cadr (assoc elem dictionary)) stack (cons (+ 1 ind) return) dictionary))))))
  (point 0 stack '() '()))

(define the-tests
  (list (test (interpret #(   define abs 
                               dup 0 < 
                               if neg endif 
                               end 
                               9 abs 
                               -9 abs      ) (quote ()))
              '(9 9))
        (test (interpret #(   define =0? dup 0 = end
                               define <0? dup 0 < end
                               define signum
                               =0? if exit endif
                               <0? if drop -1 exit endif
                               drop
                               1
                               end
                               0 signum
                               -5 signum
                               10 signum       ) (quote ()))
              '(1 -1 0))
        (test (interpret #(   define -- 1 - end
                               define =0? dup 0 = end
                               define =1? dup 1 = end
                               define factorial
                               =0? if drop 1 exit endif
                               =1? if drop 1 exit endif
                               dup --
                               factorial
                               *
                               end
                               0 factorial
                               1 factorial
                               2 factorial
                               3 factorial
                               4 factorial     ) (quote ()))
              '(24 6 2 1 1))
        (test (interpret #(   define =0? dup 0 = end
                               define =1? dup 1 = end
                               define -- 1 - end
                               define fib
                               =0? if drop 0 exit endif
                               =1? if drop 1 exit endif
                               -- dup
                               -- fib
                               swap fib
                               +
                               end
                               define make-fib
                               dup 0 < if drop exit endif
                               dup fib
                               swap --
                               make-fib
                               end
                               10 make-fib     ) (quote ()))
              '(0 1 1 2 3 5 8 13 21 34 55))
        (test (interpret #(   define =0? dup 0 = end
                               define gcd
                               =0? if drop exit endif
                               swap over mod
                               gcd
                               end
                               90 99 gcd
                               234 8100 gcd    ) '())
              '(18 9))))

