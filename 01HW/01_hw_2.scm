(define (sqr x) (* x x))

(define (count_D a b c)
  (- (sqr b) (* 4 a c))
  )

(define (only_root a b) (/ (- b) (* 2 a)))

(define (first_root a b c)
  (/ ( - (- b) (sqrt (count_D a b c))) (* 2 a))
  )

(define (second_root a b c)
  (/ ( + (- b) (sqrt (count_D a b c))) (* 2 a))
  )

(define (get_roots a b c)
  (cond ((and (= 0 a)
              (= 0 b)
              (= 0 c))
         (list 0 0)) ;inf amount of roots
        ((= a 0)
         (list (/ (- c) b)))
        (else
         (if (= 0 (count_D a b c))
             (list (only_root a b))
             (if (< 0 (count_D a b c))
                 (list (first_root a b c) (second_root a b c))
                 (list))
             )
         )
        )
  )