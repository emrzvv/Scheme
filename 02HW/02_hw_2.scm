(define (has? x xs)
  (or (and (not (null? xs)) (= x (car xs)))
      (and (not (null? xs)) (has? x (cdr xs)))))


(define (get-set as bs)
  (if (null? as)
      bs
      (if (has? (car as) bs)
          (get-set (cdr as) bs)
          (get-set (cdr as) (cons (car as) bs))
          )))

(define (list->set xs) (get-set xs '()))

(define (set? xs) (= (length xs) (length (list->set xs))))

(set? '())     
(define (get-intersection xs ys res)
  (if (null? xs)
      res
      (if (not (has? (car xs) ys))
          (get-intersection (cdr xs) ys res)
          (get-intersection (cdr xs) ys (cons (car xs) res))
          )
      ))

(define (intersection xs ys) (get-intersection xs ys '()))


(define (get-difference xs ys res)
  (if (null? xs)
      res
      (if (has? (car xs) ys)
          (get-difference (cdr xs) ys res)
          (get-difference (cdr xs) ys (cons (car xs) res))
          )
      ))

(define (difference xs ys) (get-difference xs ys '()))


(define (union xs ys) (list->set (append xs ys)))


(define (symmetric-difference xs ys)
  (difference (union xs ys) (intersection xs ys)))


(define (set-eq? xs ys)
  (= (length xs) (length ys) (length (intersection xs ys))))