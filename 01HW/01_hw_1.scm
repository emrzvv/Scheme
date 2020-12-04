(define (calc_normal day month year)
  (remainder (+ day (quotient (* 31 (- month 2)) 12) year (quotient year 4) (- (quotient year 100)) (quotient year 400)) 7)
  )

(define (calc_front day month year)
  (remainder (+ day (quotient (* 31 (+ month 10)) 12) (- year 1) (quotient (- year 1) 4) (- (quotient (- year 1) 100)) (quotient (- year 1) 400)) 7)
  )

(define (day-of-week day month year)
  (if (< month 3)
      (calc_front day month year)
      (calc_normal day month year)
      )
  ;(day + (31 * month) / 12 + year + year / 4 - year / 100 + year / 400) % 7 ;
  )