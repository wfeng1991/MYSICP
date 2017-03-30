#lang racket
(define (double n) (+ n n))
(define (halve n) (/ n 2))
(define (* a b r)
  (cond [(= b 0) r]
        [(= (halve b) 0) (* (double a) (- (halve b) 1) (+ r (double a)))]
        [else (* a (- b 1) (+ r a))]
   )
  )
(define (wiped-* a b) (* a b 0))

(wiped-* 12 12)