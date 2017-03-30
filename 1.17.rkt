;;1.17
#lang racket
(define (* a b)
  (cond [(= 0 b) 0]
        [else (+ a (* a (- b 1)))]
        )
 )
(*  4 4 )
(define (double n) (+ n n))
(define (halve n) (/ n 2))
(define (mult a b)
  (cond [(= b 0) 0]
        [(= (halve b) 0) (+ (double a) (mult a (/ b 2)))]
        [else (+ a (mult a (- b 1)))]
    ))

(mult 6 6)