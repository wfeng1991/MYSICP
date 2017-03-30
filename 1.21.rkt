#lang racket
(require math/number-theory)
(define (square n)(* n n))
(define (find-divisor n test-divisor) (
                                       cond [(> (square test-divisor) n) n]
                                            [(divides? test-divisor n) test-divisor]
                                            [else (find-divisor n (+ test-divisor 1))]
                                      ))
(define (smallest-divisor n)(find-divisor n 2))


(smallest-divisor 6)
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)