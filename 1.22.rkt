#lang racket
(require math/number-theory)
(require racket/date)
(define (square n)(* n n))
(define (find-divisor n test-divisor) (
                                       cond [(> (square test-divisor) n) n]
                                            [(divides? test-divisor n) test-divisor]
                                            [else (find-divisor n (+ test-divisor 1))]
                                      ))
(define (smallest-divisor n)(find-divisor n 2))
(define (prime? n)(= (smallest-divisor n) n))

(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (if (prime? n)
       (report-prime (- (current-inexact-milliseconds) start-time))
       (report-prime -1))
   )
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(time-prime-test 6)
(time-prime-test 199)
(time-prime-test 1999)
(time-prime-test 19999)
