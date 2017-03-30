#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(define (horner-eval x sequence)
  (accumulate
            (lambda (a b)
              (if (null? a)
                  0
                   (+ a (* x b))))
            0 sequence))
(horner-eval 2 (list 1 3 0 5 0 1))