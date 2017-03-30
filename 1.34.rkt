#lang racket
(define (f g)(g 2))
(f (lambda (x) (* x x)))
(f (lambda (x) (* x (+ x 1))))
(f f)