#lang racket
(define (square items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square (cdr items)))
   ))
(define (map func items)
  (if (null? items)
      null
      (cons (func (car items)) (map func (cdr items)))
      ))

(square (list 1 2 3 4))
(map (lambda (x) (* x x)) (list 1 2 3 4))