#lang racket

(define (sum function a next b)
  (if (> a b)
      0
      (+ (function a)
         (sum function (next a) next b))))
(define (cube n) (* n n n))
(define (inc n) (+ n 1))
;;(sum cube 1 inc 10)

(define (integral f a b dx)
  (define (add-dx x)(+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
  )
(integral cube 0 1 0.0001)


