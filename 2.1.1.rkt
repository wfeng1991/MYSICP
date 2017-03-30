#lang racket
;;定义有理数
(require math/number-theory)
(define (make-rat n d)(cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define onehalf (make-rat 1 2))
;;(print-rat onehalf)

(define (% a b)
  (if (< a b)
      a
      (% (- a b) b)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (% a b))))

;;(divides? 10 20)

(gcd 50 20)