#lang racket

;;倒数

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx))
  )
(define dx 0.00001)

(define (cube x) (* x x x))

;;((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))
   )
  )

;;1.41
(define (double f)
  (lambda (x)
    (f (f x))
   ))

(((double (double double)) (lambda (x) (+ 1 x))) 5)

;;1.42

(define (compose f g)
  (lambda (x) (f (g x))))

((compose (lambda (x) (* x x)) (lambda (x) (+ 1 x))) 6)

;;1.43
(define (repeated f n)
  (cond [(= n 1) f]
        [else (repeated (compose f f) (- n 1))])
  )

((repeated (lambda (x) (* x x)) 2) 5)