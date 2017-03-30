#lang racket
(define (fringe x)
  (if (null? x)
      null
      (begin
        (if (not (pair? (car x)))
         (cons (car x) (fringe (cdr x)))
         (append
          (fringe (car x))
          (fringe (cdr x)))
       )
       ))
  )
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))