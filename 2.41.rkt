#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (or (< low high) (= low high))
      (cons low (enumerate-interval (+ low 1) high))
      null
   ))
;(enumerate-interval 1 7)
(define (tri-list n)
  (accumulate append
            null
            (map (lambda (k)
                   (map (lambda (p) (append k (list p)))
                        (enumerate-interval 1 (- (cadr k) 1))))
                          (two-list n))))
(define (two-list n)
  (accumulate append
            null
            (map (lambda (i)
                      (map (lambda (j) (list i j))
                           (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n)))
                  )
;(two-list 8)
;(tri-list 8)
(define (fliter proc list)
     (map (lambda (x) (if (proc x)
                          x
                          null
                       ))
          list))
(define (sum-list list)
  (if (null? list)
      0
      (if (pair? list)
      (+ (car list) (sum-list (cdr list)))
      0)
   ))
  
(define (fliter-sum n s)
  (fliter (lambda (x) (= (sum-list x) s))
          (tri-list n)))
(fliter-sum 8 11)
