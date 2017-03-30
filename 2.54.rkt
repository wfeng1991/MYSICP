#lang racket
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
;(memq 'apple '(x (apple sauce) y apple pear))
(define (equal? list1 list2)
  (cond [(and (null? list1) (null? list2)) #t]
        [(xor (null? list1) (null? list2)) #f]
        [(and (not (pair? list1)) (not (pair? list1))) (eq? list1 list2)]
        [(xor (pair? list1) (pair? list1)) #f]
        [else (and (equal? (car list1) (car list2))
                   (equal? (cdr list1) (cdr list2))
               )] ))
(equal? '(a (b c) d f) '(a (b c) d f))