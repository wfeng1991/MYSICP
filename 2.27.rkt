#lang racket

(define (reverse lst)
    (iter lst '()))

(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result)
              )))
;;通用
(define (deep-reverse lst)
  (if (null? lst)
      lst
      (begin
        (if (pair? (car lst))
            (begin (reverse (cons (reverse (car lst)) (reverse (deep-reverse (cdr lst))))))
            (begin (cons (deep-reverse (cdr lst)) (car lst)))))))
;;仅限于二叉树
(define (deep-reverse1 tree)
    (cond ((null? tree)         ; 空树
            '())
          ((not (pair? tree))   ; 叶子
            tree)
          (else
            (reverse (list (deep-reverse1 (car tree))            ; 递归地逆序左右子树
                           (deep-reverse1 (cadr tree)))))))

(deep-reverse '((1 2) (3 4) (7 8 9) (10 11 12)))