#lang racket
;;斐波那契数列
(define (a n)
  (if (> n 0)
      (+ n (a (- n 1)))
      n)
  )
(define (j b c n)
  (if (< n 2) 
      b
      (j c (+ b c) (- n 1))
   ))
(define (k a)(cond [(< 0 a) a]
                   ((> 0 a) (- a))
                   ((= 0 a) 0)))
;;(j 1 1 9)
;; 换零钱
(define (first-denomination kinds-of-coins)
  (cond ((= 1 kinds-of-coins) 1)
        ((= 2 kinds-of-coins) 5)
        ((= 3 kinds-of-coins) 10)
        ((= 4 kinds-of-coins) 25)
        ((= 5 kinds-of-coins) 50)
   ))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                     (cc (- amount (first-denomination kinds-of-coins))
                         kinds-of-coins))
               )
         )
   )
(define (count-change amount)
  (cc amount 5))

;;(count-change 11)

;;f(n)={ n (n<3)
;;      f(n-1)+2f(n-2)+3f(n-3) (n>=3)
;;}
;; 递归写法
(define (function n)
  (cond ((< n 3) n)
        (else (+ (function (- n 1)) (* 2 (function (- n 2))) (* 3 (function (- n 3)))))
    ))
;;(function 6)
;;非递归写法
(define (f n3 n2 n1 num)
         (cond ((> 3 num) num)
               ((= 3 num) (+ n3 (* 2 n2) (* 3 n1)))
               ((< 3 num) (f (+ n3 (* 2 n2) (* 3 n1)) n3 n2 (- num 1)))
           ))
;;(f 2 1 0 6)
;;pascal三角各个元素
(define (pascal row col)
  (cond [[and (= 0 row) (= 0 col)] 1]
        [[or (= 0 row) (= 0 col)] 0]
        [(= 1 row) 1]
        [(= row col) 1]
        [else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col))
               ]
   )) 
;;(pascal 6 4)

(define (square n) (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((= (/ n 2) 0)
         (square (fast-expt b (- n 1))))
        (else (* b (fast-expt b (- n 1))))))
;;(fast-expt 6 4)

;;1.16

(define (expt b n a)
  (cond ((= n 0) a)
        (else (expt b (- n 1) (* b a)))
        )
  )
(define (wiped-expt b n)
  (expt b n 1))

;;(wiped-expt 6 4)

(define (expt-lg b n a)
  (cond [(= n 0) a]
        [(= (/ n 2) 0) (expt-lg (square b) (- (/ n 2) 1) (* a (square b)))]
        [else (expt-lg b (- n 1) (* a b))]        
        ))

(define (wiped-expt-lg b n)
  (expt-lg b n 1))
(wiped-expt-lg 6 0)