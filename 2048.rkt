#lang racket
(require 2htdp/universe
         2htdp/image)


;;对于这样一个4x4的棋盘,最直观的想法是用一个嵌套的 list 来表达

(define (make-board n)
  (make-list n (make-list n 0)))
;;(make-board 4) ----> '((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))

;;我们需要在棋盘上的空闲位置随机放两个棋子,
;;所以我们先要能够随机从 '(2 2 2 2 2 2 2 2 2 4) 中挑一个出来做棋子

(define PIECE_DIST '(2 2 2 2 2 2 2 2 2 4))

(define (choice l)
  (if (list? l)
      (list-ref l (random (length l)))
      (vector-ref l (random (vector-length l)))))

(define (get-a-piece)
  (choice PIECE_DIST))

;;(get-a-piece)--->2 or 4

;;接下来我们要随机找出一个空闲的位置

;;avail? 递归查看一个棋盘或者棋盘上的一行是否有 0,来决定是否可以往上放棋子
(define (avail? lst)
  (if (list? lst)
      (ormap avail? lst)
      (zero? lst)))
;;get-empty-refs 获取当前棋盘(或者一行)上面的的可放棋子的行(或者行中元素)的索引列表,以便于我们随机摆放棋子
(define (get-empty-refs lst zero-fun?)
  (for/list ([item lst]
             [i (range (length lst))]
             #:when (zero-fun? item))
    i))


;;我们可以递归选择一个随机的行,随机的列,放入一个随机的棋子
(define (put-random-piece lst)
  (if (avail? lst)
      (if (list? lst)
          (let* ([i (choice (get-empty-refs lst avail?))]
                 [v (list-ref lst i)])
            (append (take lst i)
                    (cons (put-random-piece v) (drop lst (add1 i)))))
            (get-a-piece))
       lst))

;;(get-empty-refs '(0 0 0 2) avail?)
;;--->'(0 1 2)
;;(get-empty-refs '((2 2) (2 0) (0 0)) avail?)
;;--->'(1 2)
;;(put-random-piece '((0 2 0 0) (2 4 8 16) (0 4 4 8) (2 0 0 0)))
;;--->'((0 2 0 0) (2 4 8 16) (0 4 4 8) (2 0 2 0))

;;初始化棋盘

(define (init-board n)
  (put-random-piece (put-random-piece (make-board n))))



;;(init-board 4)
;;--->'((0 0 0 0) (0 0 2 0) (0 2 0 0) (0 0 0 0))
;;--->'((0 0 2 4) (0 0 0 0) (0 0 0 0) (0 0 0 0))
;;--->'((0 0 0 0) (0 0 0 0) (0 2 0 0) (2 0 0 0))
;;--->'((0 0 2 0) (0 0 0 0) (0 0 0 0) (0 0 0 2))

;;接下来就是按照规则合并棋子

;;merge 会从第一个元素起递归处理列表中的所有元素,如果相等就两两合并,然后返回合并后的列表
(define (merge row)
  (cond [(<= (length row) 1) row]
        [(= (first row) (second row))
         (cons (* 2 (first row)) (merge (drop row 2)))]
        [else (cons (first row) (merge (rest row)))]))

;;(merge '(2 2 2 4 4 4 8))
;;'(4 2 8 4 8)


;;如果两个数值相同的元素,比如:'(2 0 2 0),中间隔着 0 怎么办?
;;我们可以用 filter 返回非 0 的元素,然后再把 0 补齐。这就是 move-row 要做的事情:
(define (move-row row v left?)
  (let* ([n (length row)]
         [l (merge (filter (lambda (x) (not (zero? x))) row))]
         [padding (make-list (- n (length l)) v)])
    (if left?
        (append l padding)
        (append padding l))))

(define (move lst v left?)
  (map (lambda (x) (move-row x v left?)) lst))

;;(move '((0 2 0 0) (2 4 8 16) (0 4 4 8) (2 0 0 0)) 0 #t)
;;'((2 0 0 0) (2 4 8 16) (8 8 0 0) (2 0 0 0))


;;我们实现四个方向上的移动
(define (move-left lst)
  (put-random-piece (move lst 0 #t)))
(define (move-right lst)
  (put-random-piece (move lst 0 #f)))
;;apply 是个神奇的函数,如果你学过其它函数式编程语言,或者经常写javascript,那么你一定知道 apply。它能够让传递给函数的列表展开,成为函数执行时的若干个参数。
(define (transpose lsts)
  (apply map list lsts))
(define (move-up lst)
  ((compose1 transpose move-left transpose) lst))
(define (move-down lst)
  ((compose1 transpose move-right transpose) lst))

;;当任意一个方向上移动的结果和移动前相同,意味着游戏结束

(define ALL-OPS (list move-right move-down move-left move-up))
(define (finished? lst)
  (andmap (lambda (op) (equal? lst (op lst))) ALL-OPS))

;;(finished? '((2 8 4 2) (8 4 8 16) (4 32 2 4) (2 16 4 2)))
;;--->#t

(define (test-play lst step)
  (if (and (not (avail? lst)) (finished? lst))
      (values lst step)
      (test-play ((choice ALL-OPS) lst) (add1 step))))
;;(test-play (init-board 4) 0)
;;--->168 149


;;我们先来点体力活,定义游戏的配色。由于互联网世界的颜色表示均使用十六进制的hex码,而 2htdp/image 里使用的 color 是用RGBA定义,因此我们需要做个颜色的转换
(define (hex->rgb hex [alpha 255])
    (define r (regexp-match #px"^#(\\w{2})(\\w{2})(\\w{2})$" hex))
    (define (append-hex s) (string-append "#x" s))
    (define (color-alpha c) (apply color (append c (list alpha))))
    (if r
        (color-alpha (map (compose1 string->number append-hex) (cdr r)))
        #f))

;;(hex->rgb "#aabbcc" 186)
;;--->(color 170 187 204 186)

;;有了这个函数,我们就可以很方便地定义配色和棋子大小
(define ALPHA 184)
(define GRID-COLOR (hex->rgb "#bbada0"))
(define TILE-BG
    (make-hash (map (lambda (item) (cons (first item) (hex->rgb (second item))))
         '((0    "#ccc0b3") (2    "#eee4da") (4    "#ede0c8")
           (8    "#f2b179") (16   "#f59563") (32   "#f67c5f")
           (64   "#f65e3b") (128  "#edcf72") (256  "#edcc61")
           (512  "#edc850") (1024 "#edc53f") (2048 "#edc22e")))))
(define TILE-FG 'white)
(define TILE-SIZE 80)
(define TILE-TEXT-SIZE 50)
(define MAX-TEXT-SIZE 65)
(define TILE-SPACING 5)


;;接下来就是显示一个棋子。不同数值的棋子的颜色不同,而值为 0 的棋子不显示数字。
;;我们还得处理一些显示的问题,比如说 2048 这样的数值,如果以预定义的大小显示,
;;则会超出棋子的大小,所以我们需要 scale
(define (make-tile n)
    (define (text-content n)
      (if (zero? n) ""
          (number->string n)))
  
    (overlay (let* ([t (text (text-content n) TILE-TEXT-SIZE TILE-FG)]
                    [v (max (image-width t) (image-height t))]
                    [s (if (> v MAX-TEXT-SIZE) (/ MAX-TEXT-SIZE v) 1)])
               (scale s t))
             (square TILE-SIZE 'solid (hash-ref TILE-BG n))
             (square (+ TILE-SIZE (* 2 TILE-SPACING)) 'solid GRID-COLOR)))

;;(make-tile 2048)
;;(make-tile 2)
;;(make-tile 0)

;;如果你读了 Quick: An Introduction to Racket with Pictures 的话,
;;你会对 racket/pict 中的 hc-append 和 vc-append 两个函数有印象。
;;可惜这两个函数不接受我们使用 2htdp/image 中的各种方式制作出来的 image,
;;所以 make-tile 生成的图片无法使用这两个函数,那我们就只好自己写了
(define (image-append images get-pos overlap)
    (if (<= (length images) 1)
        (car images)
        (let* ([a (first images)]
               [b (second images)]
               [img (apply overlay/xy
                           (append (list a) (get-pos a overlap) (list b)))])
          (image-append (cons img (drop images 2)) get-pos overlap))))

(define (hc-append images [overlap 0])
    (image-append images
                  (λ (img o) (list (- (image-width img) o) 0))
                  overlap))

(define (vc-append images [overlap 0])
    (image-append images
                  (λ (img o) (list 0 (- (image-height img) o)))
                  overlap))

;;(hc-append (map make-tile '(0 2 4 8)) 5)
;;(vc-append (map make-tile '(1024 256 4 8)) 5)

(define (show-board b)
    (let ([images (for/list ([row b])
                    (hc-append (map make-tile row) TILE-SPACING))])
      (vc-append images TILE-SPACING)))

;;(show-board (init-board 4))

;;首先我们先把键盘操作映射成为我们之前制作好的函数
(define (key->ops a-key)
    (cond
      [(key=? a-key "left")  move-left]
      [(key=? a-key "right") move-right]
      [(key=? a-key "up")    move-up]
      [(key=? a-key "down")  move-down]
      [else (λ (x) x)]))

(define (show-board-over b)
    (let* ([board (show-board b)]
           [layer (square (image-width board) 'solid (color 0 0 0 90))])
      (overlay (text "Game over!" 40 TILE-FG)
               layer board)))
;;(show-board-over (init-board 5))

(define (change b key)
    ((key->ops key) b))
;;(init-board n) 初始化一个棋盘,
;;这个棋盘状态会传入 show_board,change,finished? 和 show-board-over 中。
;;同样,这些函数都会返回一个新的棋盘状态,供 big-bang 下次事件发生的时候使用
(define (start n)
    (big-bang (init-board n)
              (to-draw show-board)
              (on-key change)
              (stop-when finished? show-board-over)
              (name "wfeng's 2048")))

;;(show-board (init-board 4))

(start 10)