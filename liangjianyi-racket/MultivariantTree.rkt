#lang racket
(provide make-tree)
(provide append-multitree!)

(define (make-tree leaves depth . values)
  (define (f tree depth v)
    (if [= depth 0]
        tree
        (if [null? v]
            (f [append tree (make-list leaves v)] [- depth 1] v)
            (f [append tree [car v]] [- depth 1] [cdr v]))))
  (cond [(< leaves 0)
         (raise-argument-error 'leaves "leaves 不能小于 0" 0 leaves depth values)]
        [(< depth 0)
         (raise-argument-error 'depth "depth 不能小于 0" 1 leaves depth values)]
        [(or [= leaves 0] [= depth 0]) null]
        [else (if (null? values)
                  (f null depth null)
                  (f null depth values))]))

;;; 树中的每个节点坐标用一个由若干二维向量组成的矩阵表示
;;; 二维向量的左值 x 代表当前深度 y 的列表的第 x 个元素
;;; 二维向量的右值 y 代表左值 x 在整棵树中的深度
(define (append-multitree! t n coords
                           [v #((vector 0 0))]
                           [x 0]
                           [y 0]
                           [current-index 0]
                           [next-index (+ current-index 1)])
  (unless [>= current-index (- (vector-length coords) 2)]
    (cond [[and (null? t) (equal? v #((vector 0 0)))]
           (set! t n)]
          [[mpair? t]
           (cond [[equal? (vector-ref v current-index) (vector-ref coords current-index)]
                  (cond [[> (vector-ref (vector-ref coords next-index) 0) (vector-ref (vector-ref v current-index) 0)]
                         (append-multitree! [mcar t] n coords
                                            (vector-append v #([+ x 1] y))
                                            [+ x 1]
                                            y
                                            [+ current-index 1]
                                            [+ next-index 1])]
                        [[> (vector-ref (vector-ref coords next-index) 1) (vector-ref (vector-ref v current-index) 1)]
                         (append-multitree! [mcdr t] n coords
                                            (vector-append v #(x (+ y 1)))
                                            x
                                            [+ y 1]
                                            [+ current-index 1]
                                            [+ next-index 1])]
                        [(raise-argument-error 'coords "下一节点的坐标值必须大于上一个节点的坐标值" 2 t n coords v x y current-index next-index)])]
                 [else (raise-argument-error 'coords "坐标值非法" 2 t n coords v x y current-index next-index)])])))

(define (leaves? node)
  (and [not [pair? node]] [not [null? node]]))

(define (make-coord x y)
  (vector x y))

(define (coord-x coords)
  (vector-ref coords 0))

(define (coord-y coords)
  (vector-ref coords 1))

(define (append-coord coord x y)
  (vector-append coord (make-coord x y)))
