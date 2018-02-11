#lang racket
(require liangjianyi-racket/MultivariantTree)
(require liangjianyi-racket/LinkedList)

;(define top-tree (list null))
;(define left-tree (list ('(1)) ('(2))))
;(define medium-tree (list 3 4 5 (list 7 8 '(-1)) 6))
;(define right-tree (list (list '(10 11) 9)))

(define tree (linkedlist
              (linkedlist (linkedlist "def" (linkedlist "f") "a")
                          (linkedlist "f"))))

;(define (append-multitree! t n coords
;                           [v (vector (vector 0 0))]
;                           [x 0]
;                           [y 0]
;                           [current-index 0]
;                           [next-index (+ current-index 1)])
;  (if [<= current-index (- (vector-length coords) 2)]
;      (cond [[and (null? t) (equal? v (vector (vector 0 0)))]
;             (set! t n)]
;            [[mpair? t]
;             (cond [[equal? (vector-ref v current-index) (vector-ref coords current-index)]
;                    ; 一定要先判断左值再判断右值
;                    (cond [[> (vector-ref (vector-ref coords next-index) 0) (vector-ref (vector-ref v current-index) 0)]
;                           (append-multitree! [mcar t] n coords
;                                              (vector-append v (vector (vector [+ x 1] y)))
;                                              [+ x 1]
;                                              y
;                                              [+ current-index 1]
;                                              [+ next-index 1])]
;                          [[> (vector-ref (vector-ref coords next-index) 1) (vector-ref (vector-ref v current-index) 1)]
;                           (append-multitree! [mcdr t] n coords
;                                              (vector-append v (vector (vector x (+ y 1))))
;                                              x
;                                              [+ y 1]
;                                              [+ current-index 1]
;                                              [+ next-index 1])]
;                          [(raise-argument-error 'coords "下一节点的坐标值必须大于上一个节点的坐标值" 2 t n coords v x y current-index next-index)])]
;                   [else (raise-argument-error 'coords "坐标值非法" 2 t n coords v x y current-index next-index)])])
;      (if [null? (mcar t)]
;          (set-mcar! t n)
;          (set-mcdr! t n))))

(define coord (vector (vector 0 0)
                      (vector 1 0)
                      (vector 2 0)
                      (vector 2 1)
                      (vector 2 2)))
(append-multitree! tree (mcons -1 null) coord)
(mcdr (mcdr (mcdr (mcar (mcar tree)))))
(append-multitree! tree (linkedlist 'fuck) (vector [vector-ref coord 0]))
(mcdr tree)
(append-multitree! tree (linkedlist 'FUCK) (vector (vector 0 0)
                                                   (vector 0 1)))
(mcdr (mcdr tree))

tree