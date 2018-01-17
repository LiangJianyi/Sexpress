#lang racket
(provide make-tree)
(provide append-multitree)

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

(define (append-multitree tree node)
  (if (null? tree)
      node
      (cons (if (leaves? [car tree])
                [car tree]
                (append-multitree (car tree) node))
            (append-multitree (cdr tree) node))))

(define (leaves? node)
  (and [not [pair? node]] [not [null? node]]))

(define (make-treenode value) (cons value null))
