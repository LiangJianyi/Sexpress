#lang racket
(require liangjianyi-racket/MultivariantTree)

(define top-tree (list null null null))
(define left-tree (list ('(1)) ('(2))))
(define medium-tree (list 3 4 5 (list 7 8 '(-1)) 6))
(define right-tree (list (list '(10 11) 9)))
(append-multitree '(1 2 3) (cons -1 null))