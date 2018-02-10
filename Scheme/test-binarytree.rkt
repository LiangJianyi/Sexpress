#lang racket
(require liangjianyi-racket/BinaryTree)

(define left (mcons (mcons 1 2) (mcons 3 4)))
(define right (mcons (mcons 5 6) (mcons 7 8)))
(define tree (mcons left right))
(reverse-binarytree tree)

(equal? tree (clone-binarytree tree 'immutable))

(set-value-by-value tree 6 -6)
(set-value-by-value tree #t -9999 'both (lambda (x) (even? x)))
(binarytree-map tree (lambda (x) (* x x)))
