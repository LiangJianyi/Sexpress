#lang racket
(provide make-binarytree)
(provide append-binarytree)
(provide iterator-binarytree)
(provide clone-binarytree)
(provide set-value-by-value)
;(provide set-value-by-value!)
;(provide set-value-by-node)
;(provide set-value-by-node!)
(provide reverse-binarytree)

(define (make-binarytree deep [fill null])
  (if (= deep 0)
      null
      (letrec ([f (lambda (incre [tree null])
                    (if (> incre deep)
                        tree
                        (begin
                          (set! tree (append-binarytree tree (mcons null null)))
                          (f (+ incre 1) tree))))])
        (f 0))))

(define (mpair-iterator-stop? link) (or (not (mpair? link)) (null? link)))

(define (append-binarytree tree node)
  (if (null? tree)
      node
      (mcons (append-binarytree (mcar tree) node)
             (append-binarytree (mcdr tree) node))))

(define (iterator-binarytree tree proc)
  (proc tree)
  (when (mpair? tree)
    (iterator-binarytree (mcar tree) proc)
    (iterator-binarytree (mcdr tree) proc)))


(define (clone-binarytree tree [variant-option 'mutable])
  (cond [[eq? variant-option 'mutable]
         (if (mpair? tree)
             (mcons (clone-binarytree (mcar tree)) (clone-binarytree (mcdr tree)))
             tree)]
        [[eq? variant-option 'immutable]
         (if (pair? tree)
             (cons (clone-binarytree (car tree) 'immutable) (clone-binarytree (cdr tree) 'immutable))
             tree)]
        [else (raise-argument-error 'variant-option "variant-option 只能是以下两种选项: 'mutable or 'imutable" 1 tree variant-option)]))


; 根据传入的值修改相应原子节点；
; tree: 二叉树（数据源）
; value: 如果当前节点与该参数相等，用 target 替换掉原来的值，该值的类型不能是序对
; target: 目标值，该值的类型不能是序对
; variant-option: 数据节点的易变性，该参数只有三个选项：mutable immutable both，全部用符号表示
; atom-proc: 遍历每个原子节点需要进行的操作，以原子节点为参数，返回的值与 value 做比较
(define (set-value-by-value tree value target [variant-option 'mutable] [atom-proc null])
  (cond [[or (pair? value) (mpair? value)]
         (raise-argument-error 'value "value 不可以为序对" 1 tree value target variant-option atom-proc)]
        [[or (pair? target) (mpair? target)]
         (raise-argument-error 'target "target不可以为序对" 2 tree value target variant-option atom-proc)]
        [(cond [[eq? variant-option 'mutable]
                (if (mpair? tree)
                    (mcons (set-value-by-value [mcar tree] value target variant-option atom-proc)
                           (set-value-by-value [mcdr tree] value target variant-option atom-proc))
                    (if [null? atom-proc]
                        (if (equal? tree value)
                            target
                            tree)
                        (if (equal? (atom-proc tree) value)
                            target
                            tree)))]
               [[eq? variant-option 'immutable]
                (if (pair? tree)
                    (cons (set-value-by-value [car tree] value target variant-option atom-proc)
                          (set-value-by-value [cdr tree] value target variant-option atom-proc))
                    (if [null? atom-proc]
                        (if (equal? tree value)
                            target
                            tree)
                        (if (equal? (atom-proc tree) value)
                            target
                            tree)))]
               [[eq? variant-option 'both]
                (cond [(pair? tree)
                       (cons (set-value-by-value [car tree] value target variant-option atom-proc)
                             (set-value-by-value [cdr tree] value target variant-option atom-proc))]
                      [(mpair? tree)
                       (mcons (set-value-by-value [mcar tree] value target variant-option atom-proc)
                              (set-value-by-value [mcdr tree] value target variant-option atom-proc))]
                      [else (if [null? atom-proc]
                                (if (equal? tree value)
                                    target
                                    tree)
                                (if (equal? (atom-proc tree) value)
                                    target
                                    tree))])]
               [else (raise-argument-error 'variant-option "variant-option 只能是以下三种选项: 'mutable, 'immutable or 'both" 2 tree value target variant-option atom-proc)])]))


(define (reverse-binarytree tree)
  (when (mpair? tree)
    (if (and [mpair? [mcar tree]] [mpair? [mcdr tree]])
        (mcons [reverse-binarytree (mcdr tree)] [reverse-binarytree (mcar tree)])
        (mcons [mcdr tree] [mcar tree]))))
