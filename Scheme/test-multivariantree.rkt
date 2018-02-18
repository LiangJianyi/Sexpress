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

(define (get-car t flag)
  (cond [[eq? flag 'mutable] (mcar t)]
        [[eq? flag 'immutable] (car t)]
        [[eq? flag 'both] (if [mpair? t] (mcar t) (car t))]
        [else (raise-arguments-error 'get-pair "Invalid flag" "flag" flag)]))

(define (get-cdr t flag)
  (cond [[eq? flag 'mutable] (mcdr t)]
        [[eq? flag 'immutable] (cdr t)]
        [[eq? flag 'both] (if [mpair? t] (mcdr t) (cdr t))]
        [else (raise-arguments-error 'get-pair "Invalid flag" "flag" flag)]))

(define (pair-or-mpair? t flag)
  (cond [[eq? flag 'mutable] (mpair? t)]
        [[eq? flag 'immutable] (pair? t)]
        [[eq? flag 'both] (or [mpair? t] [pair? t])]
        [else (raise-arguments-error 'get-pair "Invalid flag" "flag" flag)]))

(define (iterator-multitree tree proc [flag 'mutable])
  (if [and (pair-or-mpair? tree flag)
           (procedure? proc)
           (or [eq? flag 'mutable] [eq? flag 'immutable] [eq? flag 'both])]
      (begin
        (if [pair-or-mpair? (get-car tree flag) flag]
            (iterator-multitree (get-car tree flag) proc flag)
            (proc (get-car tree flag)))
        (if [pair-or-mpair? (get-cdr tree flag) flag]
            (iterator-multitree (get-cdr tree flag) proc flag)
            (proc (get-cdr tree flag))))
      (cond [[and (not (pair-or-mpair? tree flag))
                  (not [procedure? proc])
                  (not (or [eq? flag 'mutable] [eq? flag 'immutable]))]
             (raise-arguments-error 'iterator-multitree
                                    "tree 必须是一个序对 、 proc 必须是个可用的过程且 flag 必须是个有效符号"
                                    "tree" tree
                                    "proc" proc
                                    "flag" flag)]
            [[not (or [eq? flag 'mutable] [eq? flag 'immutable])]
             (raise-argument-error 'iterator-multitree "flag 必须是一个有效的符号" 2 tree proc flag)]
            [[not (pair-or-mpair? tree flag)]
             (cond [[eq? flag 'mutable]
                    (raise-argument-error 'iterator-multitree "tree 必须是一个 mpair" 0 tree proc flag)]
                   [[eq? flag 'immutable]
                    (raise-argument-error 'iterator-multitree "tree 必须是一个 pair" 0 tree proc flag)]
                   [[eq? flag 'both]
                    (raise-argument-error 'iterator-multitree "tree 必须是一个 pair 或 mpair" 0 tree proc flag)])]
            [[not (procedure? proc)]
             (raise-argument-error 'iterator-multitree "proc 必须是一个过程" 1 tree proc flag)])))

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
(append-multitree! tree (linkedlist 41234) (vector (vector 0 0)
                                                   (vector 1 0)
                                                   (vector 1 1)))
(mcdr (mcdr (mcar tree)))

tree
(iterator-multitree tree displayln)