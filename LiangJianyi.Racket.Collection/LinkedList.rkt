#lang racket
(define (mpair-iterator-stop? link) (or (not (mpair? link)) (null? link)))

;(define (make-node left right) (mcons left right))

(define (append-linkedlist list1 list2)
  (if (mpair? list1)
      (mcons (mcar list1) (append-linkedlist (mcdr list1) list2))
      (if (null? list1)
          list2
          (mcons list1 list2))))

(define (prepend-linkedlist list1 list2)
  (if (mpair? list2)
      (mcons (mcar list2) (prepend-linkedlist list1 (mcdr list2)))
      (if (null? list2)
          list1
          (mcons list2 list1))))

(define (make-linkedlist length)
  (letrec ((f (lambda (incre)
                (if (> incre length)
                    linkedlist
                    (begin
                      (set-mcdr! linkedlist null)
                      (f (+ incre 1))))))
           (linkedlist (mcons null null)))
    (f 1)))

(define (string->linkedlist s)
  (letrec ((s-length (string-length s))
           (linkedlist null)
           (conver (lambda (i)
                     (if (= i s-length)
                         linkedlist
                         (begin
                               (set! linkedlist (append-linkedlist linkedlist (mcons (string-ref s i) null)))
                               (conver (+ i 1)))
                         ))))
    (conver 0)))

(define (iterator-linkedtree tree proc)
  (if (mpair? tree)
      (begin
        (iterator-linkedtree (mcar tree) proc)
        (iterator-linkedtree (mcdr tree) proc))
      (proc tree)))

(define (iterator-linkedlist linkedlist proc)
  (proc linkedlist)
  (cond [[mpair? linkedlist] (iterator-linkedlist (mcdr linkedlist) proc)]))

(define (linkedlist-length linkedlist)
  (letrec ((length 0)
           (f (lambda (linkedlist)
                (cond [[mpair? linkedlist] (set! length (+ length 1)) (f (mcdr linkedlist))]
                      [else length]))))
    (f linkedlist)))

(define (linkedlist-ref linkedlist k)
  (letrec ((f (lambda (linkedlist i)
                (if (= i k)
                    (mcar linkedlist)
                    (f (mcdr linkedlist) (+ i 1))))))
    (f linkedlist 0)))

;(define (contents? arg) ())

(define (find-node? linkedlist arg)
  (if (equal? linkedlist arg)
      #t
      (if (mpair-iterator-stop? arg)
          #f
          (find-node? (mcdr linkedlist) arg))))