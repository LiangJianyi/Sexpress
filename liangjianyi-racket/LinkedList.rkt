#lang racket
(define (mpair-iterator-stop? link) (or (not (mpair? link)) (null? link)))

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

(define (iterator-linkedlist lik proc)
  (proc lik)
  (cond [[mpair? lik] (iterator-linkedlist (mcdr lik) proc)]))

(define (linkedlist-length lik)
  (letrec ((length 0)
           (f (lambda (linkedlist)
                (cond [[mpair? linkedlist] (set! length (+ length 1)) (f (mcdr linkedlist))]
                      [else length]))))
    (f lik)))

(define (linkedlist-ref lik k)
  (letrec ((f (lambda (linkedlist i)
                (if (= i k)
                    (mcar linkedlist)
                    (f (mcdr linkedlist) (+ i 1))))))
    (f lik 0)))

(define (find-node? lik arg)
  (if (equal? lik arg)
      #t
      (if (mpair-iterator-stop? arg)
          #f
          (find-node? (mcdr lik) arg))))

(define (linkedlist-reverse lik)
  (define (f lik rev)
    (if (mpair-iterator-stop? lik)
        (if (null? lik)
            rev
            (append-linkedlist lik rev))
        (f (mcdr lik) (append-linkedlist (mcar lik) rev))))
  (f lik null))

(define (set-mcar-by-ref! lik ref node)
  (letrec ((n (linkedlist-length lik))
           (f (lambda (i lik aux)
                (if (= i n)
                    aux
                    (if (= i ref)
                        (if (null? (mcdr lik))
                            (f (+ i 1) (mcdr lik) (append-linkedlist aux (mcons node null)))
                            (f (+ i 1) (mcdr lik) (append-linkedlist aux (mcons node (mcdr lik)))))
                        (f (+ i 1) (mcdr lik) (append-linkedlist aux (mcons (mcar lik) null))))))))
    (f 0 lik null)))

;(eval (define abc -1))