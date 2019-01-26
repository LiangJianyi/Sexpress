#lang racket
(define global -1)
(define cust (make-custodian (current-custodian)))

(define (f1)
  (letrec [[fuck (* global global)]
           [loop (lambda (arg)
                   (if [< arg 100000000]
                       (loop (+ arg 1))
                       arg))]]
    (loop fuck)))

(define (f2) (set! global (+ global 100)))

(define t1 (thread f1))
;(call-in-nested-thread f1 (cust))
;(call-in-nested-thread f2 (cust))

(define (f3)
  (define (f3-1)
    (+ (+ 1 2)
       (call-in-nested-thread (lambda () (+ (+ 1 3)
                                            (call-in-nested-thread (lambda () (+ 1 4))
                                                                   (cust))))
                              (cust))))
  (call-in-nested-thread f3-1 (cust)))

(custodian-shut-down? cust)
(custodian-shutdown-all cust)
(custodian-shut-down? cust)
(custodian-shut-down? (current-custodian))

'------------------------
(define custbox (make-custodian-box (current-custodian) 123))
(custodian-box? custbox)
(custodian-box-value custbox)