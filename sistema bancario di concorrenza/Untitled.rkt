#lang racket
(require "./bank-view.rkt")
(require "../../JanyeeParallel/Serializer.rkt")

(define acc1 (make-account-and-serializer-2 100))
(define acc2 (make-account 39.4))
(define acc3 (make-account 321.99))

(define (while-away thunk x)
  (if [eq? x 111]
      (thunk)
      (while-away thunk (random -9999 9999))))

(define (withdraw-exe-80)
  (displayln ((acc1 'withdraw) 80)))

(define (deposit-exe-20)
  (displayln ((acc1 'deposit) 20)))

(parallel-execute (lambda () (while-away deposit-exe-20 0))
                  (lambda () (while-away withdraw-exe-80 0)))
