#lang racket
(require "./bank-view.rkt")
(require "../../JanyeeParallel/Serializer.rkt")

(define acc1 (make-account 100))
;(define acc2 (make-account 39.4))
;(define acc3 (make-account 321.99))

(define (while-away thunk [x 0])
  (if [eq? x 111]
      (thunk)
      (while-away thunk (random -9999 9999))))

(define (withdraw-exe-80)
  (displayln "Executing withdraw-exe-80")
  (displayln ((acc1 'withdraw) 80)))

(define (deposit-exe-20)
  (displayln "Executing deposit-exe-20")
  (displayln ((acc1 'deposit) 20)))


(define parallel1 (lambda ()
                    (displayln "Executing parallel1")
                    (parallel-execute (lambda () (while-away deposit-exe-20))
                                      (lambda () (while-away withdraw-exe-80)))))

(define parallel2 (lambda ()
                    (displayln "Executing parallel2")
                    (parallel-execute (lambda () (while-away deposit-exe-20))
                                      (lambda () (while-away withdraw-exe-80)))))

(define parallel3 (lambda ()
                    (displayln "Executing parallel3")
                    (parallel-execute (lambda () (while-away deposit-exe-20))
                                      (lambda () (while-away withdraw-exe-80)))))

(define serializer (make-serializer))
(define serialize-parallel1 (serializer parallel1))
(define serialize-parallel2 (serializer parallel2))
(define serialize-parallel3 (serializer parallel3))

(define thd-lst (parallel-execute serialize-parallel1
                  serialize-parallel2
                  serialize-parallel3))
(for ([thd thd-lst])
  (thread-wait thd))

