#lang racket
(require "./bank-view.rkt")

(define acc1 (make-account 100))
(define acc2 (make-account 39.4))
(define acc3 (make-account 321.99))

((acc1 'deposit) 44.1)
((acc1 'deposit) 22)
((acc1 'withdraw) 44.1)
(acc1 'balance)

(acc2 'balance)
(acc3 'balance)