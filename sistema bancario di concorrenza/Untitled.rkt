#lang racket
;(require "./bank-view.rkt")

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) balance)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
(define acc1 (make-account 100))
(define acc2 (make-account 39.4))
(define acc3 (make-account 321.99))

(define (while-away thunk x)
  (if [eq? x 111]
      (thunk)
      (while-away thunk (random -999 999))))

(define (withdraw-exe-80)
  (displayln ((acc1 'withdraw) 80)))

(define (withdraw-exe-20)
  (displayln ((acc1 'withdraw) 20)))

(thread (while-away withdraw-exe-20 0))
(thread (while-away withdraw-exe-80 0))
