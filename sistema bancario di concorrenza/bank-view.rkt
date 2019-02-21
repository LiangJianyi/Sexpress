#lang racket
(provide make-account)
(provide make-account-and-serializer)
(require "../../JanyeeParallel/Serializer.rkt")

;; Two main organizational strategies: object based and
;; stream based. Objects are hard because how can they change
;; and yet maintain their identity. Streams are most powerful
;; when one can decouple time from the order of events.

;; A message passing style stateful closure that allows for a
;; dispatch to fully encapsulate and allow for external
;; mutation of its state 
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((serializer (make-serializer)))
    (let ((serializer-withdraw (serializer withdraw))
          (serializer-deposit (serializer deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) serializer-withdraw)
              ((eq? m 'deposit) serializer-deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) serializer)
              (else (error "Unknown request -- MAKE-ACCOUNT" m))))
      dispatch)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))



;; Rewriting withdraw to encapsulate the balance
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; A withdraw that returns a "stateful processor"
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))


;; A message passing style stateful closure that allows for a
;; dispatch to fully encapsulate and allow for external
;; mutation of its state 
;(define (make-account balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance)
;        "Insufficient funds"))
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance)
;  (define (dispatch m)
;    (cond ((eq? m 'withdraw) withdraw)
;          ((eq? m 'deposit) deposit)
;          (else (error "Unknown request -- MAKE-ACCOUNT"
;                       m))))
;  dispatch)


;; 3.1 -- accumulator
(define (make-accumulator init)
  (lambda (to-add)
    (begin (set! init (+ init to-add))
           init))) 

(define (make-monitored f)
  (let ((count 0))
    (define (dispatch m)
      (cond ((eq? m 'reset) (set! count 0))
            ((eq? m 'how-many-calls?) count)
            (else (begin
                    (set! count (+ 1 count))
                    (f m)))))
    dispatch))


;; 3.3 - Modified make-account
(define (make-account2 balance password)
  (let ((invalid-pass-count 0))
    (define (call-the-cops) "BAD BOYS, BAD BOYS, WHATCHA GUNNA DO?!!")
    (define (invalid-pass x)
      (if (= 6 invalid-pass-count)
          (call-the-cops)
          (begin
            (set! invalid-pass-count (+ 1 invalid-pass-count))
            "Incorrect password")))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass-guess m)
      (cond ((eq? m 'authorized) (eq? password pass-guess))
            ((not (eq? password pass-guess)) invalid-pass)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))