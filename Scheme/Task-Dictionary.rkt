#lang racket
(define (make-lock)
  (define current-index 0)
  (define (the-lock index)
    (cond [[> index current-index]
           (the-lock index)]
          [[= index current-index]
           'execute]
          [[< index current-index]
           (set! current-index (+ current-index 1))
           'release]))
  the-lock)

(define (make-task-constructor)
  (letrec [[lock (make-lock)]]
    (lambda (thunk procedure-index)
      (define index procedure-index-increment)
      (lambda ()
        (when [eq? 'execute (lock index)]
          (thunk)
          (lock -1))))))

(define (order-execute . proc)
  (for ([p proc])
    (thread p)))