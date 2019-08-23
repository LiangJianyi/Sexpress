#lang racket

(define (memo-proc proc)
  (let [[already-run? false]
        [result false]]
    (lambda ()
      (if [not already-run?]
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax-rule (delay exp)
  (memo-proc (lambda () exp)))

(define-syntax-rule (force delayed-object)
  (delayed-object))

(define-syntax-rule (cons-stream a b) (cons a (delay b)))

(define-syntax-rule (stream-car stream) (car stream))

(define-syntax-rule (stream-cdr stream) (force (cdr stream)))

(define total 0)
(define (stream-enumerate-interval low high)
  (set! total (+ total 1))
  (display "total:")(displayln total)
  (if [> low high]
      null
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))