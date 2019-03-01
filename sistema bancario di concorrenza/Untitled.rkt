#lang racket
(require compatibility/mlist)
(require "./bank-view.rkt")
;(require "../../JanyeeParallel/Serializer.rkt")

(define (parallel-execute . proc)
  (define thd-lst null)
  (for ([p proc])
    (if [eq? thd-lst null]
        (set! thd-lst (list (thread p)))
        (set! thd-lst (append thd-lst (list (thread p))))))
  thd-lst)

(define (make-serializer debug-mode)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (when [eq? debug-mode #t]
            (begin
              (fprintf (current-output-port) "mutex of ~a: ~a\n" args (mutex 'display)) ;test
              (set! args null) ;test
        ))
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (mlist false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             ;; true if already set, false once set
             (if (test-and-set! cell) 
                 (the-mutex 'acquire) ;; retry, causing "blocking"
                 false))
            ((eq? m 'display) (mcar cell)) ;test
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell false))

(define (test-and-set! cell)
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))

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
                    (define serializer (make-serializer #f))
                    (define thd-lst (parallel-execute (serializer (lambda () (while-away deposit-exe-20)))
                                                      (serializer (lambda () (while-away withdraw-exe-80)))))
                    (for ([thd thd-lst])
                      (thread-wait thd))))

(define parallel2 (lambda ()
                    (displayln "Executing parallel2")
                    (define serializer (make-serializer #f))
                    (define thd-lst (parallel-execute (serializer (lambda () (while-away deposit-exe-20)))
                                                      (serializer (lambda () (while-away withdraw-exe-80)))))
                    (for ([thd thd-lst])
                      (thread-wait thd))))

(define parallel3 (lambda ()
                    (displayln "Executing parallel3")
                    (define serializer (make-serializer #f))
                    (define thd-lst (parallel-execute (serializer (lambda () (while-away deposit-exe-20)))
                                                      (serializer (lambda () (while-away withdraw-exe-80)))))
                    (for ([thd thd-lst])
                      (thread-wait thd))))

(define serializer (make-serializer #t))
(define serialize-parallel1 (serializer parallel1))
(define serialize-parallel2 (serializer parallel2))
(define serialize-parallel3 (serializer parallel3))

(define (run)
  (define thd-lst (parallel-execute (lambda () (serialize-parallel1 "parallel1"))
                                    (lambda () (serialize-parallel2 "parallel2"))
                                    (lambda () (serialize-parallel3 "parallel3"))))
  (for ([thd thd-lst])
    (thread-wait thd)))

(run)
