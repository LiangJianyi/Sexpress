#lang racket
(require compatibility/mlist)
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
(define t2 (thread f2))
;(call-in-nested-thread f1 cust)
;(call-in-nested-thread f2 cust)

(define (f3)
  (define (f3-1)
    (+ (+ 1 2)
       (call-in-nested-thread (lambda () (+ (+ 1 3)
                                            (call-in-nested-thread (lambda () (+ 1 4))
                                                                   cust)))
                              cust)))
  (call-in-nested-thread f3-1 cust))
;(f3)
(define t3 (thread f3))

(sync t1 t2 t3)

'------------------------
(define custbox (make-custodian-box (current-custodian) global))
(custodian-box? custbox)
(custodian-box-value custbox)

'------------------------
(letrec [[get-random-number (lambda () (random -999 999))]
      [mlst (mlist 'start)]
      [f1 (lambda ()
            (do [[x 0 (get-random-number)]]
              [[equal? x 1] [mappend! mlst (mlist x)]
                            (displayln x)]))]
      [f2 (lambda ()
            (do [[x 0 (get-random-number)]]
              [[equal? x 2] [mappend! mlst (mlist x)]
                            (displayln x)]))]
      [f3 (lambda ()
            (do [[x 0 (get-random-number)]]
              [[equal? x 3] [mappend! mlst (mlist x)]
                            (displayln x)]))]
      [f4 (lambda ()
            (do [[x 0 (get-random-number)]]
              [[equal? x 4] [mappend! mlst (mlist x)]
                            (displayln x)]))]]
  (let [[t1 (thread f1)]
        [t2 (thread f2)]
        [t3 (thread f3)]
        [t4 (thread f4)]]
    (begin (sync t1 t2 t3 t4)
           mlst
           (kill-thread t1)
           (kill-thread t2)
           (kill-thread t3)
           (kill-thread t4))))


'-------------------------
;(define ch (make-channel))
;(thread (λ () (displayln (sync ch))))
;(channel-put ch (lambda () (+ 1 2)))

(define t5 (thread (lambda () (displayln "This is a new thread."))))
(define t6 (thread (lambda () (displayln "This is another new thread."))))

(define worker (thread (lambda ()
                         (let loop ([x 0])
                           (displayln "Working...")
                           (if [= x 100]
                               (begin (display (current-thread))
                                      (displayln "is done.")
                                      (kill-thread (current-thread)))
                               (loop [+ x 1]))))))

(kill-thread t5)
(kill-thread t6)
(thread-wait worker)
(if [thread-dead? worker]
    (displayln "worker was dead.")
    (kill-thread worker))

;(do [[x 0 (random -9999 9)]]
;  [[equal? x 1] (displayln x)]
;  (displayln x))
;(sleep 12.5)
;(displayln "done")

(custodian-shutdown-all cust)