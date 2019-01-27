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
(letrec [[get-random-number (lambda () (random -999999 99999))]
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
           mlst)))

(custodian-shutdown-all cust)