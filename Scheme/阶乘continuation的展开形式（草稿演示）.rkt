#lang racket
;; source code
(define continuation null)
(define (set-continuation! proc n)
  (fprintf (current-output-port) "Set continuation when n is ~a\n" n)
  proc)
(define (f n)
  (fprintf (current-output-port) "Invoke f with n as ~a\n" n)
  (if [> n 1]
      (* n (call/cc
            (lambda (k)
              (displayln "Into call/cc")
              (set! continuation
                    (set-continuation! (lambda ()
                                         (fprintf (current-output-port) "(k [* ~a (f (- ~a 1))])\n" n n)
                                         (k [* n (f (- n 1))]))
                                       n))
              (fprintf (current-output-port) "return (f (- ~a 1))\n" n)
              (f (- n 1)))))
      (begin
        (fprintf (current-output-port) "return ~a\n" n)
        n)))









'-----------------------------------------
(k [ * 2 (f (- 2 1))])=>
(f 5)
  (if [> 5 1]
      (* 5 (call/cc (lambda (k)
                      (set! cc (lambda ()
                                 (k [* 5 (f (- 5 1))])))
                      (f (- 5 1))=>
                        (f 4)=>(if [> 4 1]
                                   (* 4 (call/cc (lambda (k)
                                                   (set! cc (lambda ()
                                                              (k [* 4 (f (- 5 1))])))
                                                   (f (- 4 1))=>
                                                     (f 3)=>(if [> 3 1]
                                                                (* 3 (call/cc (lambda (k)
                                                                                (set! cc (lambda ()
                                                                                           (k [* 3 (f (- 3 1))])))
                                                                                (f (- 3 1))=>
                                                                                   (f 2)=>(if [> 2 1]
                                                                                              (* 2 (call/cc (lambda (k)
                                                                                                              2 ;; continuation 调用点，v = [* 2 (f (- 2 1))]
                                                                                                              (f (- 2 1)))))
                                                                                              n)
                                                                                )))
                                                                n)
                                                   )))
                                   n)
                      )))
      n)


;; continuaion 展开的完全形式
[(lambda (v)
   (if [> 5 1]
       (* 5 (call/cc (lambda (k)
                       (set! cc (lambda () (k [* 5 (f (- 5 1))])))
                       (if [> 4 1]
                           (* 4 (call/cc (lambda (k)
                                           (set! cc (lambda () (k [* 4 (f (- 4 1))])))
                                           (if [> 3 1]
                                               (* 3 (call/cc (lambda (k)
                                                               (set! cc (lambda () (k [* 3 (f (- 3 1))])))
                                                               (if [> 2 1]
                                                                   (* 2 v)
                                                                   n))))
                                               n))))
                           n))))
       n))
 (* 2 (f (- 2 1)))]