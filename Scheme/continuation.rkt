#lang racket
;(let ([v (let/ec out
;           (dynamic-wind
;            (lambda () (display "in "))
;            (lambda ()
;              (display "pre ")
;              (display (call/cc out))
;              #f)
;            (lambda () (display "out "))))])
;  (when v (v "post ")))

;(dynamic-wind
; (lambda ()
;   (display "in ")
;   "in")
; (lambda ()
;   (display "pre ")
;   "pre")
; (lambda ()
;   (display "out ")
;   "out"))

(define saved-k null)
(define (save-it!)
  (call-with-composable-continuation
   (lambda (k)
     (set! saved-k k)
     0)))
(define (sum n)
  (if (zero? n)
      (save-it!)
      (+ n (sum (sub1 n)))))

(+ 1 (+ 1 (+ 1 (save-it!))))
(saved-k 5)

'----------------------
(define (try c h) 
  (call/cc (lambda (ok) 
             (let 
                 ([err (call/cc (lambda (not-ok)
                                  (let
                                      ([x (c not-ok)])
                                    (ok x))))])
               ;(displayln err)
               (h err)))))

;division 
(define (divi a b) (lambda (throw)
                     (if (= b 0)
                         (throw "divi by zero")
                         (/ a b))))

(try (divi 1 0) displayln) ;;”divi by zero”
(try (divi 1 2) displayln) ;;1/2

'----------------------
(define err (call/cc (lambda (k)
                       (k "fuck"))))
(displayln err)

(define continuation null)
(+ 1 (+ 2 (call/cc
           (lambda (k)
             (set! continuation k)
             (k 3)))))
(continuation 10)
(call/cc (lambda (k)
           (set! continuation k)
           (displayln "Initial continuation")))
(continuation 10)
(continuation "hehe....")
(continuation 101010101)
(continuation '----------------------)

(call/cc
 (lambda (e)
   (call/cc
    (lambda (k)
      (set! continuation k)
      (e (+ 1 (k)))))))
(continuation 12345)
(continuation 666)
(continuation '----------------------)

(define x (call/cc
           (lambda (k)
             (set! continuation k)
             (k "hello world")
             (k "fuck you"))))
x
(displayln x)
'----------------------

((lambda (e)
   (call/cc
    (lambda (k)
      (e (+ 1 (k "I 'dont know." "Where I am?" #\f #\u #\c #\k))))))
 displayln)

(+ 1
   (+ [call/cc (lambda (k) (k 93))]
      (+ 2 4)))

'----------------------

(call/cc
 (lambda (k)
   (set! continuation k)
   (+ -21 -1)))
(continuation 0 100000 "Content: " #\f #\u #\c #\k)

(* 3 (call/cc (lambda (k) (+ 1 (k 99)))))

'----------------------

(define forzen null)
(append ((lambda ()
           (displayln "append (list 'the 'call/cc 'returned)")
           (list 'the 'call/cc 'returned)))
        ((lambda ()
           (displayln "create new list")
           (list ((lambda ()
                    (displayln "Jump into call/cc")
                    (call/cc
                     (lambda (k)
                       (displayln "(set! forzen k)")
                       (set! forzen k)
                       (displayln "return symbol a")
                       'a))))))))