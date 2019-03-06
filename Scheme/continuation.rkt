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

(+ 1 (+ 2 (call/cc (lambda (k) (k 3)))))