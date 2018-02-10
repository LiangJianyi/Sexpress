#lang racket
'----------------------------------CPS-factorial----------------------------------
(define (return x)
  x)

(define (k+ a b k)
  (k (+ a b)))

(define (k* a b k)
  (k (* a b)))

(define (kfact n k)
  (if (= n 1) 
      (k 1)
      (kfact (- n 1) (lambda (x) (k (* n x))))))

;;; invoking kfact
(kfact 5 return)

(define (summation li f)
  (if [null? [cdr li]]
      (f [car li])
      (summation [cdr li] (lambda (x) (f (+ [car li] x))))))

(summation (reverse '(1 2 3 4 5)) return)

(define lik '(1 2 3 4 5))
(set! summation (lambda (i f)
                  (if [= i 0]
                      (f [list-ref lik i])
                      (summation (- i 1) (lambda (x)
                                           (f (- x [list-ref lik i])))))))
(summation (- [length lik] 1) return)

(define (non-number-value-error x)
  (display "Value error: ")
  (display  x)
  (display " is not number.")
  (newline)
  'error)

(define (kproduct ls k k-value-error)
  (let ([break k])
    (let loop ((ls ls) (k k))
      (cond
        ((null? ls) (k 1))
        ((not (number? (car ls))) (k-value-error (car ls)))
        ((zero? (car ls)) (break 0))
        (else (loop (cdr ls) (lambda (x) (k (* (car ls) x)))))))))


(define (search-1 wanted? lst) 
  (for-each (lambda (element) 
              (when (wanted? element) 
                (return element))) 
            lst) 
  #f)

(define (search-2 wanted? lst)
  (lambda (return) 
    (for-each (lambda (element) 
                (when (wanted? element) 
                  (return element))) 
              lst) 
    #f))

(define (search wanted? lst) 
  (call/cc 
   (lambda (return) 
     (for-each (lambda (element) 
                 (when (wanted? element) 
                   (return element))) 
               lst) 
     #f)))

(* 10
   [call/cc (lambda (x)
              (if [> 1 2]
                  (x 2)
                  (+ 4 4)))])

'----------------------------------fib----------------------------------

(define (fib n)
  (define (f i call)
    (cond [[= i 1] (call 1 0)]
          [[= i 2] (f [- i 1] (lambda (x y) (call x [+ y 1])))]
          [[>= i 3]
           (f [- i 1] (lambda (x y)
                        (if [even? i]
                            (call x [+ y x])
                            (call [+ x y] y))))]))

  (f n (lambda (x y)
         (if [even? n] y x))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)

'----------------------------------addition----------------------------------

(define (addition n f)
  (if [= n 1]
      (f 1)
      (addition [- n 1] (lambda (x) (+ (f x) 1)))))

(addition 1 return)
(addition 2 return)
(addition 3 return)
(addition 4 return)

'----------------------------------vector-fill----------------------------------
(define (vector-fill vec pre?)
  (define (f i call)
    (if [= i 0]
        (call (make-vector 0))
        (f [- i 1] (lambda (v)
                     (let ([n (vector-ref vec (- i 1))])
                       (if [pre? n]
                           (call (vector-append v (vector n)))
                           (call v)))))))
  (f (- (vector-length vec) 1) (lambda (v)
                                 (if [pre? (vector-ref vec (- [vector-length vec] 1))]
                                     (vector-append v (vector (vector-ref vec (- [vector-length vec] 1))))
                                     v))))

(vector-fill #(1 2 3 4 5 6 7 8 9 10) odd?)
(vector-fill #(1 2 3 4 5 6 7 8 9 10) even?)