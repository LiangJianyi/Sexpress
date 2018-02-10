#lang racket

(define (class-employee name id age phone)
  (lambda (f) (f name id age phone)))

(define (get-name emp)
  (emp (lambda (arg1 arg2 arg3 arg4)
         arg1)))
(define (get-id emp)
  (emp (lambda (arg1 arg2 arg3 arg4)
         arg2)))
(define (get-age emp)
  (emp (lambda (arg1 arg2 arg3 arg4)
         arg3)))
(define (get-phone emp)
  (emp (lambda (arg1 arg2 arg3 arg4)
         arg4)))

(define emp1 (class-employee "Jianyi Liang" "bdqn-001" 21 "+1 (951)-223-1469"))
(get-name emp1)
(get-id emp1)
(get-age emp1)
(get-phone emp1)

'-----------------------------------------------------------------------------------

(define (common-getter obj flag)
  (cond [(equal? 'name flag) (obj (lambda (arg1 arg2 arg3 arg4) arg1))]
        [(equal? 'id flag) (obj (lambda (arg1 arg2 arg3 arg4) arg2))]
        [(equal? 'age flag) (obj (lambda (arg1 arg2 arg3 arg4) arg3))]
        [(equal? 'phone flag) (obj (lambda (arg1 arg2 arg3 arg4) arg4))]
        [else (error "Invalid property.")]))

(common-getter emp1 'name)
(common-getter emp1 'id)
(common-getter emp1 'age)
(common-getter emp1 'phone)


'-----------------------------------------------------------------------------------
(define (f)
  (define var1 0)
  (lambda ()
    (define var2 1)
    (lambda ()
      (define var3 2)
      (lambda ()
        (define var4 3)
        (lambda (foo incre)
          (set! var1 (+ var1 incre))
          (set! var2 (+ var2 incre))
          (set! var3 (+ var3 incre))
          (set! var4 (+ var4 incre))
          (foo var1 var2 var3 var4))))))

(((((f))))
 [lambda (a b c d)
   (values a b c d)]
 100)


'-----------------------------------------------------------------------------------
(define (make-accumulator init)
  (let ([incre init])
    (lambda ([arg null])
      (if [not [null? arg]]
          (begin
            (set! incre (+ incre arg))
            incre)
          incre))))