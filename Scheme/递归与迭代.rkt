#lang racket
;; 线性迭代
(define (factorial num)
  (define (iterator counter product)
    (cond [[<= counter num] (iterator (+ counter 1) (* counter product))]
          [else product]))
  (iterator 1 1))

;; 线性递归
(define (fac num)
  (cond [[> num 1] (* num (fac (- num 1)))]
        [[= num 1] num]))

(define (fibonacci num)
  (cond [[> num 2] (+ (fibonacci (- num 2)) (fibonacci (- num 1)))]
        [[or (= num 2) (= num 1)] 1]))

;; 线性递归（高阶形式）
(define (high-fact n)
  (letrec ([f (lambda (n proc)
                (if (= n 1)
                    (proc n)
                    (f (- n 1) (lambda (x) (proc (* n x))))))])
    (f n (lambda (x) x))))