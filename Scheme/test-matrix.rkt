#lang racket
(require liangjianyi-racket/Matrix)

(let ([matrix null])
  (do ([row 1 (+ row 1)])
    ([= row 6] (newline))
    (display "begin row ")(display row)(display " of matrix")
    (newline)
    (do ([column 1 (+ column 1)])
      ([= column 6] (newline))
      (print-matrix (make-matrix row column)))))