#lang racket
(define path "/Users/liangjianyi/desktop/undefine.txt")
(define path-2 "/Users/liangjianyi/desktop/parse-text.jyml")
;(define print-text-file (lambda (ip (reader (read ip)))
;                          (if (eof-object? reader)
;                              (begin (display "\n") (display "read end."))
;                              (begin (display reader) (print-text-file ip)))))

;;; 打印文本内容
(define (print-txt iport)
  (define (f c)
    (if [eof-object? c]
        (close-input-port iport)
        (begin
          (display c)
          (f (read-char iport)))))
  (f (read-char iport)))

;;; 统计文本中换行符数量
(define (newline-count iport)
  (define (f c [tally 0])
    (if [eof-object? c]
        (begin
          (close-input-port iport)
           tally)
          (f (read-char iport)
             (if [equal? c #\newline]
                 (+ tally 1)
                 tally))))
    (f (read-char iport)))

  (print-txt (open-input-file path-2))
  (newline)
  (newline)
  (display "newline count: ")(newline-count (open-input-file path-2))
  