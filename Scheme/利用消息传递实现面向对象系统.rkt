#lang racket
(require racket/date)

(define (make-account name born gender
                      [balance 0]
                      [country "US"]
                      [city "Los Angeles"]
                      [zip-code "035-229"])
  
  (define (gender? arg) (or (equal? gender 'male)
                            (equal? gender 'female)))
  
  (cond [(not (string? name)) (error "The name type not is string.")]
        [(not (date? born)) (error "The born type not is date.")]
        [(not (gender? gender)) (error "The gender type not is symbol of male or female")]
        [else
         (define (set-name! arg)
           (if [string? arg]
               (set! name arg)
               (error "The name type not is string.")))
         (define (set-born! arg)
           (if [date? arg]
               (set! born arg)
               (error "The born type not is date.")))
         (define (set-gender! arg)
           (if [gender? arg]
               (set! gender arg)
               (error "The gender type not is symbol of male or female")))
         (define (deposit! amount)
           (if [<= amount balance]
               (set! balance (- balance amount))
               (error "Insufficient funds!")))
         (define (set-country! arg)
           (set! country arg))
         (define (set-city! arg)
           (set! city arg))
         (define (set-zip-code! arg)
           (set! zip-code arg))
         (lambda (dispatch)
           (cond [(equal? dispatch 'get-name) name]
                 [(equal? dispatch 'get-born) born]
                 [(equal? dispatch 'get-gender) gender]
                 [(equal? dispatch 'get-balance) balance]
                 [(equal? dispatch 'get-country) country]
                 [(equal? dispatch 'get-city) city]
                 [(equal? dispatch 'get-zip-code) zip-code]
                 [(equal? dispatch 'set-name) set-name!]
                 [(equal? dispatch 'set-born) set-born!]
                 [(equal? dispatch 'set-gender) set-gender!]
                 [(equal? dispatch 'deposit) deposit!]
                 [(equal? dispatch 'set-country) set-country!]
                 [(equal? dispatch 'set-city) set-city!]
                 [(equal? dispatch 'set-zip-code) set-zip-code!]
                 [else (error "Error operate!")]))
         ]))



;(current-date) → date*?
;An abbreviation for (seconds->date (* 0.001 (current-inexact-milliseconds))).
;procedure
;(date->string date [time?]) → string?
;  date : date?
;  time? : any/c = #f
;Converts a date to a string. The returned string contains the time of day only if time?. See also date-display-format.
;parameter
;(date-display-format)	 	→	 	
;(or/c 'american
;      'chinese
;      'german
;      'indian
;      'irish
;      'iso-8601
;      'rfc2822
;      'julian)
;(date-display-format format) → void?
;  	format	 	:	 	
;(or/c 'american
;      'chinese
;      'german
;      'indian
;      'irish
;      'iso-8601
;      'rfc2822
;      'julian)

(define bank-users-lists
  (list (make-account "Jianyi Liang" (current-date) 'male 10)
        (make-account "Tia Wenkeman" (current-date) 'female 100)
        (make-account "Valensiya" (current-date) 'female 1000)))

(date-display-format 'chinese)
(do ([i 0 (+ i 1)])
  ([= i (length bank-users-lists)])
  (newline)
  (display "before change details of account:")(newline)
  (display "name: ")(display ([list-ref bank-users-lists i] 'get-name))(display "  ")
  (display "born: ")(display (date->string ([list-ref bank-users-lists i] 'get-born)))(display "  ")
  (display "gender: ")(display ([list-ref bank-users-lists i] 'get-gender))(display "  ")
  (display "balance: ")(display ([list-ref bank-users-lists i] 'get-balance))(display "  ")
  (newline)

  (((list-ref bank-users-lists i) 'deposit) 9.5)
  
  (display "after change details of account:")(newline)
  (display "name: ")(display ([list-ref bank-users-lists i] 'get-name))(display "  ")
  (display "born: ")(display (date->string ([list-ref bank-users-lists i] 'get-born)))(display "  ")
  (display "gender: ")(display ([list-ref bank-users-lists i] 'get-gender))(display "  ")
  (display "balance: ")(display ([list-ref bank-users-lists i] 'get-balance))(display "  ")
  (newline)
  (display "-----------------------------------------------------------------------------------")
  (newline))
