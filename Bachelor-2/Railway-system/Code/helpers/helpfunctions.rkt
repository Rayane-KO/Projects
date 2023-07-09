#lang racket
(require racket/date)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Helpfunctions                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(define id 3)
(define (make-id)
  (let* ((number-str (number->string id))
         (id-str (string-append "T-" number-str)))
    (set! id (+ id 2))
    (string->symbol id-str)))

(define (single-digit n)
  (let ((n-str (number->string n)))
    (if (< n 10)
        (string-append "0" n-str)
        n-str)))
      
(define (get-time)
  (let* ((current-date (current-date))
         (hour (date-hour current-date))
         (minute (date-minute current-date))
         (second (date-second current-date))
         (separator ":"))
    (string-append (single-digit hour)
                   separator
                   (single-digit minute)
                   separator
                   (single-digit second))))

(define (char->number char)
  (- (char->integer char) 48))

(define (id->timer id)
  (if (dtb? id)
      (let ((string-id (symbol->string id)))
        (+ (* (char->number (string-ref string-id 0)) 10) (char->number (string-ref string-id 2))))
      0))

(define prev-segment car)
(define curr-segment cdr)

(define (connected? tcp-in)
  (not (eq? tcp-in -1)))

(define (send-tcp-msg msg out)
  (write msg out)
  (flush-output out))

(define (switch? id)
  (if (symbol? id)
      (eq? (string-ref (symbol->string id) 0) #\S)
      (eq? (string-ref id 0) #\S)))

(define (dtb? id)
  (if (symbol? id)
      (or (eq? (string-ref (symbol->string id) 0) #\1) (eq? (string-ref (symbol->string id) 0) #\2))
      (or (eq? (string-ref id 0) #\1) (eq? (string-ref id 0) #\2))))

(define (mpair->pair mpair)
  (let loop ((lst mpair)
             (res '()))
    (if (null? lst)
        res
        (loop (mcdr lst) (cons (mcar lst) res)))))
