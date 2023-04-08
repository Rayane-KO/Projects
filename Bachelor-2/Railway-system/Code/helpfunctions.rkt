#lang racket
(require racket/date)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Helpfunctions                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-id
         get-time
         char->number
         prev-segment
         curr-segment)

(define id 1)
(define (make-id)
  (let* ((number-str (number->string id))
        (id-str (string-append "T-" number-str)))
    (set! id (+ id 1))
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

(define prev-segment car)
(define curr-segment cdr)
