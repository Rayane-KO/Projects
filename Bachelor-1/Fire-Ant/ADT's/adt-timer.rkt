;;;--------------------TIMER ADT--------------------;;;

(define (make-timer seconds)
  (let ((current-time 0)
        (milliseconds (* seconds 1000)))
    ;;
    ;;gaat de timer updaten met de delta-tijd
    ;;
    (define (update! time)
      (set! current-time (+ current-time time)))
    ;;
    ;;zet de timer weer op 0
    ;;
    (define (reset!)
      (set! current-time 0))
    ;;
    ;;kijkt of de timer aan het updaten is
    ;;
    (define (active?)
      (and (< current-time milliseconds) (not (zero? current-time))))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-timer m)
      (cond
        ((eq? m 'update!) update!)
        ((eq? m 'current-time) current-time)
        ((eq? m 'reset!) (reset!))
        ((eq? m 'active?) (active?))
        (else (display "MESSAGE NOT UNDERSTOOD (Timer ADT)"))))
    dispatch-timer))