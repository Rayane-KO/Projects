;;;--------------------KEY ADT--------------------;;;
(load "adt-position.rkt")
(define (make-key key-position door-position)
  (let ((collected? #f))
    ;;
    ;;collect!
    ;;
    (define (collect!)
      (set! collected? #t))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-key m)
      (cond
        ((eq? m 'collected?) collected?)
        ((eq? m 'collect!) (collect!))
        ((eq? m 'position) key-position)
        ((eq? m 'door-position) door-position)
        (else (display "MESSAGE NOT UNDERSTOOD (Key ADT)"))))
    dispatch-key))