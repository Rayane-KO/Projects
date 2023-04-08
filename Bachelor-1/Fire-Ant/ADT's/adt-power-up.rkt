(define (make-power-up position type . timer)
  (let ((active? #f) ;;variabele die gaat kijken of een power-up geactiveerd is of niet
        (collected? #f));;variabele die gaat kijken of een power-up opgerapen werd
    ;;
    ;;power activeren
    ;;
    (define (activate!)
      (set! active? #t))
    ;;
    ;;power-up uitschakelen
    ;;
    (define (disable!)
      (set! active? #f))
    ;;
    ;;power-up oprapen
    ;;
    (define (collect!)
      (set! collected? #t))
    ;;
    ;;toont of een power-up geactiveerd werd
    ;;
    (define activated? #f)
    ;;
    ;;power-up 1 keer activeren
    ;;
    (define (activate-one-time!)
      (if (not activated?)
          (begin
            (activate!)
            (set! activated? #t))
          (disable!)))
    ;;
    ;;activeert de power-up en schakelt die uit op het einde van de timer
    ;;
    (define (start-timer! time) 
      (if (not (null? timer))
          (let ((r-timer (car timer)))
            ((r-timer 'update!) time)
            (if (r-timer 'active?)
                (begin (activate!)
                       (set! activated? #t))
                (disable!)))))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-power-up m)
      (cond
        ((eq? m 'position) position)
        ((eq? m 'type) type)
        ((eq? m 'active?) active?)
        ((eq? m 'activate!) (activate!))
        ((eq? m 'activated?) activated?)
        ((eq? m 'activate-one-time!) (activate-one-time!))
        ((eq? m 'disable!) (disable!))
        ((eq? m 'collected?) collected?)
        ((eq? m 'collect!) (collect!))
        ((eq? m 'start-timer!) start-timer!)
        (else (display "MESSAGE NOT UNDERSTOOD (power-up adt)"))))
    dispatch-power-up))