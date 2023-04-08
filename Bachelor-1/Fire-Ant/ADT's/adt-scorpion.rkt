;;;--------------------SCORPION ADT--------------------;;;

(load "adt-position.rkt")
(load "constants.rkt")
(define (make-scorpion position type)
  (let ((direction 'down)
        (previous-direction 'down)
        (opposite? #f)
        (switched-to-fast? #f)
        (switched-to-normal? #t))
    ;;
    ;;controleert of een schorpioen sneller gaat en voert een meegegeven procedure uit
    ;;
    (define (speed-test proc)
      (if (= scorpion-speed normal-speed)
          (if (not switched-to-normal?)
              (begin
                (proc)
                (set! switched-to-normal? #t)
                (set! switched-to-fast? #f)))
          (if (not switched-to-fast?)
              (begin
                (proc)
                (set! switched-to-fast? #t)
                (set! switched-to-normal? #f)))))
    ;;
    ;;
    ;;
    (define (switch!)
      (if opposite?
          (set! opposite? #f)
          (set! opposite? #t)))
    ;;
    ;;
    ;;
    (define (direction! new-direction)
      (set! direction new-direction))
    ;;
    ;;
    ;;
    (define (prev-direction! new-previous-direction)
      (set! previous-direction new-previous-direction))
    ;;
    ;;procedure dat ervoor zal zorgen dat een schorpioen alleen beweegt
    ;;
    (define scorpion-time 0)
    
    (define (follow! direction time)
      (if (> scorpion-time scorpion-speed)
          (begin
            ((position 'move!) direction step)
            (set! scorpion-time 0))
          (set! scorpion-time (+ scorpion-time time))))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-scorpion m)
      (cond
        ((eq? m 'position) position)
        ((eq? m 'direction) direction)
        ((eq? m 'previous-direction) previous-direction)
        ((eq? m 'prev-direction!) prev-direction!)
        ((eq? m 'direction!) direction!)
        ((eq? m 'follow!) follow!)
        ((eq? m 'opposite?) opposite?)
        ((eq? m 'switch!) (switch!))
        ((eq? m 'type) type)
        ((eq? m 'switched-to-fast?) switched-to-fast?)
        ((eq? m 'speed-test) speed-test)
        (else (display "MESSAGE NOT UNDERSTOOD (scorpion-adt)"))))
    dispatch-scorpion))