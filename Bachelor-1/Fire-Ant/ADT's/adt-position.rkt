;;;--------------------POSITIE ADT--------------------;;;

(load "constants.rkt")
(define (make-position x y)
  ;;
  ;;procedures om de positie te wijzigen
  ;;
  (define (x! new-x) (set! x new-x))
  (define (y! new-y) (set! y new-y))
  ;;
  ;;kijkt of 2 posities gelijk zijn
  ;;
  (define (same? position)
    (and (= x (position 'x))
         (= y (position 'y))))
  ;;
  ;;beweegt volgens een symbool met een gegeven waarde
  ;;
  (define (move! direction value)
    (cond
      ((eq? direction 'up) (y! (- y value)))
      ((eq? direction 'down) (y! (+ y value)))
      ((eq? direction 'left) (x! (- x value)))
      ((eq? direction 'right) (x! (+ x value)))
      (else (display "give a direction"))))
  ;;
  ;;dispatch
  ;;
  (define (dispatch-position m)
    (cond
      ((eq? m 'x) x)
      ((eq? m 'y) y)
      ((eq? m 'x!) x!)
      ((eq? m 'y!) y!)
      ((eq? m 'same?) same?)
      ((eq? m 'move!) move!)
      (else (display "MESSAGE NOT UNDERSTOOD (position-adt)"))))
  dispatch-position)