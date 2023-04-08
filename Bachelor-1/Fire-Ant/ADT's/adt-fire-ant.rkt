;;;--------------------FIRE-ANT ADT--------------------;;;

(load "adt-position.rkt")
(load "constants.rkt")
(define (make-fire-ant position)
  ;;
  ;;mijn fire-ant zal ook zijn eigen richting en vorige richting bijhouden
  ;;
  (let ((direction 'down)
        (previous-direction 'down))
    ;;
    ;;procedure die de richting zal wijzigen
    ;;
    (define (direction! new-direction)
      (set! direction new-direction))   

    (define (prev-direction! new-previous-direction)
      (set! previous-direction new-previous-direction))
    ;;
    ;;procedure om mijn mier te kunnen bewegen, kijkt ook of de mier tegen een muur botst of niet
    ;;
    (define (move-ant! dir maze)
      (move-maze! position dir direction! maze))
    ;;
    ;;procedure dat controleert of de mier een ander object (met een eigen hoogte en breedte) elkaar raken of niet
    ;;
    (define (hit? object object-width object-height)                      
      (let* ((object-position (object 'position))
             (left-corner-x (position 'x))                                            
             (left-corner-y (position 'y))                                        
             (left-corner-x-b (object-position 'x))                                  
             (left-corner-y-b (object-position 'y))                                  
             (right-corner-x (+ (position 'x) ant-width))                                         
             (right-corner-y (position 'y))
             (right-corner-x-b (+ (object-position 'x) object-width))
             (right-corner-y-b (object-position 'y))
             (left-corner-down-x (position 'x))
             (left-corner-down-y (+ (position 'y) ant-height))
             (left-corner-down-x-b (object-position 'x))
             (left-corner-down-y-b (+ (object-position 'y) object-height)))
        
        (and (or (< left-corner-x left-corner-x-b right-corner-x)
                 (< left-corner-x right-corner-x-b right-corner-x)
                 (and (= left-corner-x left-corner-x-b) (= right-corner-x right-corner-x-b)))
             (or (< left-corner-y left-corner-y-b left-corner-down-y)
                 (< left-corner-y left-corner-down-y-b left-corner-down-y)
                 (and (= left-corner-y left-corner-y-b) (= left-corner-down-y left-corner-down-y-b))))))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-fire-ant m)
      (cond
        ((eq? m 'position) position)
        ((eq? m 'direction) direction)
        ((eq? m 'previous-direction) previous-direction)
        ((eq? m 'direction!) direction!)
        ((eq? m 'prev-direction!) prev-direction!)
        ((eq? m 'move-ant!) move-ant!)
        ((eq? m 'hit?) hit?)
        (else (display "MESSAGE NOT UNDERSTOOD (fire-ant-adt)"))))
    dispatch-fire-ant))
