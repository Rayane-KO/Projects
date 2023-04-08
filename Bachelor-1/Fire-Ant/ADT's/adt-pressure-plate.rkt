(define (make-pressure-plate position door-position block-position)
  ;;
  ;;procedure om het blok te laten bewegen
  ;;
  (define (move! fire-ant-adt maze)
    (let ((ant-position (fire-ant-adt 'position))
          (ant-direction (fire-ant-adt 'direction))
          (direction! (fire-ant-adt 'direction!)))
      (move-maze! block-position ant-direction direction! maze ant-position)))
  ;;
  ;;kijkt of het blok op de drukplaat staat
  ;;
  (define (pressed?)
    ((position 'same?) block-position))
  ;;
  ;;dispatch
  ;;
  (define (dispatch-pp m)
    (cond
      ((eq? m 'position) position)
      ((eq? m 'door-position) door-position)
      ((eq? m 'block-position) block-position)
      ((eq? m 'move!) move!)
      ((eq? m 'pressed?) (pressed?))
      (else (display "MESSAGE UNDERSTOOD (pressure-plate adt)"))))
  dispatch-pp)