 #lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Train ADT                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-train)

(define (make-train id)
  (let ((speed 0)
        (trajectory? #f) ;is de trein een traject aan het volgen?
        (direction #t) ;de richting #t en tegengestelde richting is #f
        (prev-dtb 'none)) ;welke dt-block was de trein het laatst op?

    (define (speed! new-speed)
      (set! speed new-speed))

    (define (prev-dtb! current-dtb)
      (set! prev-dtb current-dtb))

    (define (inverse-direction!)
      (set! direction (not direction)))

    (define (trajectory!)
      (set! trajectory? (not trajectory?)))

    (define (dispatch m)
      (cond
        ((eq? m 'id) id)
        ((eq? m 'speed) speed)
        ((eq? m 'speed!) speed!)
        ((eq? m 'direction) direction)
        ((eq? m 'inverse-direction!) (inverse-direction!))
        ((eq? m 'prev-dtb) prev-dtb)
        ((eq? m 'prev-dtb!) prev-dtb!)
        ((eq? m 'trajectory?) trajectory?)
        ((eq? m 'trajectory!) (trajectory!))
        (else (error "Train-ADT/Message not understood:" m))))
    dispatch))