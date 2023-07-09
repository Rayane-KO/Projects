#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Train ADT                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-train)

(define (make-train id)
  (let ((speed 0)
        (trajectory '()) ;voorstelling van het traject
        (direction #t) ;de richting #t en tegengestelde richting is #f
        (curr-dtb 'none) ;wat is de huidige positie
        (prev-dtb 'none)) ;wat is de vorige positie

    (define (speed! new-speed)
      (set! speed new-speed))

    (define (curr-dtb! new-dtb)
      (when (not (eq? new-dtb curr-dtb))
        (set! prev-dtb curr-dtb)
        (set! curr-dtb new-dtb)))

    (define (inverse-direction!)
      (set! direction (not direction)))

    (define (trajectory?) ;is de trein een traject aan het volgen?
      (not (null? trajectory)))

    (define (trajectory! new-trajectory)
      (if trajectory?
          (set! trajectory new-trajectory)
          (set! trajectory '())))

    (define (dispatch m)
      (cond
        ((eq? m 'id) id)
        ((eq? m 'speed) speed)
        ((eq? m 'speed!) speed!)
        ((eq? m 'direction) direction)
        ((eq? m 'inverse-direction!) (inverse-direction!))
        ((eq? m 'prev-dtb) prev-dtb)
        ((eq? m 'curr-dtb) curr-dtb)
        ((eq? m 'curr-dtb!) curr-dtb!)
        ((eq? m 'trajectory) trajectory)
        ((eq? m 'trajectory?) (trajectory?))
        ((eq? m 'trajectory!) trajectory!)
        (else (error "Train-ADT/Message not understood:" m))))
    dispatch))