#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Switch ADT                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-switch)

(define (make-switch id)
  (let ((state 1))

    (define (state! new-state)
      (set! state new-state))

    (define (dispatch m)
      (cond
        ((eq? m 'id) id)
        ((eq? m 'state) state)
        ((eq? m 'state!) state!)
        (else (error "Switch-ADT/Message not understood:" m))))
    dispatch))