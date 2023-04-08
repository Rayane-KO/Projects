#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Detection-block ADT                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-detection-block)

(define (make-detection-block id)
  (let ((used? #f)
        (last-train 'none)) ;welke trein was het laatst op deze dt-block?

    (define (change-status!)
      (if used?
          (set! used? #f)
          (set! used? #t)))

    (define (last-train! new-train-id)
      (set! last-train new-train-id))

    (define (dispatch m)
      (cond
        ((eq? m 'used?) used?)
        ((eq? m 'change-status!) (change-status!))
        ((eq? m 'id) id)
        ((eq? m 'last-train) last-train)
        ((eq? m 'last-train!) last-train!)
        (else (error "Detection-block-ADT/Message not understood:" m))))
    dispatch))