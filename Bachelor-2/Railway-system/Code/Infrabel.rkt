#lang racket
(require "ADTs/ADT-Railway.rkt")
(require "helpfunctions.rkt")
(require "simulator/interface.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Infrabel                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-infrabel)

(define (make-infrabel)
  (let ((railway-infrabel (make-railway))
        (nmbs 'empty)) 
    ;;
    ;;Trein
    ;;
    (define (add-train! previous-seg current-seg)
      (let ((id (make-id)))
        ((railway-infrabel 'add-train!) id previous-seg current-seg)
        (((nmbs 'railway) 'add-train!) id previous-seg current-seg)
        (add-loco id previous-seg current-seg)))

    (define (speed train-id)
      (get-loco-speed train-id))

    (define (speed! train-id new-speed)
      ((railway-infrabel 'train-speed!) train-id new-speed)
      (((nmbs 'railway) 'train-speed!) train-id new-speed)
      (set-loco-speed! train-id new-speed))

    (define (train-dt-block train-id) ;deze procedure gaat de trein (laatste positie) en de detectie-blok (laatste trein) updaten
      (let ((dtb-id (get-loco-detection-block train-id)))
        (cond
          (dtb-id
           ((railway-infrabel 'train-prev-dtb!) train-id dtb-id)
           (((nmbs 'railway) 'train-prev-dtb!) train-id dtb-id)
           ((railway-infrabel 'dtb-last-train!) dtb-id train-id)
           (((nmbs 'railway) 'dtb-last-train!) dtb-id train-id)))
      dtb-id))

    (define (train-direction train-id)
      ((railway-infrabel 'train-direction) train-id))

    (define (train-inverse-direction! train-id)
      ((railway-infrabel 'train-inverse-direction!) train-id)
      (((nmbs 'railway) 'train-inverse-direction!) train-id)
      (set-loco-speed! train-id (- (speed train-id))))

    (define (train-trajectory? train-id)
      ((railway-infrabel 'train-trajectory?) train-id))

    (define (train-trajectory! train-id)
      ((railway-infrabel 'train-trajectory!) train-id)
      (((nmbs 'railway) 'train-trajectory!) train-id))
    ;;
    ;;Detectie-blok
    ;;
    (define (make-dt-blocks!)
      (let ((dt-block-ids (get-detection-block-ids)))
        ((railway-infrabel 'make-detection-blocks!) dt-block-ids)
        (((nmbs 'railway) 'make-detection-blocks!) dt-block-ids)))

    (define (dtb-last-train dtb-id)
      ((railway-infrabel 'dtb-last-train) dtb-id))
    ;;
    ;;update
    ;;
    (define (update) ;voorlopig zorgt update enkel voor het updaten van de vorige dt-block van elke trein
      (let loop ()
        (for-each (lambda (train) (train-dt-block (train 'id))) (railway-infrabel 'trains))
        (loop)))

    (define dtb-update-thread 'empty)

    (define (activate!)
      (set! dtb-update-thread (thread (lambda () (update)))))
    ;;
    ;;Wissels
    ;;
    (define (make-switches!)
      (let ((switch-ids (get-switch-ids)))
        ((railway-infrabel 'make-switches!) switch-ids)
        (((nmbs 'railway) 'make-switches!) switch-ids)))

    (define (switch-position switch-id)
      (get-switch-position switch-id))

    (define (switch-position! switch-id new-state)
      (if (eq? switch-id 'S-2-3) ;driewegwissel is de enige wissel met 3 posities
          (cond
            ((= new-state 1) (set-switch-position! 'S-2 1))
            ((= new-state 2) (set-switch-position! 'S-2 2) (set-switch-position! 'S-3 1))
            ((= new-state 3) (set-switch-position! 'S-2 2) (set-switch-position! 'S-3 2)))
          (set-switch-position! switch-id new-state))
      ((railway-infrabel 'switch-position!) switch-id new-state)
      (((nmbs 'railway) 'switch-position!) switch-id new-state))

    (define (start-railway setup train-list) ;afhankelijk van setup starten we met een andere configuratie van het spoor
      (cond
        ((eq? setup 'setup-hardware) (setup-hardware))
        ((eq? setup 'setup-loop) (setup-loop))
        ((eq? setup 'setup-loop-and-switches) (setup-loop-and-switches))
        ((eq? setup 'setup-straight) (setup-straight))
        ((eq? setup 'setup-straight-with-switch) (setup-straight-with-switch))
        (else (error "Undefined setup: " setup)))
      (for-each (lambda (segments) (add-train! (prev-segment segments) (curr-segment segments))) train-list)
      (make-dt-blocks!)
      (make-switches!)
      (activate!)
      (start))

    (define (stop-railway)
      (railway-infrabel 'reset!)
      (kill-thread dtb-update-thread)
      (stop))

    (define (dispatch m)
      (cond
        ((eq? m 'trains) (railway-infrabel 'trains))
        ((eq? m 'dt-blocks) (railway-infrabel 'detection-blocks))
        ((eq? m 'switches) (railway-infrabel 'switches))
        ((eq? m 'add-train!) add-train!)
        ((eq? m 'speed) speed)
        ((eq? m 'speed!) speed!)
        ((eq? m 'train-dt-block) train-dt-block)
        ((eq? m 'train-direction) train-direction)
        ((eq? m 'train-inverse-direction!) train-inverse-direction!)
        ((eq? m 'train-trajectory?) train-trajectory?)
        ((eq? m 'train-trajectory!) train-trajectory!)
        ((eq? m 'dtb-last-train) dtb-last-train)
        ((eq? m 'switch-position) switch-position)
        ((eq? m 'switch-position!) switch-position!)
        ((eq? m 'start) start-railway)
        ((eq? m 'stop) (stop-railway))
        ((eq? m 'add-nmbs!) (lambda (new) (set! nmbs new)))))
    dispatch))
    