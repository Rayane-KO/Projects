#lang racket
(require "ADT-Train.rkt")
(require "ADT-Switch.rkt")
(require "ADT-Detection-block.rkt")
(require "../simulator/interface.rkt")
(require "../helpfunctions.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Railway ADT                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-railway)

(define (make-railway)
  (let ((trains '()) ;lijsten die de objecten zullen bijhouden
        (detection-blocks '())
        (switches '()))
    
    (define (find-id id lst)
      (let ((frst (car lst))
            (rest (cdr lst)))
        (cond
          ((null? lst) #f)
          ((eq? id (frst 'id)) frst)
          (else (find-id id rest)))))

    (define (reset!)
      (set! trains '())
      (set! detection-blocks '())
      (set! switches '()))
    ;;
    ;;Trein procedures
    ;;
    (define (add-train! id previous-seg current-seg)
      (let* ((train (make-train id)))
        ((train 'prev-dtb!) current-seg) 
        (set! trains (append trains (list train)))))
           
    (define (train-speed train-id)
      (let ((train (find-id train-id trains)))
        (train 'speed)))

    (define (train-speed! train-id speed)
      (let ((train (find-id train-id trains)))
        ((train 'speed!) speed)))

    (define (train-prev-dtb train-id)
      (let ((train (find-id train-id trains)))
        (train 'prev-dtb)))

    (define (train-prev-dtb! train-id dtb-id)
      (let ((train (find-id train-id trains)))
        ((train 'prev-dtb!) dtb-id)))

    (define (train-direction train-id)
      (let ((train (find-id train-id trains)))
        (train 'direction)))

    (define (train-inverse-direction! train-id)
      (let ((train (find-id train-id trains)))
        (train 'inverse-direction!)
        (train-speed! train-id (- (train-speed train-id)))))

    (define (train-trajectory? train-id)
      (let ((train (find-id train-id trains)))
        (train 'trajectory?)))

    (define (train-trajectory! train-id)
      (let ((train (find-id train-id trains)))
        (train 'trajectory!)))
    ;;
    ;;Detection-block procedures
    ;;
    (define (make-detection-blocks! dtb-ids)
        (set! detection-blocks (map make-detection-block dtb-ids)))

    (define (dtb-last-train! dtb-id train-id)
      (let ((dt-block (find-id dtb-id detection-blocks)))
        ((dt-block 'last-train!) train-id)))

    (define (dtb-last-train dtb-id)
      (let ((dt-block (find-id dtb-id detection-blocks)))
        (dt-block 'last-train)))   
    ;;
    ;;Switch procedures
    ;;
    (define (make-switches! switch-ids)
        (set! switches (map make-switch switch-ids)))

    (define (switch-position switch-id)
      (let ((switch (find-id switch-id switches)))
        (switch 'state)))

    (define (switch-position! switch-id new-state)
      (let ((switch (find-id switch-id switches)))
        ((switch 'state!) new-state)))
    ;;
    ;;Dispatch
    ;;
    (define (dispatch m)
      (cond
        ((eq? m 'trains) trains)
        ((eq? m 'detection-blocks) detection-blocks)
        ((eq? m 'switches) switches)
        ((eq? m 'reset!) (reset!))
        ((eq? m 'add-train!) add-train!)
        ((eq? m 'train-speed) train-speed)
        ((eq? m 'train-speed!) train-speed!)
        ((eq? m 'train-prev-dtb) train-prev-dtb)
        ((eq? m 'train-prev-dtb!) train-prev-dtb!)
        ((eq? m 'train-direction) train-direction)
        ((eq? m 'train-inverse-direction!) train-inverse-direction!)
        ((eq? m 'train-trajectory?) train-trajectory?)
        ((eq? m 'train-trajectory!) train-trajectory!)
        ((eq? m 'make-switches!) make-switches!)
        ((eq? m 'switch-position) switch-position)
        ((eq? m 'switch-position!) switch-position!)
        ((eq? m 'make-detection-blocks!) make-detection-blocks!)
        ((eq? m 'dtb-last-train!) dtb-last-train!)
        ((eq? m 'dtb-last-train) dtb-last-train)
        (else (error "Railway-ADT/Message not understood:" m))))
    dispatch))

        

