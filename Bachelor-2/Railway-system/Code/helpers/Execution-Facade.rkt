#lang racket
(require (prefix-in sm: "../simulator/interface.rkt"))
(require (prefix-in hw: "../hardware-library/interface.rkt"))
(require "railway-graph.rkt")
(require "constants.rkt")
(require "helpfunctions.rkt")

(provide sim?
         switch-option!
         start
         stop
         add-loco
         get-loco-speed
         set-loco-speed!
         get-loco-detection-block
         get-switch-position
         set-switch-position!
         get-detection-block-ids
         get-switch-ids
         setup-hardware
         setup-loop
         setup-loop-and-switches
         setup-straight
         setup-straight-with-switch
         get-occupied-detection-blocks)

(define sim? #t)
(define (switch-option! option)
  (set! sim? option))

(define (choose-interface sm-proc hw-proc)
  (if sim?
      (sm-proc)
      (hw-proc)))

(define (start)
  (choose-interface (lambda () (sm:start)) (lambda () (hw:start))))

(define (stop)
  (choose-interface (lambda () (sm:stop)) (lambda () (hw:stop))))

(define (add-loco id prev-seg curr-seg)
  (choose-interface (lambda () (sm:add-loco id prev-seg curr-seg)) (lambda () (hw:add-loco id prev-seg curr-seg))))

(define (get-loco-speed id)
  (choose-interface (lambda () (sm:get-loco-speed id)) (lambda () (hw:get-loco-speed id))))

(define (set-loco-speed! id speed)
  (choose-interface (lambda () (sm:set-loco-speed! id speed)) (lambda () (hw:set-loco-speed! id speed))))

(define (hardware-get-detection id)
  (let ((occupied-detections (hw:get-occupied-detection-blocks)))
    occupied-detections))

(define (dtb? id)
  (if (symbol? id)
      (or (eq? (string-ref (symbol->string id) 0) #\1) (eq? (string-ref (symbol->string id) 0) #\2))
      (or (eq? (string-ref id 0) #\1) (eq? (string-ref id 0) #\2))))

(define (find-id id lst)
  (let ((frst (car lst))
        (rest (cdr lst)))
    (cond
      ((null? lst) #f)
      ((eq? id (frst 'id)) frst)
      (else (find-id id rest)))))

(define (get-detection-block id railway)
  (let* ((occupied-dtbs (hw:get-occupied-detection-blocks))
         (last-dtb ((railway 'train-prev-dtb) id))
         (current-dtb ((railway 'train-curr-dtb) id))
         (neighbours (mpair->pair (railway-neighbours last-dtb)))
         (res '()))
    (let loop ((ngbrs neighbours))
      (when (not (null? ngbrs))
        (if (member (car ngbrs) occupied-dtbs)
            (set! res (car ngbrs))
            (loop (cdr ngbrs)))))
    res))

(define (get-occupied-detection-blocks)
  (hw:get-occupied-detection-blocks))
        
(define (get-loco-detection-block id railway)
  (choose-interface (lambda () (sm:get-loco-detection-block id)) (lambda () (get-detection-block id railway))))

(define (get-switch-position id)
  (choose-interface (lambda () (sm:get-switch-position id)) (lambda () (hw:get-switch-position id))))

(define (set-switch-position! id position)
  (choose-interface (lambda () (sm:set-switch-position! id position)) (lambda () (hw:set-switch-position! id position))))

(define (get-detection-block-ids)
  (sm:get-detection-block-ids))

(define (get-switch-ids)
  (sm:get-switch-ids))

(define (setup-hardware)
  (sm:setup-hardware))

(define (setup-loop)
  (sm:setup-loop))

(define (setup-loop-and-switches)
  (sm:setup-loop-and-switches))

(define (setup-straight)
  (sm:setup-straight))

(define (setup-straight-with-switch)
  (sm:setup-straight-with-switch))