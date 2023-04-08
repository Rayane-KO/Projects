#lang racket
(require "../ADTs/ADT-railway.rkt"
         "../ADTs/ADT-Train.rkt"
         rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      UNIT-TEST: Railway ADT                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define railway (make-railway))
(define train-id 'T-1)
(define switches '(S-1 S-2 S-3))
(define dtbs '(1-1 1-2 1-3 2-2 2-3))
(define first-train car)
;;
;;Railway test (train)
;;
(define (railway-train)
  (check-equal? (railway 'trains) '())
  ((railway 'add-train!) 'T-1 'S-27 '1-3)
  ;(define test-train (first-train (railway 'trains)))
  (check-equal? ((railway 'train-speed) 'T-1) 0)
  ((railway 'train-speed!) 'T-1 127)
  (check-equal? ((railway 'train-speed) 'T-1) 127)
  (check-equal? ((railway 'train-prev-dtb!) 'T-1 '1-3) (void 0))
  (check-equal? ((railway 'train-direction) 'T-1) 1)
  ((railway 'train-inverse-direction!) 'T-1)
  (check-equal? ((railway 'train-direction) 'T-1) -1))
;;
;;Railway test (detection-block)
;;
(define (railway-dtb)
  (check-equal? (railway 'detection-blocks) '())
  ((railway 'make-detection-blocks!) dtbs)
  (check-equal? ((railway 'dtb-last-train) '2-2) 'none)
  ((railway 'dtb-last-train!) '2-2 'T-1)
  (check-equal? ((railway 'dtb-last-train) '2-2) 'T-1))
;;
;;Railway test (switches)
;;
(define (railway-switch)
  (check-equal? (railway 'switches) '())
  ((railway 'make-switches!) switches)
  (check-equal? ((railway 'switch-position) 'S-2) 1)
  ((railway 'switch-position!) 'S-2 2)
  (check-equal? ((railway 'switch-position) 'S-2) 2))