#lang racket
(require "../ADTs/ADT-Train.rkt"
         "../ADTs/ADT-Detection-block.rkt"
         "../ADTs/ADT-Switch.rkt"
         rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        UNIT-TEST: Train, Detection-block, Switch ADT          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define train (make-train 'T-1))
(define dt-block (make-detection-block '1-3))
(define switch (make-switch 'S-27))
;;
;;Train tests
;;
(define (train-test)
  (check-equal? (train 'id) 'T-1 "Get id")
  (check-equal? (train 'speed) 0 "Get speed")
  ((train 'speed!) 100)
  (check-equal? (train 'speed) 100 "Recheck speed")
  (check-equal? (train 'direction) 1 "Get direction")
  (train 'inverse-direction!)
  (check-equal? (train 'direction) -1 "Get direction")
  ((train 'prev-dtb!) '1-3)
  (check-equal? (train 'prev-dtb) '1-3 "Get last dtb"))
;;
;;Detection-block tests
;;
(define (dt-block-test)
  (check-equal? (dt-block 'id) '1-3)
  (check-equal? (dt-block 'used?) #f)
  (dt-block 'change-status!)
  (check-equal? (dt-block 'used?) #t)
  (check-equal? (dt-block 'last-train) 'none)
  ((dt-block 'last-train!) 'T-1)
  (check-equal? (dt-block 'last-train) 'T-1))
;;
;;Switch tests
;;
(define (switch-test)
  (check-equal? (switch 'id) 'S-27)
  (check-equal? (switch 'state) 1)
  ((switch 'state!) 2)
  (check-equal? (switch 'state) 2))









