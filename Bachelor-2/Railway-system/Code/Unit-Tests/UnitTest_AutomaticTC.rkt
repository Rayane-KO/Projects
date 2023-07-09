#lang racket
(require "../railway-graph.rkt"
         "../helpfunctions.rkt"
         "../a-d/scheme-tools.rkt"
         rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             UNIT-TEST: Automatic traject calculation          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (calculate-trajectory train-id start destination)
      (let* ((trajectory (railway-shortest-path start destination))
             (path (list (append (list train-id) trajectory))))
        path))

(define random-start (vector '1-1 '2-4 '2-5 '1-8 '1-6))
(define random-end (vector '1-8 '2-3 '1-5 '1-3 '2-2))

(define start (vector-ref random-start (random-integer (vector-length random-start))))
(define end (vector-ref random-end (random-integer (vector-length random-end))))

(calculate-trajectory 'T-3 start end)