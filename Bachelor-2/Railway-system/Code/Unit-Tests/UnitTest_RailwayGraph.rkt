#lang racket
(require "../railway-graph.rkt"
         "../helpfunctions.rkt"
         rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      UNIT-TEST: Railway Graph                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;;Shortest-path test
;;
(check-equal? (mpair->pair (railway-shortest-path '2-4 '1-6)) '(2-4 S-20 S-6 S-5 1-6))
;;
;;Neighbours test
;;
(for-each (lambda (node) (check member node '(S-1 S-8 2-2 S-7))) (mpair->pair (railway-neighbours 'S-2-3)))