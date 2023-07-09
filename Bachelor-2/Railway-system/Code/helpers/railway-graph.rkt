#lang racket
(require "../a-d/graph/labeled/config.rkt"
         "../a-d/graph-algorithms/undirected/bft-applications.rkt"
         "helpfunctions.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Railway-Graph                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide railway-shortest-path get-states cons-high-low railway-neighbours all-nodes node-ref node-set!)

(define dt-blocks '(1-1 1-2 1-3 1-4 1-5 1-6 1-7 1-8 2-1 2-2 2-3 2-4 2-5 2-6 2-7 2-8))
(define switches '(S-1 S-2-3 S-4 S-5 S-6 S-7 S-8 S-9 S-10 S-11 S-12 S-16 S-20 S-23 S-24 S-25 S-26 S-27 S-28))
;;
;;procedures voor de graf
;;
(define node-labels (make-hash))
(define (node-number id)
  (hash-ref node-labels id))

(define (railway-shortest-path from to)
  (mpair->pair (shortest-path graph (node-number from) (node-number to))))

(define (railway-neighbours node)
  (mpair->pair (g-neighbours graph (node-number node))))

(define (get-states from to)
  (edge-label graph (node-number from) (node-number to)))

(define (cons-high-low from-node to-node)     ;deze procedure wordt gebruikt om in de car de node te zetten met de hoogste index
  (let ((from-number (node-number from-node)) ;en in de cdr deze met de laagste
        (to-number (node-number to-node)))
    (if (< from-number to-number)
        (cons to-node from-node)
        (cons from-node to-node))))

(define all-nodes 'empty)

(define (node-ref v node)
  (vector-ref v (node-number node)))

(define (node-set! v node el)
  (vector-set! v (node-number node) el))
;;
;;graf
;;
(define graph
  (let* ((all (append dt-blocks switches))
         (g (new #f (length all)))
         (node-ctr 0))
    (set! all-nodes (make-vector (length all) #f))
    (define (insert-node! node)
      (begin
        (label! g node-ctr node)
        (set! node-ctr (+ node-ctr 1))))
    (for-each (lambda (el) (hash-set! node-labels el node-ctr) (insert-node! el)) all)
    ;;label van een boog is een cijfer of een cons-cel
    (add-edge! g (node-number '2-1) (node-number 'S-1) 1)
    (add-edge! g (node-number '1-8) (node-number 'S-25) 1)
    (add-edge! g (node-number 'S-25) (node-number 'S-1) (cons 2 2)) 
    (add-edge! g (node-number 'S-1) (node-number 'S-2-3) (cons 1 1))
    (add-edge! g (node-number 'S-25) (node-number 'S-7) (cons 1 1))
    (add-edge! g (node-number 'S-7) (node-number 'S-2-3) (cons 2 1))
    (add-edge! g (node-number 'S-2-3) (node-number 'S-8) (cons 1 3))
    (add-edge! g (node-number 'S-2-3) (node-number '2-2) 2)
    (add-edge! g (node-number 'S-8) (node-number 'S-4) (cons 2 2))
    (add-edge! g (node-number 'S-8) (node-number '2-5) 1)
    (add-edge! g (node-number 'S-4) (node-number '2-6) 1)
    (add-edge! g (node-number 'S-4) (node-number '2-7) 2)
    (add-edge! g (node-number 'S-7) (node-number 'S-5) (cons 1 2))
    (add-edge! g (node-number 'S-5) (node-number 'S-6) (cons 1 1))
    (add-edge! g (node-number 'S-5) (node-number '1-6) 1)
    (add-edge! g (node-number '1-6) (node-number '1-7) 1)
    (add-edge! g (node-number 'S-6) (node-number 'S-20) (cons 2 2))
    (add-edge! g (node-number 'S-20) (node-number '1-5) 1)
    (add-edge! g (node-number '1-5) (node-number '1-4) 1)
    (add-edge! g (node-number '1-4) (node-number 'S-26) 1)
    (add-edge! g (node-number 'S-26) (node-number 'S-28) (cons 2 1))
    (add-edge! g (node-number '1-7) (node-number 'S-28) 1)
    (add-edge! g (node-number 'S-26) (node-number 'S-27) (cons 1 2))
    (add-edge! g (node-number 'S-28) (node-number '1-1) 1)
    (add-edge! g (node-number 'S-27) (node-number '1-2) 2)
    (add-edge! g (node-number 'S-27) (node-number '1-3) 1)
    (add-edge! g (node-number '1-1) (node-number 'S-10) 1)
    (add-edge! g (node-number '1-2) (node-number 'S-9) 1)
    (add-edge! g (node-number '1-3) (node-number 'S-24) 2)
    (add-edge! g (node-number 'S-10) (node-number 'S-16) (cons 1 2))
    (add-edge! g (node-number 'S-16) (node-number '2-8) 2)
    (add-edge! g (node-number 'S-9) (node-number 'S-11) (cons 1 2))
    (add-edge! g (node-number 'S-10) (node-number 'S-11) (cons 2 1))
    (add-edge! g (node-number 'S-11) (node-number 'S-12) (cons 1 1))
    (add-edge! g (node-number 'S-9) (node-number 'S-24) (cons 1 1))
    (add-edge! g (node-number 'S-23) (node-number 'S-24) (cons 1 1))
    (add-edge! g (node-number 'S-12) (node-number 'S-23) (cons 2 1))
    (add-edge! g (node-number 'S-12) (node-number '2-3) 2)
    (add-edge! g (node-number 'S-23) (node-number '2-4) 1)
    (add-edge! g (node-number '2-3) (node-number 'S-6) 1)
    (add-edge! g (node-number '2-4) (node-number 'S-20) 1)
    g))