#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Constants                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(define max-speed 127)
(define zero-speed 0)
(define slow-speed (quotient max-speed 2))

(define tcp-time 0.2)
(define wait-time 0.1)
(define loop-time 0.05)

(define empty -1)
(define tcp-port 9883)

