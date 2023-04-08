#lang racket
;constanten

(provide max-speed
         zero-speed
         slow-speed
         special-track-timer)

(define max-speed 127)
(define zero-speed 0)
(define slow-speed (quotient max-speed 2))

(define special-track-timer 20.0)

(define sim? #t)

