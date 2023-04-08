#lang racket 
(require "raspi-gpio.rkt")
(gpio-setup) 
(gpio-mcp23017-setup 100 #x21)
;;
;;naam geven aan de pins van de kolommen
;;
(define column1 108)
(define column2 109)
(define column3 110)
(define column4 111)
(define column5 112)
;;
;;naam geven aan de pins van de rijen
;;
(define row1 103)
(define row2 104)
(define row3 105)
(define row4 106)
(define row5 107)
;;
;;beiden in een vector steken
;;
(define columns-pins (vector column1 column2 column3 column4 column5))
(define rows-pins (vector row1 row2 row3 row4 row5))
(define column-list (vector->list columns-pins))
(define rows-list (vector->list rows-pins))
;;
;;alle led's op output modus zetten
;;
(for-each (lambda (pin) (gpio-set-pin-mode pin 'output)) column-list)
(for-each (lambda (pin) (gpio-set-pin-mode pin 'output)) rows-list)
;;
;;zorgt ervoor dat alle led's uit staan
;;
(for-each (lambda (pin) (gpio-digital-write pin 0)) column-list)
(for-each (lambda (pin) (gpio-digital-write pin 1)) rows-list)
;;
;;de 3 patronen
;;
(define pattern-1 (vector (vector 1 0 0 0 1)
                          (vector 0 1 0 1 0)
                          (vector 0 0 1 0 0)
                          (vector 0 1 0 1 0)
                          (vector 1 0 0 0 1)))

(define pattern-2 (vector (vector 0 0 1 0 0)
                          (vector 0 1 0 1 0)
                          (vector 1 0 0 0 1)
                          (vector 0 1 0 1 0)
                          (vector 0 0 1 0 0)))

(define pattern-3 (vector (vector 0 1 0 1 0)
                          (vector 1 0 1 0 1)
                          (vector 1 0 0 0 1)
                          (vector 0 1 0 1 0)
                          (vector 0 0 1 0 0)))
;;
;;hulp-procedures
;;
(define (column-pins x)
  (vector-ref columns-pins x))

(define (row-pins x)
  (vector-ref rows-pins x))

(define (value v i j)
  (vector-ref (vector-ref v i) j))
;;
;;draw-functie die de matrix zal tekenen
;;
(define (draw pattern delay)
  (for ([column (range 5)])
    (for ([row (range 5)])
      (gpio-digital-write (row-pins row) (value pattern row column)))   
    (gpio-digital-write (column-pins column) 0)
    (gpio-delay-ms delay)
    (gpio-digital-write (column-pins column) 1)))
;;
;;demonstratie van de 3 patronen
;;
(define draw-time 500)

(for ([i (range draw-time)])
(draw pattern-1 0))
(for ([i (range draw-time)])
(draw pattern-2 0))
(for ([i (range draw-time)])
(draw pattern-3 0))

