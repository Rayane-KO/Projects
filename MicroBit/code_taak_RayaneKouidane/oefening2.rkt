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
;;een naam geven aan de pins vann de drukknoppen
;;
(define right-button 113)
(define left-button 114)
;;
;;deze zijn pull-resistor-up drukknoppen
;;
(gpio-set-pin-mode right-button 'input)
(gpio-set-pin-mode left-button 'input)
(gpio-set-pull-resistor right-button 'up)
(gpio-set-pull-resistor left-button 'up)
;;
;;hulp-procedures
;;
(define (value v i j)
  (vector-ref (vector-ref v i) j))

(define (value! v i j val)
  (vector-set! (vector-ref v i) j val))

(define (column-pins x)
  (vector-ref columns-pins x))

(define (row-pins x)
  (vector-ref rows-pins x))
;;
;;draw-functie die de matrix zal tekenen
;;
(define (draw pattern)
  (for ([column (range 5)])
    (for ([row (range 5)])
      (gpio-digital-write (row-pins row) (value pattern row column)))   
    (gpio-digital-write (column-pins column) 0)
    (gpio-digital-write (column-pins column) 1)))
;;
;;de spelomgeving
;;
(define game-env (vector (vector 0 0 0 0 0)
                         (vector 0 0 0 0 0)
                         (vector 0 0 0 0 0)
                         (vector 0 0 0 0 0)
                         (vector 0 0 0 0 0)))
;;
;;procedure om een led-object aan te maken
;;
(define (make-led row column)
  (value! game-env row column 1)
  
  (define (row! val)
    (set! row val))
  
  (define (column! val)
    (set! column val))
  
  (define (move-led! direction)
    (value! game-env row column 0)
    (cond
      ;;de if zorgt ervoor dat, wanneer de led op een uiteinde staat, het nog steeds verder kan gaan
      ((eq? direction 'right) (if (= column 4)
                                  (column! 0)
                                  (column! (+ column 1))))
      ((eq? direction 'left) (if (= column 0)
                                 (column! 4)
                                 (column! (- column 1)))))
    (value! game-env row column 1))
  
  (define (move! direction)
    (cond
      ((eq? direction 'right) (move-led! 'right)) 
      ((eq? direction 'left) (move-led! 'left)))
    (gpio-delay-ms 150))
  
  (define (dispatch m)
    (cond
      ((eq? m 'move!) move!)))
  dispatch)
;;
;; aangeschakelde led links-rechts te verplaatsen over het display
;;
(define led-1 (make-led 2 2))

(for ([i (range 1000)])
  (cond
    ((= 0 (gpio-digital-read left-button)) ((led-1 'move!) 'left))
    ((= 0 (gpio-digital-read right-button)) ((led-1 'move!) 'right)))
  (draw game-env))


   






