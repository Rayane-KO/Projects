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
  (let ((collected? #f))
    (value! game-env row column 1)

    (define (row! val)
      (set! row val))

    (define (column! val)
      (set! column val))

    (define (position! row-val col-val)
      (row! row-val)
      (column! col-val))

    (define (same-position? led)
      (let ((led-row (led 'row))
            (led-col (led 'column)))
        (and (= row led-row)
             (= column led-col))))

    (define (switch! val)
      (value! game-env row column val))

    (define (move-led! direction)
      (cond
        ((eq? direction 'left) (switch! 0) (if (= column 0)
                                               (position! row 4)
                                               (position! row (- column 1))))
        ((eq? direction 'right) (switch! 0) (if (= column 4)
                                                (position! row 0)
                                                (position! row (+ column 1))))
        ((eq? direction 'down) (switch! 0) (if (= row 4)
                                               (position! 0 (random 5))
                                               (position! (+ row 1) column))))
      (switch! 1))

    (define (move! direction)
      (cond
        ((eq? direction 'left) (move-led! 'left))
        ((eq? direction 'right) (move-led! 'right)))
      (gpio-delay-ms 150))

    (define (fall!)
      (move-led! 'down))

    (define (collect!)
      (set! collected? #t))

    (define (uncollect!)
      (set! collected? #f))

    (define (lost? led)
      (and (= (led 'row) 4) (not (same-position? led))))

    (define (touched? led)
      (and (same-position? led) (not (led'collected?))))

    (define (led-reset?)
      (= row 0))
       
    (define (dispatch m)
      (cond
        ((eq? m 'row) row)
        ((eq? m 'column) column)
        ((eq? m 'move!) move!)
        ((eq? m 'fall!) (fall!))
        ((eq? m 'collected?) collected?)
        ((eq? m 'collect!) (collect!))
        ((eq? m 'uncollect!) (uncollect!))
        ((eq? m 'lost?) lost?)
        ((eq? m 'touched?) touched?)
        ((eq? m 'reset?) (led-reset?))
        ((eq? m 'same-position?) same-position?)))
    dispatch))
;;
;;spel implementeren
;;
(define player (make-led 4 2))
(define snowflake (make-led 0 (random 5)))

(define score 0)
(define lost-counter 0)

(define show-score? #f)
(define lost-var? #t)

(define interval 100)

(define (set-position!)
  (value! game-env (player 'row) (player 'column) 1))

(for ([i (range 3000)])
  (cond ((< lost-counter 3)
         (cond ((and (zero? (modulo i interval)) (> i 0)) (snowflake 'fall!))
               ((= 0 (gpio-digital-read left-button)) ((player 'move!) 'left))
               ((= 0 (gpio-digital-read right-button)) ((player 'move!) 'right))
               (((player 'touched?) snowflake) (set! score (+ score 1))
                                               (snowflake 'collect!))
               ((and ((player 'lost?) snowflake) lost-var?) (set! lost-counter (+ lost-counter 1))
                                                            (set! lost-var? #f))
               ((snowflake 'reset?) (set! lost-var? #t)
                                    (snowflake 'uncollect!)
                                    (set-position!)))
         (draw game-env))    
        ((= lost-counter 3)
         (set! show-score? #t)
         (set! lost-counter 4)))
  (cond
    (show-score? (displayln (string-append "Your score: " (number->string score)))
                 (set! show-score? #f)))
  (cond
    ((and (= (modulo i 400) 0) (> i 0)) (set! interval (round (/ interval 2))))))