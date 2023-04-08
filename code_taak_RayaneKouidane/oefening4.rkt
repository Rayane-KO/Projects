#lang racket
(require "raspi-gpio.rkt")
(define X_CH 0)
(define Y_CH 1)
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
;;setup van de accelerometer
;;
(define spi-channel 0)
(define channel-config-single 8)
(define channel-config-diff 0)
(define channel-config channel-config-single)
(gpio-mcp3008-setup spi-channel)
(define sample-size 10)
(define (read-axis ch)
  (let ((reading 0))
    (gpio-mcp3008-analog-read spi-channel channel-config ch)
    (gpio-delay-ms 1)
    (for ([i sample-size])
      (set! reading 
            (+ reading (gpio-mcp3008-analog-read spi-channel channel-config ch))))
    (/ reading (exact->inexact sample-size))))

(define x-raw-min 512)
(define y-raw-min 512)
(define x-raw-max 512)
(define y-raw-max 512)
(define (auto-calibrate x-raw y-raw)
  (cond ((< x-raw x-raw-min) (set! x-raw-min x-raw)))
  (cond ((> x-raw x-raw-max) (set! x-raw-max x-raw)))
  (cond ((< y-raw y-raw-min) (set! y-raw-min y-raw)))
  (cond ((> y-raw y-raw-max) (set! y-raw-max y-raw))))

(define (calibrate-loop)
  (let ((x-raw (read-axis X_CH))
        (y-raw (read-axis Y_CH)))
    (for ([i 400])
      (set! x-raw (read-axis X_CH))
      (set! y-raw (read-axis Y_CH))
      (auto-calibrate x-raw y-raw)
      (gpio-delay-ms 50))))
(calibrate-loop)
(define (value-map x in-min in-max out-min out-max)
  (/ (* (- x in-min) (- out-max out-min))
     (+ (- in-max in-min) out-min)))
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
      (begin
        (row! row-val)
        (column! col-val)))

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
                                               (position! row 0)
                                               (position! row (- column 1))))
        ((eq? direction 'right) (switch! 0) (if (= column 4)
                                                (position! row 4)
                                                (position! row (+ column 1))))
        ((eq? direction 'down) (switch! 0) (if (= row 4)
                                               (position! 4 column)
                                               (position! (+ row 1) column)))
        ((eq? direction 'up) (switch! 0) (if (= row 0)
                                             (position! 0 column)
                                             (position! (- row 1) column)))
        ((eq? direction 'random) (switch! 0) (position! (random 5) (random 5))))
      (switch! 1))

    (define (move! direction)
      (cond
        ((eq? direction 'left) (move-led! 'left))
        ((eq? direction 'right) (move-led! 'right))
        ((eq? direction 'up) (move-led! 'up))
        ((eq? direction 'down) (move-led! 'down))
        ((eq? direction 'random) (move-led! 'random)))
      (gpio-delay-ms 150))

    (define (collect!)
      (set! collected? #t))

    (define (uncollect!)
      (set! collected? #f))

    (define (touched? led)
      (and (same-position? led) (not (led'collected?))))
    
    (define (dispatch m)
      (cond
        ((eq? m 'row) row)
        ((eq? m 'column) column)
        ((eq? m 'move!) move!)
        ((eq? m 'collected?) collected?)
        ((eq? m 'collect!) (collect!))
        ((eq? m 'uncollect!) (uncollect!))
        ((eq? m 'touched?) touched?)
        ((eq? m 'same-position?) same-position?)))
    dispatch))
;;
;;
;;
(define (make-axis-controller)
  (let* ((x-raw (read-axis X_CH))
         (y-raw (read-axis Y_CH))
         (x-scaled (value-map x-raw x-raw-min x-raw-max -1000 1000))
         (y-scaled (value-map y-raw y-raw-min y-raw-max -1000 1000)))
    (define (dispatch m)
      (cond
        ((eq? m 'x) x-scaled)
        ((eq? m 'y) y-scaled)))
    dispatch))
  
;;
;;
;;
(define player (make-led 2 2))
(define star (make-led (random 5) (random 5)))

(define score 0)

(define (set-position!)
  (value! game-env (player 'row) (player 'column) 1))

(define game-length 1300)

(for ([i (range game-length)])
  (let* ((axis (make-axis-controller))
         (x (axis 'x))
         (y (axis 'y)))
    (if (= i (- game-length 1))
        (displayln (string-append "In 1 minute you could catch " (number->string score) " stars!"))
        (begin
          (cond
            ((> x 50) ((player 'move!) 'right))
            ((> y 50) ((player 'move!) 'up))
            ((< x -50) ((player 'move!) 'left))
            ((< y -50) ((player 'move!) 'down)))
          (cond
            (((player 'touched?) star) (set! score (+ score 1))
                                       (star 'collect!))
            ((star 'collected?) ((star 'move!) 'random)
                                (star 'uncollect!)
                                (set-position!)))
          (draw game-env)))))
 