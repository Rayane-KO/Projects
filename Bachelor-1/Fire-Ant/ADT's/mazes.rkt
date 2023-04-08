(load "help-procedures.rkt")
(load "constants.rkt")
;;;--------------------MAZES--------------------;;;

;;procedures om een te kunnen vullen
;;fill zal makkelijk rijen, kolommen en rechthoeken vullen met een bepaald getal

(define (fill rc m r-c from to number)
  (define (fill matrix i)
    (if (< i to)
        (begin
          (cond ((eq? rc 'row) (value! matrix r-c i number))
                ((eq? rc 'column) (value! matrix i r-c number)))
          (fill matrix (+ i 1)))))
  (fill m from))

(define (fill-row matrix row from to number)
  (fill 'row matrix row from to number))
  
(define (fill-column matrix column from to number) 
  (fill 'column matrix column from to number))

;fill-rectangle 
(define (fill-rectangle matrix start stop from to number)
  (if (not (> start stop))
      (begin
        (fill-row matrix start from to number)
        (fill-rectangle matrix (+ start 1) stop from to number))))

;;eerste schrijf ik 2 procedures om rijen en kolommen te kunnen vullen met 1'en
  (define matrix-width-n (- matrix-width 1))
    
    (define (make-four-walls maze)
      (fill-column maze 0 2 (- matrix-width-n 1) 1)
      (fill-column maze matrix-width-n 2 (- matrix-width-n 1) 1)
      (fill-row maze 2 1 9 1)
      (fill-row maze 2 11 matrix-width-n 1)
      (fill-row maze (- matrix-width-n 2) 1 matrix-width-n 1))

    (define (fill-sky maze)
      (fill-row maze 0 0 20 2)
      (fill-row maze 1 0 20 2))
  
    ;;hiermee maak ik manueel de 'muren' aan door 1'en te plaatsen
    (define (level-1-maze)
      (let ((maze (make-matrix matrix-width matrix-height)))
        (fill-sky maze)
        ;;zij-muren
        (make-four-walls maze)
        ;;doolhof
        (fill-column maze 11 4 8 1)
        (fill-row maze 8 11 (- matrix-width 1) 1)
      
        (fill-rectangle maze 10 15 11 (- matrix-width-n 1) 1)
        (fill-row maze 12 11 (- matrix-width 3) 0)
        (fill-row maze 13 11 (- matrix-width 3) 0)

        (fill-row maze 5 1 9 1)
        (value! maze 5 10 1)

        (fill-rectangle maze 7 (- matrix-width-n 4) 2 9 1)
        (fill-row maze 11 2 9 0)
        (value! maze 16 19 0)
        (value! maze 4 5 1)
        (value! maze 15 10 1)
        

        (fill-row maze 8 2 8 0)
        (value! maze 9 7 0)
        (value! maze 9 6 0)
        (fill-row maze 11 2 9 1)
        (fill-column maze 6 10 15 0)
        (fill-rectangle maze 12 14 3 6 0)
        (value! maze 13 18 1)
        (value! maze 7 2 0)

        (value! maze 2 9 6);zodat de willekeurige schorpioenen niet kunnen ontsnappen
        (value! maze 2 10 6)
        (value! maze 16 19 6)
        
        maze))

    (define (level-2-maze)
      (let ((maze (make-matrix matrix-width matrix-height)))
        (fill-sky maze)
        (make-four-walls maze)
        (value! maze 16 0 0)
        (value! maze 15 1 1)
        (value! maze 15 2 1)
        (value! maze 2 9 1)
        (value! maze 2 10 1)
        (fill-column maze 2 4 15 1)
        (fill-column maze 17 5 11 1)
        (value! maze 10 18 1)
        (value! maze 7 16 1)
        (value! maze 9 16 1)
        (value! maze 9 15 1)
        (fill-column maze 15 5 8 1)
        (fill-row maze 6 7 15 1)
        (value! maze 5 7 1)
        (value! maze 4 7 1)
        (fill-row maze 10 7 17 1)
        (value! maze 9 19 0)
        (fill-column maze 17 12 17 1)
        (fill-column maze 15 11 16 1)
        (fill-column maze 13 12 17 1)
        (fill-column maze 11 11 16 1)
        (fill-column maze 9 12 17 1)
        (fill-column maze 7 9 16 1)
        (fill-row maze 4 3 5 1)
        (fill-row maze 10 5 7 1)
        (value! maze 7 7 1)
        (fill-column maze 4 5 9 1)

        (value! maze 16 7 1)
        (value! maze 10 9 0)
        (value! maze 8 7 1)
        (value! maze 6 12 0)

        (value! maze 16 0 6)
        maze))

    (define (level-3-maze)
      (let ((maze (make-matrix matrix-width matrix-height)))
        (fill-sky maze)
        (make-four-walls maze)
        (value! maze 2 9 1)
        (value! maze 2 10 1)
        (fill-rectangle maze 2 4 16 19 1)
        (fill-row maze 6 16 19 1)

        (fill-row maze 4 10 15 1)
        (fill-row maze 4 4 9 1)

        (value! maze 9 0 0)
        (value! maze 10 1 1)
        (fill-column maze 2 4 11 1)

        (fill-row maze 7 4 15 1)
        (fill-row maze 10 14 19 1)
        (value! maze 9 14 1)
        (value! maze 8 14 1)

        (fill-column maze 16 12 17 1)
        (fill-column maze 4 10 15 1)
        (fill-row maze 14 1 4 1)
        (value! maze 11 12 1)
        (value! maze 10 12 1)
        (fill-row maze 12 6 13 1)

        (fill-row maze 14 5 13 1)

        (fill-row maze 9 4 13 1)
        (value! maze 3 18 0)
        (value! maze 4 18 0)
        (value! maze 3 19 0)
        (value! maze 15 2 1)
        (value! maze 12 16 0)

        (value! maze 9 0 6)
        
        maze))

    
    
 
