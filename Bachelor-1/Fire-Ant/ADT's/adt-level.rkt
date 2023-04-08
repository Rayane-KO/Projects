;;;--------------------LEVEL ADT--------------------;;;

(load "adt-fire-ant.rkt")
(load "adt-position.rkt")
(load "adt-key.rkt")
(load "adt-scorpion.rkt")
(load "adt-timer.rkt")
(load "adt-food.rkt")
(load "adt-pressure-plate.rkt")
(load "adt-power-up.rkt")
(define (make-level config)
  (let ((fire-ant-position #f)
        (scorpions-positions #f)
        (scorpion-paths #f)
        (keys-positions #f)
        (foods-positions #f)
        (power-ups-positions #f)
        (pressure-plates-positions #f)
        (exit #f)
        (current-level 1)
        (current-maze #f)
        (10-timer #f)
        (timer (make-timer (random 30))))
    ;;
    ;;afhankelijk van de configuratie, zullen er andere lijsten met posities en
    ;;andere informatie meegegeven worden
    (cond     
      ((= config 1)
       (begin 
         (set! fire-ant-position (make-position 9 2))
         (set! scorpions-positions (list (cons (make-position 12 3) 'normal) (cons (make-position 10 6) 'normal) (cons (make-position 1 13) 'random)))
         (set! scorpion-paths (list 'path-1 'path-2))
         (set! keys-positions (list (cons (make-position 14 5) (make-position 5 3)) (cons (make-position 1 3) (make-position 3 8))
                                    (cons (make-position 18 12) (make-position 17 16))))
         (set! foods-positions (list (make-position 5 16) (make-position 18 11) (make-position 12 13) (make-position 14 13) (make-position 16 13)
                                     (make-position 3 6) (make-position 5 6) (make-position 7 6)))
         (set! power-ups-positions (list (list (make-position 8 16) 'shield (make-timer 5)) (list (make-position 6 16) 'extra-life '())))
         (set! pressure-plates-positions (list (list (make-position 1 16) (make-position 11 9) (make-position 5 12))))
         (set! exit (make-position 18 16))
         (set! current-maze (level-1-maze))))

      ((= config 2)
       (begin
         (set! fire-ant-position (make-position 2 16))
         (set! scorpions-positions (list (cons (make-position 8 3) 'normal) (cons (make-position 18 16) 'normal) (cons (make-position 3 11) 'normal)
                                         (cons (make-position 8 8) 'random) (cons (make-position 11 8) 'random)))
         (set! scorpion-paths (list 'path-3 'path-4 'path-5))
         (set! keys-positions (list (cons (make-position 15 8) (make-position 18 5)) (cons (make-position 1 14) (make-position 18 6))
                                    (cons (make-position 18 16) (make-position 18 7))))
         (set! foods-positions (list (make-position 12 11) (make-position 12 13) (make-position 12 15)
                                     (make-position 16 6) (make-position 1 8) (make-position 1 6) (make-position 1 4)))
         (set! pressure-plates-positions (list (list (make-position 3 5) (make-position 2 3) (make-position 5 12))))
         (set! power-ups-positions (list (list (make-position 9 11) 'shield (make-timer 10))))
         (set! exit (make-position 19 9))
         (set! current-maze (level-2-maze))))

      ((= config 3)
       (begin
         (set! fire-ant-position (make-position 0 9))
         (set! scorpions-positions (list (cons (make-position 5 10) 'normal) (cons (make-position 18 16) 'normal) (cons (make-position 1 11) 'normal)
                                         (cons (make-position 16 8) 'random) (cons (make-position 13 8) 'random)))
         (set! scorpion-paths (list 'path-6 'path-7 'path-8))
         (set! keys-positions (list (cons (make-position 10 10) (make-position 16 5)) (cons (make-position 2 12) (make-position 2 16))))
         (set! foods-positions (list (make-position 10 11) (make-position 5 3) (make-position 7 3) (make-position 11 3) (make-position 13 3)
                                     (make-position 6 13) (make-position 8 13) (make-position 10 13)))
         (set! pressure-plates-positions (list (list (make-position 1 16) (make-position 5 12) (make-position 17 15))))
         (set! power-ups-positions (list (list (make-position 7 10) 'extra-life '())))
         (set! exit (make-position 19 3))
         (set! current-maze (level-3-maze)))))
    ;;
    ;;Procedure: lijst van posities -> lijst van objecten
    ;;
    (define (make-object-list object-proc position-list)
      (map object-proc position-list))
    ;;
    ;;MIER
    ;;
    (define fire-ant '())

    (set! fire-ant (make-fire-ant fire-ant-position))
    ;;      
    ;;SCHORPIOENEN
    ;;
    (define scorpion-objects '())
    (define normal-scorpions '())
    (define random-scorpions '())

    ;maakt van een lijst van posities en een symbool, dat het type schorpioen teruggeeft,
    ;een lijst van schorpioen-objecten
    ;('normal -> volgt een vast pad ;'random -> volgt een willekeurig pad)
    (define (make-scorpion-objects)
      (map (lambda (cel)
                  (let ((position (car cel))
                        (type (cdr cel)))
                    (make-scorpion position type))) scorpions-positions))
  
    (set! scorpion-objects
          (make-scorpion-objects))
    ;procedure om de schorpioenen in de juiste lijst de plaatsen
    (define (find-scorpions list)
      (map (lambda (scorpion) (if (eq? (scorpion 'type) 'random)
                                  (set! random-scorpions (cons scorpion random-scorpions))
                                  (set! normal-scorpions (cons scorpion normal-scorpions)))) scorpion-objects))
    (find-scorpions scorpion-objects)
    
    (set! normal-scorpions (reverse normal-scorpions))
    ;;
    ;;SLEUTELS
    ;;
    (define key-objects '())
    ;maakt van een lijst van posities en deur-posities, een lijst van sleutel-objecten
    (define (make-key-objects)
      (map (lambda (cel)
             (let ((position (car cel))
                   (door-position (cdr cel)))
               (make-key position door-position))) keys-positions))

    (set! key-objects (make-key-objects))
    ;;
    ;;VOEDSEL
    ;;
    (define food-objects '())
    ;maakt van een lijst van posities, een lijst van voedsel-objecten
    (define (make-food-objects position-list)
      (make-object-list make-food position-list))

    (set! food-objects (make-food-objects foods-positions))
    ;;
    ;;DRUKPLATEN
    ;;
    (define pressure-plate-objects '())
    ;maakt van een lijst van posities (met hierin de positie van de drukplaat, deur en blok),
    ;een lijst van drukplaat-objecten
    (define (make-pressure-plate-objects)
      (map (lambda (cel)
          (let ((pp-position (car cel))
                 (door-position (cadr cel))
                 (block-position (caddr cel)))
            (make-pressure-plate pp-position door-position block-position))) pressure-plates-positions))

    (set! pressure-plate-objects (make-pressure-plate-objects))

    (define objects-with-doors (append key-objects pressure-plate-objects))
    ;;
    ;;POWER-UPS
    ;;
    (define power-up-objects '())
    ;maakt van een lijst van posities en het soort power-up (eventueel ook een timer),
    ;een lijst van power-up-objecten
    (define (make-power-up-objects)
      (map (lambda (cel)
          (let ((position (car cel))
                 (type (cadr cel))
                 (timer (caddr cel)))
            (if (null? timer)
                      (make-power-up position type)
                      (make-power-up position type timer)))) power-ups-positions))

    (set! power-up-objects (make-power-up-objects))

    (define shield-power-ups (search-power-up 'shield power-up-objects))
    (define extra-life-power-ups (search-power-up 'extra-life power-up-objects))
    ;;;;-------------------------------------------------------------------------------------------------------------;;;;

    ;deze procedure zal, in de huidige maze, op de positie van de deuren een getal plaatsen
    ;hierdoor zal mijn mier niet door de muur kunnen gaan
    (define (activate-doors! object-list)
      (map (lambda (object) (value! current-maze ((object 'door-position) 'y) ((object 'door-position) 'x) 6)) object-list))
    (activate-doors! objects-with-doors)
    
    ;hiermee zullen we, voor de huidige maze, een lijst hebben met de posities van de muren
    (define walls-position '())
    (define (make-maze-positions)
      (define (go i j)
        (if (< i matrix-width)
            (if (or (= 1 (value current-maze j i)) (= 6 (value current-maze j i)))
                (begin
                  (set! walls-position (cons (make-position i j) walls-position))
                  (go (+ i 1) j))
                (go (+ i 1) j))))
      (define (go-t i j)
        (if (< j matrix-height)
            (begin
              (go i j)
              (go-t i (+ j 1)))))
      (go-t 0 0))
    (make-maze-positions)

    ;procedures die ervoor zal zorgen dat mijn mier kan bewegen in functie van de ingedrukte toets
    (define (move-fire-ant! key)
      (define (move-direction! direction)
        (if (eq? key direction)
            ((fire-ant 'move-ant!) direction current-maze)))
      (for-each move-direction! direction-list))

    ;won? geeft #t als de mier, de uitgang, heeft bereikt  
    (define (won? element)
      (and (if (eq? element 'level)
               (< config max-level)
               (= config max-level))
           ((fire-ant-position 'same?) exit)))
    ;lost? geeft #t als de mier geen levens en extra-levens meer heeft
    (define (lost? score)
      (and (= (score 'lives) 0) (= (score 'extra-lives) 0)))
    
    ;hit-object? kijkt of de mier een ander object heeft aangeraakt
    (define (hit-object? object width height)
      ((fire-ant 'hit?) object width height))
    
    ;scorpion-follow! neemt 2 lijsten: een lijst van schorpioen-objecten en lijst van paden
    ;en elk schorpioen zal het gegeven pad volgen. De random-schorpioenen zullen ook hun eigen willekeurig pad volgen.
    (define (normal-scorpion-follow! scorpion-list path-list time)
      (for-each (lambda (normal-scorpion path) (paths normal-scorpion path walls-position time)) scorpion-list path-list))
    (define (random-scorpion-follow! time)
      (for-each (lambda (random-scorpion) (paths random-scorpion 'random-path walls-position time)) random-scorpions))
    ;;
    ;;Update-procedures
    ;;
    ;update-puzzles! zal kijken of de mier een ander object raakt en
    ;vervolgens de juiste deur openen (door in de maze een getal te veranderen in een 0)
    (define (update-puzzles!)
      (for-each (lambda (key)
                  (if (hit-object? key 1 1)
                      (let ((door-x ((key 'door-position) 'x))
                            (door-y ((key 'door-position) 'y)))
                        (value! current-maze door-y door-x 0)
                        (key 'collect!)))) key-objects)
      (for-each (lambda (pp) ((pp 'move!) fire-ant current-maze) 
                  (if (pp 'pressed?)
                      (let ((door-x ((pp 'door-position) 'x))
                            (door-y ((pp 'door-position) 'y)))
                        (value! current-maze door-y door-x 0)))) pressure-plate-objects))
      
    ;activate-power-ups! zal de power-up activeren wanneer de power-up opgerapen wordt
    (define (activate-power-ups! power-up-list time)
      (for-each (lambda (power-up)
                  (cond
                    ((and (hit-object? power-up 1 1) (not (power-up 'collected?))) (power-up 'collect!))
                    ((power-up 'collected?)
                     (cond
                       ((eq? (power-up 'type) 'shield) ((power-up 'start-timer!) time))
                       ((eq? (power-up 'type) 'extra-life) (power-up 'activate-one-time!))))))
                power-up-list))

    ;procedure die ervoor zorgt dat de schorpioenen op willekeurige momenten sneller gaan
    (define (random-speed-change time)
      (cond (timer
             (if (timer 'active?)
                 ((timer 'update!) time)
                 (begin
                   (timer 'reset!)
                   (set! 10-timer (make-timer 10))
                   ((10-timer 'update!) time)
                   (set! scorpion-speed normal-speed)
                   (set! timer #f))))
            (10-timer
             (if (10-timer 'active?)
                 ((10-timer 'update!) time)
                 (begin
                   (10-timer 'reset!)
                   (set! timer (make-timer (random 30)))
                   ((timer 'update!) time)
                   (set! scorpion-speed fast-speed)
                   (set! 10-timer #f))))))

    ;update! zal alle spelelementen updaten
    (define (update! time)
      (update-puzzles!)
      (activate-power-ups! power-up-objects time)
      (normal-scorpion-follow! normal-scorpions scorpion-paths time)
      (random-scorpion-follow! time)
      (random-speed-change time))

    ;kijkt of de vuurmier een schorpioen heeft aangeraakt
    (define (hit-scorpion? scorpion)
      (hit-object? scorpion scorpion-width scorpion-height))

    ;update de score als het een object aanraakt 
    (define (increase-score! object-list score)
      (for-each (lambda (object)
                  (if (and (hit-object? object object-width object-height) (not (object 'collected?)))
                      (begin (object 'collect!)
                             ((score 'update-current-score!) (object 'bonus))))) object-list))

    ;update-score! zal het scoreboard updaten
    (define (update-score! score reset-proc)
      (let ((lives (score 'lives))
            (extra-lives (score 'extra-lives))
            (power-ups power-up-objects))
        (define (update-lives!)
          (for-each (lambda (scorpion) (if (hit-scorpion? scorpion)
                                           (begin
                                             (reset-proc)
                                             (if (= lives 0)
                                                 ((score 'update-extra-lives!) (- extra-lives 1))
                                                 ((score 'update-lives!) (- lives 1)))))) scorpion-objects))
        (define (update-extra-life!)
          (for-each (lambda (power-up)
                      (if (power-up 'active?)
                          (begin
                            ((score 'update-extra-lives!) (+ extra-lives 1))))) extra-life-power-ups))
        
        (increase-score! food-objects score)
        (update-extra-life!)
        (if (null? shield-power-ups)
            (update-lives!)
            (for-each (lambda (power-up)
                            (if (not (power-up 'active?))
                                (update-lives!))) shield-power-ups))))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-level m)
      (cond
        ((eq? m 'fire-ant-object) fire-ant)
        ((eq? m 'scorpion-objects) scorpion-objects)
        ((eq? m 'key-objects) key-objects)
        ((eq? m 'food-objects) food-objects)
        ((eq? m 'power-up-objects) power-up-objects)
        ((eq? m 'pressure-plate-objects) pressure-plate-objects)
        ((eq? m 'move-fire-ant!) move-fire-ant!)
        ((eq? m 'won?) won?)
        ((eq? m 'lost?) lost?)
        ((eq? m 'update!) update!)
        ((eq? m 'update-score!) update-score!)
        ((eq? m 'maze) current-maze)
        (else "MESSAGE NOT UNDERSTOOD (level-adt)")))
    dispatch-level))