;;;--------------------DRAW ADT--------------------;;;

(#%require "Graphics.rkt")
(load "adt-fire-ant.rkt")
(load "adt-position.rkt")
(load "adt-level.rkt")
(load "adt-scorpion.rkt")

(load "constants.rkt")
(define (make-draw)
  (let ((current-score-var -1)
        (high-score-var -1)
        (lives-var -1)
        (extra-lives-var -1)
        (current-level-var 0))
    ;;
    ;;Hier wordt de window aangemaakt en de achtergrond-kleur veranderd
    ;;
    (define window (make-window window-width window-height "Fire Ant"))
    ((window 'set-background!) "black")
    ;;
    ;;Hier zijn de verschillende lagen terug te vinden
    ;;
    (define maze-layer (window 'make-layer))
    (define collectables-layer (window 'make-layer))
    (define fire-ant-layer (window 'make-layer))
    (define scorpion-layer (window 'make-layer))
    (define score-layer (window 'make-layer))
    ;;
    ;;een procedure om alle lagen leeg te maken
    ;;
    (define (clear-all-layers!)
      (for-each (lambda (layer) (layer 'empty)) (list maze-layer collectables-layer fire-ant-layer scorpion-layer score-layer)))
    ;;
    ;;MIER
    ;;
    (define fire-ant-tile (make-tile-sequence (list (make-bitmap-tile "images/tile000.png" "images/tile000-m.png")
                                                    (make-bitmap-tile "images/tile003.png" "images/tile003-m.png")
                                                    (make-bitmap-tile "images/tile006.png" "images/tile006-m.png")
                                                    (make-bitmap-tile "images/tile009.png" "images/tile009-m.png"))))
    ;de fire-ant-tile bestaat uit een tile-sequence van de mier in verschillende richtingen

    ((fire-ant-layer 'add-drawable) fire-ant-tile)
    ;;
    ;;SCHORPIOENEN
    ;;
    (define scorpion-tiles '())
    ;lijst van schorpioenen. De lijst bevat cons-cellen met in de car het adt en in de cdr de tile


    ;procedure om een cons-cel toe te voegen aan de lijst
    (define (add-scorpion-tile! scorpion-adt)
      (let ((type (scorpion-adt 'type))
            (scorpion-tile #f))
        (define (scorpion-type)
          (if (eq? type 'random);afhankelijk van het type krijg je ofwel een paarse schorpioen ofwel een groene
              "r.png"
              "s.png"))
        (define (tile string)
          (make-bitmap-tile (string-append string (scorpion-type)) (string-append string "sm.png")))
        
        (set! scorpion-tile (make-tile-sequence
                             (list (tile "images/tile000-")
                                   (tile "images/tile003-")
                                   (tile "images/tile006-")
                                   (tile "images/tile009-")
                                   (make-bitmap-tile "images/tile000-f.png" "images/tile000-sm.png")
                                   (make-bitmap-tile "images/tile003-f.png" "images/tile003-sm.png")
                                   (make-bitmap-tile "images/tile006-f.png" "images/tile006-sm.png")
                                   (make-bitmap-tile "images/tile009-f.png" "images/tile009-sm.png"))))
        ((scorpion-layer 'add-drawable) scorpion-tile)
        (set! scorpion-tiles (cons (cons scorpion-adt scorpion-tile) scorpion-tiles))
        scorpion-tile))
    
    ;hiermee kunnen we de tile van een bepaald adt teruggeven
    (define (get-tile adt tiles-list add-proc)
      (let ((res (assoc adt tiles-list)))
        (if res
            (cdr res)
            (add-proc adt))))
    
    ;;hiermee krijgen we de scorpion-tile van een bepaald scorpion-adt terug
    (define (get-scorpion scorpion-adt)
      (get-tile scorpion-adt scorpion-tiles add-scorpion-tile!))
    ;;
    ;;SLEUTELS
    ;;
    (define key-tiles '())

    ;;procedure om een sleutel toe te voegen met de bijhorende tile
    (define (add-key-tile! key-adt)
      (let* ((key-tile (make-bitmap-tile "images/keys/key-1.png"))
             (door-tile (make-tile cel-width cel-height "images/keys/door-1.png"))
             (tiles (cons key-tile door-tile)))
        ((collectables-layer 'add-drawable) key-tile)
        ((collectables-layer 'add-drawable) door-tile)
        (set! key-tiles (cons (cons key-adt tiles) key-tiles))
        tiles))

    ;;tile van een bepaald key-adt
    (define (get-key key-adt)
      (get-tile key-adt key-tiles add-key-tile!))
    ;;
    ;;VOEDSEL
    ;;
    (define food-tiles '())
    ;lijst van voedsel

    ;;procedure om een voedsel-object toe te voegen met de bijhorende tile
    (define (add-food-tile! food-adt)
      (let ((type (food-adt 'type)))
        (define (test-color)
          (cond
            ((eq? type 'rare) "deepskyblue")
            ((eq? type 'epic) "mediumorchid")
            ((eq? type 'legendary) "gold")
            ((eq? type 'mythical) "tomato")))
        (let* ((food-tile (make-tile cel-width cel-height))
               (egg-tile (make-tile egg-width-px egg-height-px "images/egg.png" "images/egg-m.png"))
               (ellipse-x 5)
               (ellipse-y 10))
          (if (eq? type 'egg)
              (set! food-tile egg-tile)
              ((food-tile 'draw-ellipse) ellipse-x ellipse-y food-width-px food-height-px (test-color)))
          ((collectables-layer 'add-drawable) food-tile)
          (set! food-tiles (cons (cons food-adt food-tile) food-tiles))
          food-tile)))

    ;;tile van een bepaalde food-adt
    (define (get-food food-adt)
      (get-tile food-adt food-tiles add-food-tile!))
    ;;
    ;;DRUKPLATEN
    ;;
    (define pressure-plate-tiles '())
    ;lijst van drukplaten

    ;voegt aan de lijst van drukplaten een cons-cel met in de car het adt
    ;en in de cdr de bijhorende tile
    (define (add-pressure-plate-tile! pressure-plate-adt)
      (let* ((pressure-tile (make-tile cel-width cel-height))
             (door-tile (make-tile cel-width cel-height))
             (block-tile (make-tile cel-width cel-height))
             (tiles (list pressure-tile door-tile block-tile))
             (pressure-px 30)
             (pressure-x 3)
             (button-px (- pressure-px 10))
             (button-x 5)
             (button-y 8)
             (block-detail 7)
             (block-detail-x 8)
             (block-detail-y 5))
        ((pressure-tile 'draw-rectangle) zero pressure-x pressure-px pressure-px "darkgray")
        ((pressure-tile 'draw-rectangle) button-x button-y button-px button-px "red")
        ((block-tile 'draw-ellipse) zero zero cel-width cel-height "darkslategray")
        ((block-tile 'draw-ellipse) block-detail-x block-detail-y block-detail block-detail "white")
        ((door-tile 'draw-rectangle) zero zero cel-width cel-height "firebrick")
        ((collectables-layer 'add-drawable) block-tile)
        ((collectables-layer 'add-drawable) pressure-tile)
        ((collectables-layer 'add-drawable) door-tile)
        (set! pressure-plate-tiles (cons (cons pressure-plate-adt tiles) pressure-plate-tiles))
        tiles))

    ;geeft de cdr van de bovenvermelde cons-cel terug
    (define (get-pressure-plate pressure-plate-adt)
      (get-tile pressure-plate-adt pressure-plate-tiles add-pressure-plate-tile!))
    ;;
    ;;POWER-UPS
    ;;
    (define power-up-tiles '())
    
    (define (add-power-up-tile! power-up-adt . type)
      (let ((type (power-up-adt 'type))
            (power-up-tile (make-tile cel-width cel-height))
            (power-up-ellipse 30)
            (power-up-detail 20)
            (ellipse-x 3)
            (ellipse-y 5)
            (detail-x 8)
            (detail-y 10)
            (text-size 12)
            (text-x 14)
            (text-y 10))
        (define (power-up-color)
          (cond
            ((eq? type 'shield) "royalblue")
            ((eq? type 'extra-life) "orangered")))
        (define (power-up-letter)
          (cond
            ((eq? type 'shield) "S")
            ((eq? type 'extra-life) "E")))
        ((power-up-tile 'draw-ellipse) ellipse-x ellipse-y power-up-ellipse power-up-ellipse (power-up-color))
        ((power-up-tile 'draw-ellipse) detail-x detail-y power-up-detail power-up-detail "white")
        ((power-up-tile 'draw-text) (power-up-letter) text-size text-x text-y (power-up-color))
        ((collectables-layer 'add-drawable) power-up-tile)
        (set! power-up-tiles (cons (cons power-up-adt power-up-tile) power-up-tiles))
        power-up-tile))
    
    (define (get-power-up power-up-adt)
      (get-tile power-up-adt power-up-tiles add-power-up-tile!))
    ;;
    ;;SHIELD
    ;;
    (define shield-tiles '())

    (define (add-shield-tile! shield-adt)
      (let* ((type (shield-adt 'type))
             (shield-tile-px 50)
             (shield-px 40)
             (shield-tile (make-tile shield-tile-px shield-tile-px)))
        ((shield-tile 'draw-ellipse) zero zero shield-px shield-px "royalblue")
        ((collectables-layer 'add-drawable) shield-tile)
        (set! shield-tiles (cons (cons shield-adt shield-tile) shield-tiles))
        shield-tile))

    (define (get-shield shield-adt)
      (get-tile shield-adt shield-tiles add-shield-tile!))
    ;;
    ;;DOOLHOF
    ;;
    (define maze-tile
      (make-tile window-width window-height))
    (define (draw-rectangle-on-position a b color)
      ((maze-tile 'draw-rectangle) a b cel-width cel-height color))
    (define sky-tile (make-tile window-width (* 2 cel-height) "images/sky.png"))
    ;procedure die volgens de gegeven matrix de doolhof zal tekenen
    (define (draw-maze! level-adt)
      (define (draw-walls)
        (define (draw i j)
          (if (< j window-width)
              (begin
                (let* ((value-box (value (level-adt 'maze) (/ i cel-height) (/ j cel-width))))
                  (define (draw-rectangle-color n color)
                    (if (= value-box n)
                        (draw-rectangle-on-position j i color)))
                  (for-each (lambda (number color) (draw-rectangle-color number color)) '(0 1 6) '("black" "navy" "black"))
                  (draw i (+ j cel-width))))))
        (define (walls i j)
          (if (<= i (- window-height cel-height))
              (begin (draw i j)
                     (walls (+ i cel-height) j))))
        (walls 0 0))
      (draw-walls)
      ((maze-layer 'add-drawable) sky-tile)
      ((maze-layer 'add-drawable) maze-tile))
    ;;
    ;;procedure om een tile, op een positie, te tekenen
    ;;
    (define (draw-object! position tile detail-x detail-y)
      (let* ((position-x (position 'x))
             (position-y (position 'y))
             (draw-x (+ (* position-x cel-width) detail-x))
             (draw-y (+ (* position-y cel-height) detail-y)))
        ((tile 'set-x!) draw-x)
        ((tile 'set-y!) draw-y)))
    ;;
    ;;gaat een tile in de juiste richting door middel van de set-next! en set-previous!
    ;;
    (define (draw-direction! object tile-seq)
      (let* ((direction (object 'direction))
             (prev-direction (object 'previous-direction))
             (prev-direction! (object 'prev-direction!))
             (draw-detail -5))
        (draw-object! (object 'position) tile-seq draw-detail draw-detail) ;tile-seq= (down left right up) om van down naar
        (define (go-to-tile prev-next-msg prev-d d times)                  ;bijvoorbeeld left te gaan, doe je 1 keer set-next!
          (if (and (eq? prev-direction prev-d) (eq? direction d))
              (cond
                ((= times 1) (tile-seq prev-next-msg))
                ((= times 2) (tile-seq prev-next-msg) (tile-seq prev-next-msg))
                (else (tile-seq prev-next-msg) (tile-seq prev-next-msg) (tile-seq prev-next-msg)))
              (prev-direction! direction)))
          
        (if (not (eq? prev-direction direction))
            (begin
              (go-to-tile 'set-next! 'down 'up 3)
              (go-to-tile 'set-next! 'left 'up 2)
              (go-to-tile 'set-next! 'right 'up 1)

              (go-to-tile 'set-previous! 'up 'down 3)
              (go-to-tile 'set-previous! 'right 'down 2)
              (go-to-tile 'set-previous! 'left 'down 1)

              (go-to-tile 'set-previous! 'up 'left 2)
              (go-to-tile 'set-previous! 'right 'left 1)
              (go-to-tile 'set-next! 'down 'left 1)

              (go-to-tile 'set-next! 'down 'right 2)
              (go-to-tile 'set-previous! 'up 'right 1)
              (go-to-tile 'set-next! 'left 'right 1)))))
    ;;
    ;;gaat de tile van de mier resetten
    ;;
    (define (switch-tile! tile-seq)
      (tile-seq 'set-next!) (tile-seq 'set-next!) (tile-seq 'set-next!) (tile-seq 'set-next!))
    
    (define (reset-fire-ant-tile! fire-ant-adt)
      (let ((direction (fire-ant-adt 'previous-direction)))
        (cond
          ((eq? direction 'left) (fire-ant-tile 'set-previous!))
          ((eq? direction 'right) (fire-ant-tile 'set-previous!) (fire-ant-tile 'set-previous!))
          ((eq? direction 'up) (fire-ant-tile 'set-next!)))))
    
    (define (reset-scorpion-tile! scorpion-adt)
      (let ((switched-to-fast? (scorpion-adt 'switched-to-fast?))
            (scorpion-tile (get-scorpion scorpion-adt)))
        (if switched-to-fast?
            (switch-tile! scorpion-tile))))
    
    (define (reset-tiles! fire-ant-adt scorpion-adts)
      (begin
        (reset-fire-ant-tile! fire-ant-adt)
        (for-each (lambda (scorpion) (reset-scorpion-tile! scorpion)) scorpion-adts)))
    ;;
    ;;het tekenen van de schorpioenen
    ;;
    (define (draw-scorpion! scorpion-adt)
      (let ((scorpion-tile (get-scorpion scorpion-adt)))
        ((scorpion-adt 'speed-test) (lambda () (switch-tile! scorpion-tile)))
        (draw-direction! scorpion-adt scorpion-tile)))

    (define (draw-scorpions! scorpion-adts)
      (for-each draw-scorpion! scorpion-adts))
    ;;
    ;;het tekenen van de mier
    ;;
    (define (draw-fire-ant! fire-ant-adt)
      (draw-direction! fire-ant-adt fire-ant-tile))
    ;;
    ;;het tekenen van de sleutels
    ;;
    (define (draw-key! key-adt)
      (let* ((tiles (get-key key-adt))
             (key-tile (car tiles))
             (door-tile (cdr tiles))
             (draw-detail 0))
        (if (key-adt 'collected?)
            (begin
              ((collectables-layer 'remove-drawable) key-tile)
              ((collectables-layer 'remove-drawable) door-tile))
            (begin
              (draw-object! (key-adt 'position) key-tile draw-detail draw-detail)
              (draw-object! (key-adt 'door-position) door-tile draw-detail draw-detail)))))

    (define (draw-keys! key-adts)
      (map draw-key! key-adts))
    ;;
    ;;het tekenen van het voedsel
    ;;
    (define (draw-food! food-adt)
      (let ((food-tile (get-food food-adt))
            (draw-detail-x 0)
            (draw-detail-y -5))
        (if (food-adt 'collected?)
            ((collectables-layer 'remove-drawable) food-tile)
            (draw-object! (food-adt 'position) food-tile draw-detail-x draw-detail-y))))

    (define (draw-foods! food-adts)
      (map draw-food! food-adts))
    ;;
    ;;het tekenen van de drukplaten
    ;;
    (define (draw-pressure-plate pressure-plate-adt)
      (let* ((tiles (get-pressure-plate pressure-plate-adt))
             (pressure-plate-tile (car tiles))
             (door-tile (cadr tiles))
             (block-tile (caddr tiles))
             (draw-detail 2))
        (if (pressure-plate-adt 'pressed?)
            ((collectables-layer 'remove-drawable) door-tile)
            (begin
              (draw-object! (pressure-plate-adt 'position) pressure-plate-tile draw-detail zero)
              (draw-object! (pressure-plate-adt 'door-position) door-tile zero zero)))
        (draw-object! (pressure-plate-adt 'block-position) block-tile zero zero)))

    (define (draw-pressure-plates pressure-plate-adts)
      (map draw-pressure-plate pressure-plate-adts))
    ;;
    ;;het tekenen van de power-ups
    ;;
    (define (draw-power-up power-up-adt)
      (let ((power-up-tile (get-power-up power-up-adt))
            (draw-detail -2))
        (if (power-up-adt 'collected?)
            ((collectables-layer 'remove-drawable) power-up-tile)
            (draw-object! (power-up-adt 'position) power-up-tile zero draw-detail))))

    (define (draw-power-ups power-up-adts)
      (for-each draw-power-up power-up-adts))
    ;;
    ;;het tekenen van de shield
    ;;
    (define (draw-shield shield-adt fire-ant-adt)
      (let ((shield-tile (get-shield shield-adt)))
        (if (and (shield-adt 'collected?) (shield-adt 'active?))
            (draw-object! (fire-ant-adt 'position) shield-tile zero zero)
            (if (shield-adt 'activated?)
                ((collectables-layer 'remove-drawable) shield-tile)))))

    (define (draw-shields shield-adts fire-ant-adt)
      (for-each (lambda (shield) (if (eq? (shield 'type) 'shield)
                                     (draw-shield shield fire-ant-adt))) shield-adts))
    ;;
    ;;level tekenen
    ;;
    (define (draw-level! level-adt)
      (draw-fire-ant! (level-adt 'fire-ant-object))
      (draw-scorpions! (level-adt 'scorpion-objects))
      (draw-keys! (level-adt 'key-objects))
      (draw-foods! (level-adt 'food-objects))
      (draw-pressure-plates (level-adt 'pressure-plate-objects))
      (draw-power-ups (level-adt 'power-up-objects))
      (draw-shields (level-adt 'power-up-objects) (level-adt 'fire-ant-object)))
    ;;level verwijderen
    ;;
    (define (clear-level!)
      (scorpion-layer 'empty)
      (collectables-layer 'empty))
    ;;
    ;;scoreboard tekenen
    ;;
    (define number-tile (make-tile window-width window-height))
    ((score-layer 'add-drawable) number-tile)
    (define (draw-number! number)
      (let ((number-string (number->string number))
            (text-size 20)
            (pos-x 5)
            (pos-y (* 2 cel-height)))
        (number-tile 'clear)
        ((number-tile 'draw-text) (string-append "CHAMBER #" number-string) text-size pos-x pos-y "black")))
    
    (define score-tile (make-tile window-width window-height))
    ((score-layer 'add-drawable) score-tile)
    (define (draw-score! score-adt)
      (let ((current-score (score-adt 'get-current-score))
            (high-score (score-adt 'get-high-score))
            (lives (score-adt 'lives))
            (extra-lives (score-adt 'extra-lives)))
        (cond
          ((or (not (and (= current-score current-score-var) (= high-score high-score-var)))
               (not (= lives-var lives)) (not (= extra-lives-var extra-lives)))
           (set! lives-var lives)
           (set! extra-lives-var extra-lives)
           (set! current-score-var current-score)
           (set! high-score-var high-score)
           (let ((current-score-string (number->string current-score))
                 (high-score-string (number->string high-score))
                 (lives-string (number->string lives))
                 (extra-lives-string (number->string extra-lives))
                 (text-size 25)
                 (score-x 90)
                 (lives-x 550)
                 (extra-x 440)
                 (scores-y (* 18 cel-height))
                 (health-y (* 19 cel-height)))
             (score-tile 'clear)
             ((score-tile 'draw-text) (string-append "SCORE " current-score-string) text-size score-x scores-y "yellow")
             ((score-tile 'draw-text) (string-append "HIGH-SCORE " high-score-string) text-size zero health-y "gold")
             ((score-tile 'draw-text) (string-append "LIVES #" lives-string) text-size lives-x scores-y "red")
             ((score-tile 'draw-text) (string-append "EXTRA-LIVES #" extra-lives-string) text-size extra-x health-y "lightslategray"))))))

    (define (add-drawables-after-lost)
      ((score-layer 'add-drawable) score-tile)
      ((score-layer 'add-drawable) number-tile)
      ((maze-layer 'add-drawable) maze-tile)
      ((maze-layer 'add-drawable) sky-tile)
      ((fire-ant-layer 'add-drawable) fire-ant-tile)
      ((score-layer 'remove-drawable) game-over-tile)
      ((score-layer 'remove-drawable) finish-tile)
      ((score-layer 'remove-drawable) try-again-tile))

    (define draw-time 0)
    (define (change-position-to tile x y)
      ((tile 'set-x!) x)
      ((tile 'set-y!) y))
    (define (draw-tile! main-tile scnd-tile time)
      (let ((main-x (* 2 cel-width))
            (main-y (* 4 cel-height))
            (other-x (+ (* 3 cel-width) 10))
            (other-y (* 8 cel-height)))
      ((score-layer 'add-drawable) main-tile)
      ((score-layer 'add-drawable) scnd-tile)
      (change-position-to main-tile main-x main-y)
      (change-position-to scnd-tile other-x other-y)
      (if (> draw-time 800)
          (begin
            (main-tile 'set-next!)
            (set! draw-time 0))
          (set! draw-time (+ draw-time time)))))

    
    (define game-over-tile (make-tile-sequence (list (make-bitmap-tile "images/game-over-red.png")
                                                     (make-bitmap-tile "images/game-over-white.png"))))
    (define finish-tile (make-tile-sequence (list (make-bitmap-tile "images/finish.png")
                                                  (make-bitmap-tile "images/finish-red.png"))))
    (define try-again-tile (make-bitmap-tile "images/try-again.png"))

    (define (draw-end! event time)
      (draw-tile! (if (eq? event 'lost)
                      game-over-tile
                      finish-tile) try-again-tile time))
      

    ;;spel-lus & toets-functie
    (define (game-loop-function! function)
      ((window 'set-update-callback!) function))
    (define (game-key-function! function)
      ((window 'set-key-callback!) function))

    

    (define (dispatch-draw m)
      (cond
        ((eq? m 'draw-level!) draw-level!)
        ((eq? m 'draw-number!) draw-number!)
        ((eq? m 'draw-maze!) draw-maze!)
        ((eq? m 'draw-score!) draw-score!)
        ((eq? m 'draw-end!) draw-end!)
        ((eq? m 'clear-level!) (clear-level!))
        ((eq? m 'clear-all-layers!) (clear-all-layers!))
        ((eq? m 'reset-tiles!) reset-tiles!)
        ((eq? m 'add-drawables-after-lost) (add-drawables-after-lost))
        ((eq? m 'game-loop-function!) game-loop-function!)
        ((eq? m 'game-key-function!) game-key-function!)
        
        (else (display "MESSAGE NOT UNDERSTOOD! (draw-adt)"))))
    dispatch-draw))





  
