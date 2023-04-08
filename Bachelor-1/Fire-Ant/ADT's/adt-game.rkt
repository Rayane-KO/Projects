;;;--------------------GAME ADT--------------------;;;

(define (make-game)
  (let* ((draw-adt (make-draw))
         (current-config 1)
         (past-config -1)
         (level-adt (make-level current-config))
         (score (make-score)))
    ;;
    ;;de message die je meegeeft aan deze procedure zal ofwel zorgen dat we naar de volgende level gaan of het hele spel herstarten
    ;;
    (define (switch-level! message . wanted-config)
      (cond
        ((eq? message 'next) (set! current-config (+ current-config 1)))
        ((eq? message 'reset) (score 'reset-current-score!))
        ((eq? message 'restart) (set! current-config 1)
                                (score 'reset-current-score!)
                                (score 'reset-lives!)
                                (score 'reset-extra-lives!)
                                (draw-adt 'add-drawables-after-lost))
        ((eq? message 'go-to-next-level) (set! current-config (car wanted-config))))
      (draw-adt 'clear-level!)
      ((draw-adt 'reset-tiles!) (level-adt 'fire-ant-object) (level-adt 'scorpion-objects))
      (set! level-adt (make-level current-config)))
    ;;
    ;;procedure die gaat kijken naar welke toets ingedrukt is en de juiste procedure uitvoeren
    ;;
    (define (reset-keys key)
      (define (if-key-do given-key do-procedure)
        (if (eq? key given-key)
            (do-procedure)))
      (if-key-do #\r (lambda () (switch-level! 'reset)))
      (for-each (lambda (number) (if-key-do number (lambda () (switch-level! 'go-to-next-level (char->number number))))) (list #\1 #\2 #\3))
      (if (or ((level-adt 'lost?) score) ((level-adt 'won?) 'game))
          (if-key-do #\f (lambda () (switch-level! 'restart)))))
    
    (define (key-controls type key)
      (if (eq? type 'pressed)
          (begin
            ((level-adt 'move-fire-ant!) key)
            (reset-keys key))))
    ;;
    ;;draw-end zal ofwel het lost-scherm of het won-scherm tekenen
    ;;
    (define (draw-end! end time)
      (draw-adt 'clear-all-layers!)
      (if (eq? end 'lost)
          ((draw-adt 'draw-end!) 'lost time)
          ((draw-adt 'draw-end!) 'won time)))        
    ;;
    ;;spel-lus functie
    ;;
    (define (game-loop time)
      (let* ((won-game? ((level-adt 'won?) 'game))
             (won-level? ((level-adt 'won?) 'level))
             (lost-level? ((level-adt 'lost?) score))
             (continue-level? (not lost-level?)))
        (cond
          (won-game? (draw-end! 'won time) time)
          (won-level? (switch-level! 'next))
          (lost-level? (draw-end! 'lost time))
          (continue-level? (if (not (= current-config past-config));;zal bij elke level-overgang het juiste doolhof en kamernummer tekenen
                               (begin
                                 ((draw-adt 'draw-maze!) level-adt)
                                 ((draw-adt 'draw-number!) current-config)
                                 (set! past-config current-config)))
                           ((level-adt 'update!) time)
                           ((level-adt 'update-score!) score (lambda () (switch-level! 'reset)))
                           ((draw-adt 'draw-score!) score)
                           ((draw-adt 'draw-level!) level-adt)))))
    ;;
    ;;procedure die het spel zal opstarten
    ;;
    (define (start!)
      ((draw-adt 'game-loop-function!) game-loop)
      ((draw-adt 'game-key-function!) key-controls))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-game m)
      (cond
        ((eq? m 'start!) (start!))
        (else "MESSAGE NOT UNDERSTOOD (game-adt)")))
    dispatch-game))

