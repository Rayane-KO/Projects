#lang racket
(require racket/gui/base)
(require "../helpfunctions.rkt")
(require "../constants.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             GUI                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-gui)

(define (make-gui)
  (let ((window-width 200)
        (window-height 200)
        (window-x 1000)
        (window-y 50)
        (sim-width 500)
        (sim-height 600))
    
    (define (open-frame frame)
      (send frame show #t))
    (define (close-frame frame)
      (send frame show #f))
    
    (define (start callback) (begin (simulator-button callback) (open-frame main-frame)))
    (define (stop) (close-frame simulator-frame))
    
    (define main-frame (new frame%
                            [label "Railway System"]
                            [width window-width]
                            [height window-height]
                            [x window-x]
                            [y window-y]
                            [style '(no-resize-border)]
                            [alignment '(center top)]))

    (define choice-msg (new message%
                            [parent main-frame]
                            [label "Choose an option:"]))

    (define choice-panel (new horizontal-panel%
                              [parent main-frame]
                              [alignment '(center center)]))

    ;sim-callback bevat alle procedures die uitgevoerd zullen worden wanneer de GUI opgestart wordt
    (define (simulator-button sim-callback) (new button%                           
                                                 [label "Simulator"]
                                                 [parent choice-panel]
                                                 [callback (lambda (button event)
                                                             (sim-callback)
                                                             (open-frame simulator-frame)
                                                             (close-frame main-frame))]
                                                 [horiz-margin 20]))

    ;voorlopig zal de hardware-button niets doen
    (define hardware-button (new button%
                                 [label "Hardware"]
                                 [parent choice-panel]
                                 [horiz-margin 20]))

    (define simulator-frame (new frame%
                                 [label "Railway System - Simulator"]
                                 [width sim-width]
                                 [height sim-height]
                                 [x window-x]
                                 [y window-y]
                                 [stretchable-width #t]
                                 [style '(no-resize-border)]
                                 [alignment '(center top)]))

    (define change? #t) ;variabele die zal helpen om steeds de juiste componenten te tekenen
    ;wanneer ik van tab verander in de tab-panel

    (define tab-panel (new tab-panel%
                           [parent simulator-frame]
                           [choices '("&Control" "&Log")]
                           [callback (lambda (b e) (switch-tab))]))
    
    (define (switch-tab)
      (if change?
          (begin
            (send tab-panel delete-child command-panel)
            (send tab-panel add-child msg-panel))
          (begin
            (send tab-panel add-child command-panel)
            (send tab-panel delete-child msg-panel)))
      (set! change? (not change?)))

    (define command-panel (new vertical-panel%
                               [parent tab-panel]))
    
    (define msg-panel (new vertical-panel%
                           [parent tab-panel]
                           [style (list 'deleted 'auto-vscroll)]
                           [horiz-margin 10]
                           [alignment '(left top)]))
    ;;
    ;;Onderdelen voor het tekenen van de controls van de trein
    ;;    
    (define train-panel (new horizontal-panel%
                             [parent command-panel]
                             [vert-margin 2]
                             [horiz-margin 2]
                             [style '(border)]
                             [border 1]
                             [stretchable-width #t]
                             [min-height 250]))
    
    ;current-dtb zal de laatste detectie-blok bijhouden zodat de message enkel geupdated
    ;wordt wanneer de trein een nieuwe, vorige detectie-blok heeft
    (define current-dtb #f)
    
    ;thread die zorgt voor het updaten van de message (laatste dt-block)
    (define dtb-msg-thread 'empty)
    
    ;lijst van threads bijhouden want voor elke trein maken we een nieuwe thread aan
    (define thread-list '())
    ;;
    ;;draw-train
    ;;
    (define (draw-train train-callback)             ;main-panel
      (let* ((main-panel (new horizontal-panel%     ;*******************;
                              [parent train-panel]  ; ******** ******** ; 
                              [vert-margin 2]       ; * info * *slider* ;
                              [horiz-margin 2]      ; *      * *      * ;
                              [style '(border)]))   ; *      * *      * ;  
             (info-panel (new vertical-panel%       ; *      * *      * ;
                              [parent main-panel]   ; *      * *      * ;
                              [vert-margin 2]       ; ******** ******** ;
                              [horiz-margin 2])))   ;*******************;
    
        (new message%
             [parent info-panel]
             [label (symbol->string (train-callback 'id))])

        (define start-button
          (new button%
               [parent info-panel]
               [label "Start"]
               [enabled #t]
               [callback (lambda (button event)
                           (send slider set-value max-speed)
                           (send text-field set-value (number->string max-speed))
                           (send start-button enable #f)
                           (send stop-button enable #t)
                           (save-action " started!")
                           (direction-speed! max-speed))]))

        (define (direction-speed! new-speed) ;wanneer de snelheid van de trein verandert, controleren we eerst in welke richting we gaan
          (if (train-callback 'direction)
              ((train-callback 'speed!) new-speed)
              ((train-callback 'speed!) (- new-speed))))

        (define stop-button
          (new button%
               [parent info-panel]
               [label "Stop"]
               [enabled #f]
               [callback (lambda (button event)
                           (send slider set-value 0)
                           (send text-field set-value "0")
                           (send start-button enable #t)
                           (send stop-button enable #f)
                           (save-action " stopped!")
                           ((train-callback 'speed!) 0))]))

        (define inverse-button
          (new button%
               [parent info-panel]
               [label "Inverse direction"]
               [callback (lambda (button event) (train-callback 'inverse-direction!) (save-action " changed direction!"))]))
        
        (define prev-msg
          (begin
            (new message%
                 [parent info-panel]
                 [label "Previous detection:"])))
        
        (define prev-dtb-msg
          (new message%
               [parent info-panel]
               [label (if (train-callback 'dt-block)                      ;als de trein in het begin niet op een detectie-blok staat,
                          (symbol->string (train-callback 'dt-block))
                          "none")])) ;dan geven we none terug als 'vorige detectie-blok'
    
        (define slider-panel
          (new horizontal-panel%
               [parent main-panel]
               [vert-margin 2]
               [horiz-margin 2]))

        (define slider
          (new slider%
               [parent slider-panel]
               [label "Speed"]
               [max-value max-speed]
               [min-value zero-speed]
               [init-value (train-callback 'speed)]
               [style '(vertical vertical-label plain)]
               [callback (lambda (button event)
                           (let ((new-speed (send slider get-value)))
                             (send text-field set-value (number->string new-speed))
                             (send start-button enable #f)
                             (send stop-button enable #t)
                             (direction-speed! new-speed)))]))

        (define text-field
          (new text-field%
               [parent slider-panel]
               [label #f]
               [init-value "0"]
               [min-width 30]
               [style '(single)]
               [stretchable-width #f]
               [callback (lambda (button event)
                           (let ((new-value (check-value)))
                             (send slider set-value new-value)
                             ((train-callback 'speed!) new-value)))]))

        (define (check-value)
          (let* ((value-str (send text-field get-value))
                 (value (string->number value-str)))
            (cond
              ((not value) zero-speed)
              ((> value max-speed) max-speed)
              ((< value zero-speed) zero-speed)
              (else value))))

        (define (save-action action)
          (let* ((id (symbol->string (train-callback 'id)))
                 (train (string-append (get-time) " : Train " id)))
            (new message%
                 [parent msg-panel]
                 [label (string-append train action)])))

        (define enabled? #t)
        (define disabled? #f)

        (define (enable-controls!)
          (cond ((not enabled?)
                 (send start-button enable #t)
                 (send stop-button enable #f)
                 (send inverse-button enable #t)
                 (send slider enable #t)
                 (set! enabled? #t)
                 (set! disabled? #f))))

        (define (disable-controls!)
          (cond ((not disabled?)
                 (send start-button enable #f)
                 (send stop-button enable #f)
                 (send inverse-button enable #f)
                 (send slider enable #f)
                 (set! enabled? #f)
                 (set! disabled? #t))))
        
        (define (update)
          (let ((last-dtb (train-callback 'dt-block))
                (speed (train-callback 'speed)))
            (cond (last-dtb
                   (cond ((not (eq? current-dtb last-dtb))
                          (send prev-dtb-msg set-label (symbol->string last-dtb)) (set! current-dtb last-dtb)))))
            (if (train-callback 'trajectory?)
                (begin
                  (send text-field set-value (number->string speed))
                  (disable-controls!))
                (enable-controls!))))
               
        (set! dtb-msg-thread (thread (lambda () (let loop ()
                                                  (update)
                                                  (loop)))))
        (set! thread-list (cons dtb-msg-thread thread-list))
        'ok))

    (define (draw-trains train-callback trains)
      (for-each (lambda (train) (draw-train (train-callback (train 'id)))) trains))
    ;;
    ;;draw switches
    ;;
    (define comp-panel (new horizontal-panel%
                            [parent command-panel]))

    (define (draw-switches switch-callback)
      (let ((switch-panel (new horizontal-panel%
                               [parent comp-panel]
                               [vert-margin 5]
                               [horiz-margin 4]
                               [min-height 100]
                               [min-width 250]
                               [alignment '(center center)]
                               [style '(border)]
                               [border 1])))
        
        ;wanneer we een bepaalde switch kiezen dan wordt de huidige positie in de radio-box weergegeven
        (define choice
          (new choice%
               [label "Switch: "]
               [choices (map (lambda (switch) (symbol->string (switch 'id))) (switch-callback 'list))]
               [callback (lambda (button event) (check-3-way) (check-position))]
               [horiz-margin 20]
               [vert-margin 8]
               [min-width 100]
               [stretchable-width #f]
               [parent switch-panel]))
        
        ;waneer je een positie kiest in de radio-box, dan wordt de positie van de switch ook veranderd
        (define radio-box
          (new radio-box%
               [parent switch-panel]
               [label #f]
               [choices '("Position 1" "Position 2" "Position 3")]
               [callback (lambda (button event)
                           (let* ((selected-position (send radio-box get-selection))
                                  (current-position (+ selected-position 1))) ;selected-position start bij 0 terwijl de states van de switchen bij 1 beginnen
                             ((switch-callback 'switch-position!) (string->symbol (send choice get-string-selection)) current-position)
                             (save-action (string-append " position changed to " (number->string current-position)))))]))

        ;als we met een three-way switch hebben geselecteerd dan wordt position 3 bruikbaar
        (define (check-3-way)
          (if (eq? (string->symbol (send choice get-string-selection)) 'S-2-3) 
              (send radio-box enable 2 #t)
              (send radio-box enable 2 #f)))

        ;geeft de positie weer van een gekozen switch
        (define (check-position)
          (let* ((switch-id (string->symbol (send choice get-string-selection)))
                 (current-pos ((switch-callback 'switch-position) switch-id)))
            (send radio-box set-selection (- current-pos 1))))
        (send radio-box enable 2 #f)
        
        (define (save-action action)
          (let* ((id (send choice get-string-selection))
                 (switch (string-append (get-time) ": Switch " id)))
            (new message%
                 [parent msg-panel]
                 [label (string-append switch action)])))
        'ok))
    ;;
    ;;draw dt-blocks
    ;;
    (define current-train #f)

    (define train-msg-thread 'empty)

    (define (draw-dt-blocks dt-block-callback)
      (let ((dt-panel (new horizontal-panel%
                           [parent comp-panel]
                           [vert-margin 5]
                           [horiz-margin 4]
                           [min-height 100]
                           [min-width 250]
                           [alignment '(center center)]
                           [style '(border)]
                           [border 1])))
        
        (define choice
          (new choice%
               [label "Detection-block: "]
               [choices (map (lambda (dt-block) (symbol->string (dt-block 'id))) (dt-block-callback 'list))]
               [horiz-margin 10]
               [vert-margin 8]
               [min-width 100]
               [stretchable-width #f]
               [parent dt-panel]))
      
        (define current-msg (new message%
                                 [parent dt-panel]
                                 [label "Last:"]))
        
        (define train-msg (new message%
                               [parent dt-panel]
                               [label (symbol->string ((dt-block-callback 'last-train)(string->symbol (send choice get-string-selection))))]
                               [auto-resize #t]))
        
        (define (update-msg)
          (let ((dt-block (send choice get-string-selection)))
            (cond
              (dt-block
               (let ((last-train ((dt-block-callback 'last-train) (string->symbol dt-block))))
                 (cond ((not (eq? current-train last-train))
                        (send train-msg set-label (symbol->string last-train))
                        (set! current-train last-train))))))))
      
        (set! train-msg-thread (thread (lambda () (let loop ()
                                                    (update-msg)
                                                    (sleep 1) ;als er twee treinen op de detectie-blok zijn dan verandert de waarde te snel
                                                    (loop)))))
        'ok))
    ;;
    ;;Timetables
    ;;
    (define time-tbl-panel (new horizontal-panel%
                                [parent command-panel]))

    (define (draw-time-tbl time-tbl-callback)
      (let ((tbl-panel (new horizontal-panel%
                            [parent time-tbl-panel]
                            [vert-margin 5]
                            [horiz-margin 4]
                            [min-height 100]
                            [min-width 250]
                            [alignment '(center center)]
                            [style '(border)]
                            [border 1])))
        (define file-field
          (new text-field%
               [label "Name of file: "]
               [parent tbl-panel])) 
        (define start-button
          (new button%                           
               [label "Start trajectory"]
               [parent tbl-panel]
               [callback (lambda (button event) ((time-tbl-callback 'follow!) (string-append "trajectories/" (send file-field get-value))) (save-action))]
               [horiz-margin 20]))
        (define (save-action)
            (new message%
                 [parent msg-panel]
                 [label (string-append (get-time) ": A new trajectory, with name '" (send file-field get-value) "' was started!")]))
        'ok))
    
    ;;
    ;;Exit
    ;;
    (define (exit-button exit-callback) ;exit-callback bevat alle procedures die uitgevoerd moeten worden wanneer de GUI gesloten wordt
      (new button%
           [parent simulator-frame]
           [label "&Exit"]
           [callback (lambda (button event)
                       (kill-thread train-msg-thread)
                       (for-each kill-thread thread-list) ;stop alle threads die de berichten (detectie-blok) van de trein updaten
                       (close-frame simulator-frame)
                       (exit-callback))])
      'ok)
    
    ;;
    ;;Dispatch
    ;;
    (define (dispatch m)
      (cond
        ((eq? m 'start) start)
        ((eq? m 'exit) exit-button)
        ((eq? m 'draw-trains) draw-trains)
        ((eq? m 'draw-switches) draw-switches)
        ((eq? m 'draw-dt-blocks) draw-dt-blocks)
        ((eq? m 'draw-time-tbl) draw-time-tbl)
        (else (error "GUI/Message not understood: " m))))
    dispatch))
