#lang racket
(require "ADTs/GUI.rkt")
(require "ADTs/ADT-Railway.rkt")
(require "constants.rkt")
(require "helpfunctions.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              NMBS                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-nmbs)

(define (make-nmbs infrabel) ;voorlopig neemt nmbs infrabel als argument
  (let ((gui (make-gui))
        (railway-nmbs (make-railway)))    ;later zullen ze via TCP communiceren (en in aparte processen draaien)

    ;;callbacks (bevatten de procedures die de gui nodig heeft) die worden meegegeven aan de gui
    ;;wanneer we de staat van een component willen wijzigen dan doen we dat in infrabel, om een waarde te lezen
    ;;vraag je dat aan de railway van nmbs

    (define (switch? id)
      (if (symbol? id)
          (eq? (string-ref (symbol->string id) 0) #\S)
          (eq? (string-ref id 0) #\S)))

    (define (dtb? id)
      (let ((frst (char->number (string-ref id 0))))
        (or (= frst 1)
            (= frst 2))))
    ;;
    ;trein callback
    ;;
    (define (train-callback train-id)
      (define (speed! new-speed)
        (let ((train-direction ((railway-nmbs 'train-direction) train-id)))
          (if train-direction
              ((infrabel 'speed!) train-id new-speed)
              ((infrabel 'speed!) train-id (- new-speed)))))

      (define (long? dtb-id) ;controleert of de trein zich op een lang stuk spoor bevindt
        (let ((string (symbol->string dtb-id)))
          (and (eq? (string-ref string 0) #\1)
               (not (eq? dtb-id '1-7))
               (not (eq? dtb-id '1-4))
               (not (eq? dtb-id '1-8)))))

      (define (dead-end? dtb-id) ;controleert of de trein zich op een doodlopend stuk spoor bevindt
        (let ((string (symbol->string dtb-id)))
          (and (or (eq? (string-ref string 0) #\2)
                   (eq? dtb-id '1-8))
               (not (eq? dtb-id '2-3))
               (not (eq? dtb-id '2-4)))))
      
      (define (special-track?) ;controleert of de trein, naar het speciale doodlopend stuk spoor (zonder detectieblok), rijdt 
        (let ((switch-procs (switch-callback)))
          (and (or (eq? ((railway-nmbs 'train-prev-dtb) train-id) '2-3) (eq? ((railway-nmbs 'train-prev-dtb) train-id) '2-4))
               ((railway-nmbs 'train-direction) train-id)
               (= ((switch-procs 'switch-position) 'S-10) 2)
               (= ((switch-procs 'switch-position) 'S-11) 2))))

      (define (dynamic-speed) ;dynamische snelheden worden bepaald door het soort spoor waarop de trein rijdt
        (let ((speed-thread 'empty) ;zal continu checken of de snelheid gewijzigd moet worden
              (on? #f))

          (define (speed-changer! current-speed end-speed adder)
            (let ((speed (abs current-speed)))
              (cond ((not (= speed end-speed)) (speed! (adder current-speed 1))))))
          
          (define (speed-up speed)
            (speed-changer! speed max-speed +))
          
          (define (slow-down speed)
            (speed-changer! speed slow-speed -))
          
          (define (speed-control)
            (let ((dtb ((infrabel 'train-dt-block) train-id))
                  (speed ((railway-nmbs 'train-speed) train-id)))
              (if dtb                                         ;als een trein zich niet op een dtb bevindt, dan zal het moeten vertragen
                  (cond ((long? dtb) (speed-up speed))
                        ((dead-end? dtb) (speed! zero-speed))
                        ((special-track?) (sleep special-track-timer) (speed! zero-speed))
                        (else (slow-down speed)))
                  (slow-down speed))))

          (define (start!)
            (speed! slow-speed)
            (cond ((not on?) (set! speed-thread (thread (lambda () (let loop () (speed-control) (loop))))))))
          
          (define (dispatch m)
            (cond
              ((eq? m 'start!) (start!))
              ((eq? m 'stop!) (speed! 0) (kill-thread speed-thread))))
          dispatch))
    
      (define (dispatch m)
        (cond
          ((eq? m 'id) train-id)
          ((eq? m 'speed) ((railway-nmbs 'train-speed) train-id))
          ((eq? m 'speed!) speed!)
          ((eq? m 'direction) ((railway-nmbs 'train-direction) train-id))
          ((eq? m 'inverse-direction!) ((infrabel 'train-inverse-direction!) train-id))
          ((eq? m 'trajectory?) ((railway-nmbs 'train-trajectory?) train-id))
          ((eq? m 'trajectory!) ((infrabel 'train-trajectory!) train-id))
          ((eq? m 'dt-block) ((infrabel 'train-dt-block) train-id))
          ((eq? m 'dynamic-speed) (dynamic-speed))))
      dispatch)
    ;;
    ;;dt-block callback
    ;;
    (define (dtb-callback)
      (define (dispatch m)
        (cond
          ((eq? m 'last-train) (railway-nmbs 'dtb-last-train))
          ((eq? m 'list) (railway-nmbs 'detection-blocks))
          ((eq? m 'trains) (railway-nmbs 'trains))))
      dispatch)
    ;;
    ;;switch callback
    ;;
    (define (switch-callback)
      (define (dispatch m)
        (cond
          ((eq? m 'switch-position) (railway-nmbs 'switch-position))
          ((eq? m 'switch-position!) (infrabel 'switch-position!))
          ((eq? m 'list) (railway-nmbs 'switches))))
      dispatch)
    ;;
    ;;timetable FASE 2
    ;;
    (define (time-tbl-callback)
      (define (read filename)
        (open-input-file filename))
      
      (define (cut-last string) ;wanneer het een lijn is er steeds op het einde een spatie dat we niet nodig hebben
        (substring string 0 (- (string-length string) 1)))
      
      (define (follow-trajectory! filename)
        (let* ((f (read filename))
               (train-procs (train-callback (string->symbol (cut-last (read-line f)))))
               (switch-procs (switch-callback))
               (calc-thread 'empty)
               (dynamic-speed (train-procs 'dynamic-speed))
               (component-start 0)
               (component-end 3)
               (action-length 4)
               (order-length 6))
          (define (get-component line start end)
            (string->symbol (substring line start end)))
          (define (get-switch-pos line end)
            (char->number (string-ref line (+ end 1))))
          (define (get-action line end)
            (let ((action-start (+ end 1)))
              (substring line action-start (+ action-start action-length))))
          (define (get-order line)
            (substring line 0 order-length))
          (dynamic-speed 'start!)
          (train-procs 'trajectory!)
          (set! calc-thread
                (thread
                 (lambda ()
                   (for ([line (in-lines f)])
                     (cond ((eq? (string-ref line 3) #\/) (set! component-end 3))
                           ((eq? (string-ref line 4) #\/) (set! component-end 4))
                           ((eq? (string-ref line 5) #\/) (set! component-end 5)))
                     (cond
                       ((switch? line) ((switch-procs 'switch-position!) (get-component line component-start component-end) (get-switch-pos line component-end)))
                       ((dtb? line) (cond ((string=? (get-action line component-end) "stop")
                                           (let destination-loop ()
                                             (if (eq? (train-procs 'dt-block) (get-component line component-start component-end))
                                                 (dynamic-speed 'stop!)
                                                 (destination-loop))))))
                       ((string=? (get-order line) "+start") (dynamic-speed 'start))
                       ((string=? (get-order line) "-start") (train-procs 'inverse-direction!) (dynamic-speed 'start))
                       ((string=? (get-order line) "finish")
                        (train-procs 'trajectory!)
                        (let ((next (read-line f))) (if (eof-object? next)
                                                        (begin
                                                          (kill-thread calc-thread))
                                                        (begin
                                                          (set! train-procs (train-callback (string->symbol (cut-last next))))
                                                          (set! dynamic-speed (train-procs 'dynamic-speed))
                                                          (dynamic-speed 'start!)
                                                          (train-procs 'trajectory!))))))))))))
      (define (dispatch m)
        (cond
          ((eq? m 'follow!) follow-trajectory!)))
      dispatch)
    ;;
    ;;start
    ;;
    (define (start)
      ((gui 'start) (lambda ()
                      ((infrabel 'start) 'setup-hardware (list (cons 'S-26 '1-4) (cons '1-4 '1-5) (cons '1-5 'S-20)))
                      ;((infrabel 'start) 'setup-loop-and-switches (list (cons 'D3 'D2) (cons 'D5 'D6)))
                      ;((infrabel 'start) 'setup-loop (list (cons 'D3 'D2) (cons 'D5 'D6)))
                      ;((infrabel 'start) 'setup-straight-with-switch (list (cons 'A1 'D1)))
                      ;((infrabel 'start) 'setup-straight (list (cons 'A1 'D1)))
                      ((gui 'draw-trains) train-callback (railway-nmbs 'trains))
                      ((gui 'draw-dt-blocks) (dtb-callback))
                      ((gui 'draw-switches) (switch-callback))
                      ((gui 'draw-time-tbl) (time-tbl-callback)))))
    ;;
    ;;stop
    ;;
    (define (stop)
      ((gui 'exit) (lambda ()
                     (infrabel 'stop))))
    ;;
    ;;dispatch
    ;;
    (define (dispatch m)
      (cond
        ((eq? m 'start) (start))
        ((eq? m 'stop) (stop))
        ((eq? m 'railway) railway-nmbs)))
    dispatch))