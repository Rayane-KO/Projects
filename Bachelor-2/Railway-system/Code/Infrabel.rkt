#lang racket
(require "ADTs/ADT-Railway.rkt"
         "helpers/helpfunctions.rkt"
         "helpers/constants.rkt"
         "helpers/railway-graph.rkt"
         "helpers/Execution-Facade.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Infrabel                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-infrabel)

(define (make-infrabel)
  (let ((railway-infrabel (make-railway))
        (tcp-in empty)
        (tcp-out empty))
    
    (define the-listener (tcp-listen tcp-port 4 #t))
    (define connect
      (thread (lambda () (define-values (in out) (tcp-accept the-listener)) (set! tcp-in in) (set! tcp-out out))))

    (define (close-tcp)
      (kill-thread connect)
      (kill-thread listener-thread)
      (tcp-close the-listener)
      (close-input-port tcp-in)
      (close-output-port tcp-out))

    (define (eval expr)
      (let ((proc (car expr))
            (args (cdr expr)))
        (define (apply-proc procedure) (apply procedure args))
        (cond
          ((eq? proc 'switch-option!) (apply-proc switch-option!))
          ((eq? proc 'speed!) (apply-proc speed!))
          ((eq? proc 'train-inverse-direction!) (apply-proc train-inverse-direction!))
          ((eq? proc 'train-inverse-speed!) (apply-proc train-inverse-speed!))
          ((eq? proc 'train-trajectory!) (apply-proc train-trajectory!))
          ((eq? proc 'process-trajectories!) (apply-proc process-trajectories!))
          ((eq? proc 'calculate-trajectory) (apply-proc calculate-trajectory))
          ((eq? proc 'switch-position!) (apply-proc switch-position!))
          ((eq? proc 'start) (apply-proc start-railway))
          ((eq? proc 'stop) (apply-proc stop-railway))
          (else (error "proc is undefinded" proc)))))

    (define listener-thread
      (thread (lambda ()
                (let read-loop ((in tcp-in))
                  (when (connected? in)
                    (let ((msg (read in)))
                      (eval msg)))
                  (read-loop tcp-in)))))

    (define (send-nmbs msg)
      (send-tcp-msg msg tcp-out))    
    ;;
    ;;Trein
    ;;
    (define (add-train! previous-seg current-seg)
      (let ((id (make-id)))
        ((railway-infrabel 'add-train!) id previous-seg current-seg)
        (send-nmbs (list 'add-train! id previous-seg current-seg))
        (add-loco id previous-seg current-seg)))

    (define (speed train-id)
      (get-loco-speed train-id))

    (define (speed! train-id new-speed)
      (let ((goal-speed new-speed))
        (when (not (train-direction train-id)) (set! goal-speed (- goal-speed)))
        (when (not (= (speed train-id) goal-speed))
          ((railway-infrabel 'train-speed!) train-id goal-speed)
          (send-nmbs (list 'speed! train-id goal-speed))
          (set-loco-speed! train-id goal-speed))))

    (define (train-direction train-id)
      ((railway-infrabel 'train-direction) train-id))

    (define (train-inverse-direction! train-id)
      ((railway-infrabel 'train-inverse-direction!) train-id)
      (send-nmbs (list 'train-inverse-direction! train-id)))

    (define (train-inverse-speed! train-id)
      (train-inverse-direction! train-id)
      (speed! train-id (abs ((railway-infrabel 'train-speed) train-id))))

    (define (train-trajectory? train-id)
      ((railway-infrabel 'train-trajectory?) train-id))

    (define (train-trajectory! train-id trajectory)
      ((railway-infrabel 'train-trajectory!) train-id trajectory)
      (send-nmbs (list 'train-trajectory! train-id trajectory)))

    (define (train-dt-block train-id . dtb) ;deze procedure gaat de trein (laatste positie) en de detectie-blok (laatste trein) updaten
      (let ((dtb-id (if (null? dtb) (get-loco-detection-block train-id railway-infrabel) (car dtb))))
        (cond
          (dtb-id
           ((railway-infrabel 'train-curr-dtb!) train-id dtb-id)
           (send-nmbs (list 'train-curr-dtb! train-id dtb-id))
           ((railway-infrabel 'dtb-last-train!) dtb-id train-id)
           (send-nmbs (list 'dtb-last-train! dtb-id train-id))))
        dtb-id))
    ;;
    ;;Correcte detectieblok voor elke trein (hardware)
    ;;
    (define (check-switch-states trajectory) ;gaat checken of twee treinen op dezelfde dtb zijn door te kijken of de wissels op
      (define same-dtb? #t)                  ;de posities naar deze dtb
      (let check-loop ((path trajectory))
        (when (not (null? path))
          (let ((checkpoint (car path))
                (rest (cdr path)))
            (if (switch? checkpoint)
                (let* ((next-checkpoint (when (not (null? (cdr path))) (cadr path)))
                       (states (get-states checkpoint next-checkpoint)))
                  (if (pair? states)
                      (let ((high-low (cons-high-low checkpoint next-checkpoint)))
                        (if (and (= (switch-position (car high-low)) (car states)) (= (switch-position (cdr high-low)) (cdr states)))
                            (check-loop rest)
                            (set! same-dtb? #f)))
                      (if (= (switch-position checkpoint) states)
                          (check-loop rest)
                          (set! same-dtb? #f))))
                (check-loop rest)))))
      same-dtb?)

    (define (find-closest-dtb current-dtb occupied-dtbs hash) ; zoekt dichtste bezette dtb
      (let ((closest-dtb #f)
            (min-distance +inf.0))
        (for-each (lambda (dtb)
                    (let* ((path (railway-shortest-path current-dtb dtb))
                           (path-length (length path)))
                      (when (and (< path-length min-distance)
                                 (not (hash-has-key? hash dtb)))
                        (set! min-distance path-length)
                        (set! closest-dtb dtb))))
                  occupied-dtbs)
        (cons closest-dtb min-distance)))

    (define (check-dtb)
      (let ((occupied-dtbs (get-occupied-detection-blocks))
            (trains (railway-infrabel 'trains))
            (trains-dtb (make-hash)))
        (when (not (null? occupied-dtbs))
          (cond
            ((= (length (railway-infrabel 'trains)) (length occupied-dtbs))
             (for-each (lambda (train)
                         (let* ((current-dtb (train 'curr-dtb))
                                (closest-dtb (find-closest-dtb current-dtb occupied-dtbs trains-dtb))
                                (dtb-id (car closest-dtb))
                                (min-distance (cdr closest-dtb)))
                           (when dtb-id
                             (if (hash-has-key? trains-dtb dtb-id)
                                 (let* ((assignment (hash-ref trains-dtb dtb-id))
                                        (train-id (car assignment))
                                        (distance (cdr assignment)))
                                   (when (< min-distance distance)
                                     (hash-set! trains-dtb dtb-id (train 'id))))
                                 (hash-set! trains-dtb dtb-id (cons (train 'id) min-distance))))))
                       trains)
             (hash-map trains-dtb (lambda (key value) (train-dt-block (car value) key))))
            ((= (length occupied-dtbs) 1) ; als er maar 1 detectieblok bezet is en meerdere treinen, dan kan het zijn dat
             (for-each (lambda (train)    ; één van de treinen niet gedetecteerd wordt of ze zijn op dezelfde detectieblok
                         (let* ((current-dtb (train 'curr-dtb))
                                (dtb (car occupied-dtbs))
                                (path (railway-shortest-path current-dtb dtb))
                                (same-dtb? (check-switch-states path)))
                           (when same-dtb?
                             (train-dt-block (train 'id) dtb))))
                       trains))))))
    ;;
    ;;Dynamische snelheden
    ;;
    (define (dead-end? dtb-id)
      (let ((string (symbol->string dtb-id)))
        (and (or (eq? (string-ref string 0) #\2)
                 (eq? dtb-id '1-8))
             (not (eq? dtb-id '2-3))
             (not (eq? dtb-id '2-4)))))

    (define (long? dtb-id) ;controleert of de trein zich op een lang stuk spoor bevindt
      (let ((string (symbol->string dtb-id)))
        (and (eq? (string-ref string 0) #\1)
             (not (eq? dtb-id '1-7))
             (not (eq? dtb-id '1-4))
             (not (eq? dtb-id '1-8)))))
    
    (define (special-track? train-id) ;controleert of de trein, naar het speciale doodlopend stuk spoor (zonder detectieblok), rijdt
      (and (or (eq? ((railway-infrabel 'train-curr-dtb) train-id) '2-3) (eq? ((railway-infrabel 'train-curr-dtb) train-id) '2-4))
           ((railway-infrabel 'train-curr-dtb) train-id)
           (= (switch-position 'S-10) 2)
           (= (switch-position 'S-11) 2)))

    (define (special-track-timer train-id)
      (let* ((prev-dtb ((railway-infrabel 'train-prev-dtb) train-id))
             (curr-dtb ((railway-infrabel 'train-curr-dtb) train-id))
             (prev-timer (id->timer prev-dtb))
             (curr-timer (id->timer curr-dtb))
             (delay 5))
        (- (+ prev-timer curr-timer) delay)))
        

    (define (trains-on-dtb train-id dtb-id) ; geeft alle treinen op een detectieblok
      (let ((trains '()))
        (when dtb-id (for-each (lambda (train)
                                 (when (and (eq? ((railway-infrabel 'train-curr-dtb) (train 'id)) dtb-id)
                                            (not (eq? (train 'id) train-id))) (set! trains (cons train trains)))) (railway-infrabel 'trains)))
        trains))
    
    (define (dynamic-speed train-id) ;dynamische snelheden worden bepaald door het soort spoor waarop de trein rijdt
      (let ((speed-thread 'empty)    ;zal continu checken of de snelheid gewijzigd moet worden
            (curr-dtb ((railway-infrabel 'train-curr-dtb) train-id))
            (prev-dtb ((railway-infrabel 'train-prev-dtb) train-id))
            (on? #f))

        (define (set-speed! speed)
          (speed! train-id speed))

        (define (calculate-optimal-speed dtb)
          (let* ((other-trains (trains-on-dtb train-id dtb))
                 (nr-of-trains (length other-trains))
                 (trains-speed (map (lambda (train) (speed (train 'id))) other-trains))
                 (min-speed (if (null? trains-speed) #f (apply min trains-speed)))
                 (train-delay (* nr-of-trains 10))
                 (curr-speed (speed train-id)))
            (define optimal-speed
              (if dtb
                  (cond
                    ((long? dtb) (- max-speed train-delay))
                    (else (- slow-speed train-delay)))
                  slow-speed))
            (if min-speed ; hou rekening met de snelheid van andere treinen
                (if (= min-speed optimal-speed) optimal-speed min-speed)
                optimal-speed)))
          
        (define (speed-control)
          (let* ((dtb ((railway-infrabel 'train-curr-dtb) train-id))
                 (optimal-speed (calculate-optimal-speed dtb)))
            (set-speed! optimal-speed)))

        (define (start!)
          (if (long? curr-dtb) (set-speed! max-speed) (set-speed! slow-speed))
          (cond ((not on?) (set! speed-thread (thread (lambda () (let loop () (speed-control) (loop))))) (set! on? #t))))           
          
        (define (dispatch m)
          (cond
            ((eq? m 'start!) (start!))
            ((eq? m 'stop!) (set-speed! zero-speed) (kill-thread speed-thread) (set! on? #f))
            ((eq? m 'on?) on?)))
        dispatch))          
    ;;
    ;;Trajecten (Automatisch & Tijdstabellen)
    ;;
    (define new-trajectory '())

    (define (calculate-trajectory trajectory)
      (set! new-trajectory (car trajectory)))

    (define reservations all-nodes) ;simpel systeem van reservaties zodat 2 treinen tegelijk een automatisch
                                    ;berekend traject kunnen afleggen
    (define (get-reservation el)
      (node-ref reservations el))

    (define (make-reservation! el train-id)
      (node-set! reservations el train-id))

    (define (reserve! el train-id)
      (make-reservation! el train-id))

    (define (unreserve! el)
      (make-reservation! el #f))

    (define (reserve-traject! train-id trajectory)
      (for-each (lambda (checkpoint)
                  (when (get-reservation checkpoint)
                    (let wait-loop ((reservation (get-reservation checkpoint)))
                      (when reservation (wait-loop (get-reservation checkpoint)))))
                  (reserve! checkpoint train-id)) trajectory))

    (define (unreserve-prev! last-dtb trajectory)
      (let unreserving-loop ((path trajectory))
        (let ((checkpoint (car path))
              (rest (cdr path)))
          (when (not (eq? checkpoint last-dtb))
            (unreserve! checkpoint)
            (unreserving-loop rest)))))

    (define (unreserve-all! trajectory)
      (for-each (lambda (checkpoint)
                  (when (or (dtb? checkpoint) (switch? checkpoint))
                    (unreserve! checkpoint))) trajectory))

    (define (wait-for-free-reservation checkpoint train-id dynamic-speed)
      (when (not (eq? (get-reservation checkpoint) train-id))
        (dynamic-speed 'stop!)
        (let wait-loop ((reservation (get-reservation checkpoint)))
          (when reservation (wait-loop (get-reservation checkpoint))))
        (dynamic-speed 'start!)))

    (define (automatic-switch-position! checkpoint next-checkpoint)
      (let ((state (get-states checkpoint next-checkpoint)))
        (if (not (pair? state))
            (switch-position! checkpoint state)
            (let ((high-low (cons-high-low checkpoint next-checkpoint)))
              (switch-position! (car high-low) (car state))
              (switch-position! (cdr high-low) (cdr state))))))

    (define (execute-time-tbl checkpoint train-id dynamic-speed)
      (if (pair? checkpoint)
          (let ((id (car checkpoint))
                (instruction (cdr checkpoint)))
            (if (dtb? id)
                (let destination-loop ()
                  (if (eq? ((railway-infrabel 'train-curr-dtb) train-id) id)
                      (begin
                        (dynamic-speed 'stop!))
                      (destination-loop)))
                (begin
                  (switch-position! id instruction))))
          (cond
            ((eq? checkpoint '+start) (dynamic-speed 'start!))
            ((eq? checkpoint '-start) (dynamic-speed 'start!) (train-inverse-speed! train-id)))))

    (define (check-start-direction path train-id)
      (when (not (null? (cdr path)))
        (when (or (member ((railway-infrabel 'train-prev-dtb) train-id) path) (dead-end? ((railway-infrabel 'train-curr-dtb) train-id)))
          (train-inverse-speed! train-id))))

    (define (automatic-start! path train-id dynamic-speed)
      (reserve-traject! train-id path)
      (dynamic-speed 'start!)
      (check-start-direction path train-id))

    (define (end-trajectory! automatic? complete-trajectory train-id dynamic-speed)
      (dynamic-speed 'stop!)
      (when automatic? (unreserve-all! (cdr complete-trajectory)))
      (train-trajectory! train-id '()))

    (define (make-trajectory! train-path automatic?) ;uitstippelen van het traject
      (let ((calc-thread 'empty))
        (define (stop)
          (kill-thread calc-thread))
        (set! calc-thread
              (thread (lambda ()
                        (let loop ((path train-path))
                          (let* ((train-id (car path))
                                 (dynamic-speed (dynamic-speed train-id))
                                 (complete-trajectory path)
                                 (last (list-ref complete-trajectory (- (length complete-trajectory) 1)))
                                 (destination (if (pair? last) (car last) last))
                                 (special-path? #t))
                            (define (check-reset!)
                              (when (not (train-trajectory? train-id))
                                (dynamic-speed 'stop!)
                                (stop)))
                            (when automatic? (automatic-start! (cdr path) train-id dynamic-speed))
                            (let trajectory-loop ((trajectory (cdr path)))
                              (when (not (null? trajectory))
                                (let ((checkpoint (car trajectory))
                                      (next-checkpoint (when  (not (null? (cdr trajectory))) (cadr trajectory)))
                                      (rest-trajectory (cdr trajectory)))
                                  (check-reset!)
                                  (cond
                                    (automatic?
                                     (wait-for-free-reservation checkpoint train-id dynamic-speed)
                                     (cond
                                       ((dtb? checkpoint) (let checkpoint-loop () ; loop tot je op de juiste detectieblok bent
                                                            (check-reset!)        ; maar als je een fout traject volgt dan volg je het nieuw traject
                                                            (unreserve-prev! checkpoint (cdr complete-trajectory))
                                                            (when (not (eq? ((railway-infrabel 'train-curr-dtb) train-id) checkpoint))
                                                              (cond
                                                                ((not (member ((railway-infrabel 'train-curr-dtb) train-id) complete-trajectory))
                                                                 (send-nmbs (list 'calculate-trajectory train-id (list-ref complete-trajectory (- (length complete-trajectory) 1))))
                                                                 (sleep tcp-time)
                                                                 (when (not (member ((railway-infrabel 'train-prev-dtb) train-id) new-trajectory)) (train-inverse-speed! train-id))
                                                                 (unreserve-all! (cdr complete-trajectory))
                                                                 (set! complete-trajectory new-trajectory)
                                                                 (set! rest-trajectory (cdr complete-trajectory))
                                                                 (reserve-traject! train-id rest-trajectory))
                                                                ((and (special-track? train-id) special-path?) (sleep (special-track-timer train-id)) (train-inverse-speed! train-id) (set! special-path? #f) (checkpoint-loop))
                                                                (else (checkpoint-loop))))))
                                       ((switch? checkpoint) (automatic-switch-position! checkpoint next-checkpoint))))
                                    (else (execute-time-tbl checkpoint train-id dynamic-speed)))
                                  (if (eq? ((railway-infrabel 'train-curr-dtb) train-id) destination)
                                      (begin
                                        (end-trajectory! automatic? complete-trajectory train-id dynamic-speed)
                                        (stop))
                                      (trajectory-loop rest-trajectory))))))))))))

    (define (process-trajectories! trajectories automatic?)
      (for-each (lambda (trajectory)
                  (make-trajectory! trajectory automatic?))
                trajectories))                  
    ;;
    ;;Detectie-blok
    ;;
    (define (make-dt-blocks!)
      (let ((dt-block-ids (get-detection-block-ids)))
        ((railway-infrabel 'make-detection-blocks!) dt-block-ids)
        (send-nmbs (list 'make-detection-blocks! dt-block-ids))))

    (define (dtb-last-train dtb-id)
      ((railway-infrabel 'dtb-last-train) dtb-id))
    ;;
    ;;update
    ;;
    (define (update) ;voorlopig zorgt update enkel voor het updaten van de vorige dt-block van elke trein
      (let loop ()
        (if sim?
            (for-each (lambda (train) (train-dt-block (train 'id))) (railway-infrabel 'trains))
            (check-dtb))
        (sleep loop-time)
        (loop)))

    (define dtb-update-thread 'empty)

    (define (activate!)
      (set! dtb-update-thread (thread (lambda () (update)))))
    ;;
    ;;Wissels
    ;;
    (define (make-switches!)
      (let ((switch-ids (get-switch-ids)))
        ((railway-infrabel 'make-switches!) switch-ids)
        (send-nmbs (list 'make-switches! switch-ids))))

    (define (switch-position switch-id)
      (if (eq? switch-id 'S-2-3) ;driewegwissel is de enige wissel met 3 posities
          (let ((S-2-pos (get-switch-position 'S-2))
                (S-3-pos (get-switch-position 'S-3)))
            (cond
              ((and (= S-2-pos 2) (= S-3-pos 2)) 3)
              ((and (= S-2-pos 2) (= S-3-pos 1)) 2)
              (else 1)))
          (get-switch-position switch-id)))

    (define (switch-position! switch-id new-state)
      (if (eq? switch-id 'S-2-3) ;driewegwissel is de enige wissel met 3 posities
          (cond
            ((= new-state 1) (set-switch-position! 'S-2 1))
            ((= new-state 2) (set-switch-position! 'S-2 2) (set-switch-position! 'S-3 1))
            ((= new-state 3) (set-switch-position! 'S-2 2) (set-switch-position! 'S-3 2)))
          (set-switch-position! switch-id new-state))
      ((railway-infrabel 'switch-position!) switch-id new-state)
      (send-nmbs (list 'switch-position! switch-id new-state)))

    (define (start-railway setup train-list) ;afhankelijk van setup starten we met een andere configuratie van het spoor
      (cond
        ((eq? setup 'setup-hardware) (setup-hardware))
        ((eq? setup 'setup-loop) (setup-loop))
        ((eq? setup 'setup-loop-and-switches) (setup-loop-and-switches))
        ((eq? setup 'setup-straight) (setup-straight))
        ((eq? setup 'setup-straight-with-switch) (setup-straight-with-switch))
        (else (error "Undefined setup: " setup)))
      (for-each (lambda (segments) (add-train! (prev-segment segments) (curr-segment segments))) train-list)
      (make-dt-blocks!)
      (make-switches!)
      (start)
      (activate!))

    (define (stop-railway)
      (railway-infrabel 'reset!)
      (kill-thread dtb-update-thread)
      (stop)
      (close-tcp))

    (define (dispatch m)
      (cond
        ((eq? m 'start) start-railway)
        ((eq? m 'stop) (stop-railway))))
    dispatch))
