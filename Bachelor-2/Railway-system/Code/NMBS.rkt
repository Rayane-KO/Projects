#lang racket
(require "ADTs/GUI.rkt"
         "ADTs/ADT-Railway.rkt"
         "helpers/constants.rkt"
         "helpers/helpfunctions.rkt"
         "helpers/railway-graph.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              NMBS                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-nmbs)

(define (make-nmbs)
  (let ((gui (make-gui))
        (railway-nmbs (make-railway))
        (tcp-in empty)  ;variabelen voor de communicatie via tcp
        (tcp-out empty))

    (define connect
      (thread (lambda () (define-values (in out) (tcp-connect "localhost" tcp-port)) (set! tcp-in in) (set! tcp-out out))))

    (define (close-tcp)
      (kill-thread connect)
      (kill-thread listener-thread)
      (close-input-port tcp-in)
      (close-output-port tcp-out))

    (define (eval expr) ;procedure die de inkomende berichten zal evalueren en de juiste procedure oproepen
      (let ((proc (car expr))
            (args (cdr expr)))
        (define (apply-proc procedure) (apply procedure args))
        (cond
          ((eq? proc 'add-train!) (apply-proc (railway-nmbs 'add-train!)))
          ((eq? proc 'speed!) (apply-proc (railway-nmbs 'train-speed!)))
          ((eq? proc 'train-curr-dtb!) (apply-proc (railway-nmbs 'train-curr-dtb!)))
          ((eq? proc 'dtb-last-train!) (apply-proc (railway-nmbs 'dtb-last-train!)))
          ((eq? proc 'train-inverse-direction!) (apply-proc (railway-nmbs 'train-inverse-direction!)))
          ((eq? proc 'train-trajectory!) (apply-proc (railway-nmbs 'train-trajectory!)))
          ((eq? proc 'calculate-trajectory) (apply-proc calculate-trajectory))
          ((eq? proc 'make-detection-blocks!) (apply-proc (railway-nmbs 'make-detection-blocks!)))
          ((eq? proc 'make-switches!) (apply-proc (railway-nmbs 'make-switches!)))
          ((eq? proc 'switch-position!) (apply-proc (railway-nmbs 'switch-position!)))
          (else (error "Proc is undefinded" proc)))))

    (define listener-thread
      (thread (lambda ()
                (let read-loop ((in tcp-in))
                  (when (connected? in)
                    (let ((msg (read in)))
                      (eval msg)))
                  (read-loop tcp-in)))))

    (define (send-infra msg)
      (send-tcp-msg msg tcp-out))

    ;;callbacks (bevatten de procedures die de gui nodig heeft) die worden meegegeven aan de gui
    ;;wanneer we de staat van een component willen wijzigen dan doen we dat in infrabel, om een waarde te lezen
    ;;vraag je dat aan de railway van nmbs
    (define (calculate-trajectory train-id destination)
      (let* ((trajectory (railway-shortest-path ((railway-nmbs 'train-curr-dtb) train-id) destination))
             (path (list (append (list train-id) trajectory))))
        (send-infra (list 'train-trajectory! train-id trajectory))
        (send-infra (list 'calculate-trajectory path))
        path))
    ;;
    ;trein callback
    ;;
    (define (train-callback train-id)
      (define (speed! new-speed)
        (when (not (= new-speed ((railway-nmbs 'train-speed) train-id)))
          (send-infra (list 'speed! train-id new-speed))))

      (define (inverse-direction!)
        (send-infra (list 'train-inverse-direction! train-id)))

      (define (inverse-speed!)
        (send-infra (list 'train-inverse-speed! train-id)))

      (define (trajectory! new-trajectory)
        (send-infra (list 'train-trajectory! train-id new-trajectory)))

      (define (make-trajectory! destination)
        (let ((trajectory (calculate-trajectory train-id destination)))
          (send-infra (list 'process-trajectories! trajectory #t))))

      (define (reset-trajectory!)
        (send-infra (list 'train-trajectory! train-id '())))
    
      (define (dispatch m)
        (cond
          ((eq? m 'id) train-id)
          ((eq? m 'speed) ((railway-nmbs 'train-speed) train-id))
          ((eq? m 'speed!) speed!)
          ((eq? m 'inverse-direction!) (inverse-direction!))
          ((eq? m 'inverse-speed!) (inverse-speed!))
          ((eq? m 'trajectory) ((railway-nmbs 'train-trajectory) train-id))
          ((eq? m 'trajectory?) ((railway-nmbs 'train-trajectory?) train-id))
          ((eq? m 'trajectory!) trajectory!)
          ((eq? m 'calculate-trajectory) calculate-trajectory)
          ((eq? m 'make-trajectory!) make-trajectory!)
          ((eq? m 'reset-trajectory!) (reset-trajectory!))
          ((eq? m 'dt-block) ((railway-nmbs 'train-curr-dtb) train-id))))
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
          ((eq? m 'switch-position) (lambda (switch-id) ((railway-nmbs 'switch-position) switch-id)))
          ((eq? m 'switch-position!) (lambda (switch-id new-state) (send-infra (list 'switch-position! switch-id new-state))))
          ((eq? m 'list) (railway-nmbs 'switches))))
      dispatch)
    ;;
    ;;timetable callback
    ;;
    (define (time-tbl-callback)
      (define (open filename)
        (open-input-file filename))
      
      (define (cut-last string) ;wanneer het een lijn is dan is er steeds op het einde een spatie dat we niet nodig hebben
        (substring string 0 (- (string-length string) 1)))

      (define (make-trajectory! filename)
        (let* ((f (open filename))
               (train-id (string->symbol (cut-last (read-line f))))
               (component-start 0)
               (component-end 3)
               (action-length 4)
               (order-length 6)
               (calc-thread 'empty)
               (paths-info '()))
          (define current-path '())
          (set! current-path (cons train-id current-path))
          (define (get-component line start end)
            (string->symbol (substring line start end)))
          (define (get-switch-pos line end)
            (char->number (string-ref line (+ end 1))))
          (define (get-action line end)
            (let ((action-start (+ end 1)))
              (substring line action-start (+ action-start action-length))))
          (define (get-order line)
            (substring line 0 order-length))
          (for ([line (in-lines f)])
            (cond ((eq? (string-ref line 3) #\/) (set! component-end 3))
                  ((eq? (string-ref line 4) #\/) (set! component-end 4))
                  ((eq? (string-ref line 5) #\/) (set! component-end 5)))
            (cond
              ((switch? line) (set! current-path (cons (cons (get-component line component-start component-end) (get-switch-pos line component-end)) current-path)))
              ((dtb? line) (when (string=? (get-action line component-end) "stop")
                             (set! current-path (cons (cons (get-component line component-start component-end) (string->symbol (get-action line component-end))) current-path))))
              ((string=? (get-order line) "+start") (set! current-path (cons '+start current-path)))
              ((string=? (get-order line) "-start") (set! current-path (cons '-start current-path)))
              ((string=? (get-order line) "finish")
               (let ((next (read-line f))) (if (eof-object? next)
                                               (begin
                                                 (set! paths-info (cons (reverse current-path) paths-info)))
                                               (begin
                                                 (set! paths-info (cons (reverse current-path) paths-info))
                                                 (set! train-id (string->symbol (cut-last next)))
                                                 (set! current-path '())
                                                 (set! current-path (cons train-id current-path))))))))
          (for-each (lambda (path)
                      (let ((train-id (car path))
                            (trajectory (append (list ((railway-nmbs 'train-curr-dtb) train-id)) (map car (filter (lambda (checkpoint) (pair? checkpoint)) path)))))
                        (send-infra (list 'train-trajectory! train-id trajectory)))) paths-info)
          (send-infra (list 'process-trajectories! paths-info #f))))
      (define (dispatch m)
        (cond
          ((eq? m 'make-trajectory!) make-trajectory!)))
      dispatch)
    ;;
    ;;start
    ;;
    (define (start)
      ((gui 'start) (lambda (option)
                      (let ((start-positions (list (cons 'S-28 '1-1) (cons '1-7 '1-6) (cons 'S-12 '2-3) (cons 'S-4 '2-7))))
                      (if option
                          (send-infra (list 'start 'setup-hardware start-positions))
                          (begin
                            (send-infra (list 'switch-option! option))
                            (send-infra (list 'start 'setup-hardware start-positions))))
                      ;((infrabel 'start) 'setup-hardware (list (cons 'S-26 '1-4) (cons '1-4 '1-5) (cons '1-5 'S-20)))
                      ;((infrabel 'start) 'setup-loop-and-switches (list (cons 'D3 'D2) (cons 'D5 'D6)))
                      ;((infrabel 'start) 'setup-loop (list (cons 'D3 'D2) (cons 'D5 'D6)))
                      ;((infrabel 'start) 'setup-straight-with-switch (list (cons 'A1 'D1)))
                      ;((infrabel 'start) 'setup-straight (list (cons 'A1 'D1)))
                      (sleep wait-time)
                      ((gui 'draw-trains) train-callback (railway-nmbs 'trains))
                      ((gui 'draw-dt-blocks) (dtb-callback))
                      ((gui 'draw-switches) (switch-callback))
                      ((gui 'draw-time-tbl) (time-tbl-callback))
                      ((gui 'draw-trajectory) train-callback (dtb-callback))))))
    ;;
    ;;stop
    ;;
    (define (stop)
      ((gui 'exit) (lambda ()
                     (send-infra (list 'stop))
                     (railway-nmbs 'reset!)
                     (close-tcp))))
    ;;
    ;;dispatch
    ;;
    (define (dispatch m)
      (cond
        ((eq? m 'start) (start))
        ((eq? m 'stop) (stop))
        ((eq? m 'timetbl) (time-tbl-callback))))
    dispatch))