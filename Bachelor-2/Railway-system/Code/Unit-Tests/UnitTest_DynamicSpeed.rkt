#lang racket
(require "../ADTs/ADT-Train.rkt"
         "../constants.rkt"
         rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      UNIT-TEST: DYNAMIC-SPEED                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define train (make-train 'T-1))
((train 'current-dtb!) '1-4)
;ik gebruik het Train ADT in plaats van de opstelling in Infrabel en NMBS zodat
;ik dit makkelijk kan testen

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

(define (dynamic-speed)
  (let ((speed-thread 'empty) 
        (on? #f))

    (define (speed-changer! current-speed end-speed adder)
      (let ((speed (abs current-speed)))
        (cond ((not (= speed end-speed)) ((train 'speed!) (adder current-speed 1))))))
          
    (define (speed-up speed)
      (speed-changer! speed max-speed +))
          
    (define (slow-down speed)
      (speed-changer! speed slow-speed -))
          
    (define (speed-control)
      (let ((dtb (train 'current-dtb))
            (speed (train 'speed)))
        (if dtb                                         ;als een trein zich niet op een dtb bevindt, dan zal het moeten vertragen
            (cond ((long? dtb) (speed-up speed))
                  ((dead-end? dtb) ((train 'speed!) zero-speed))
                  (else (slow-down speed)))
            (slow-down speed))))

    (define (start!)
      ((train 'speed!) slow-speed)
      (cond ((not on?) (set! speed-thread (thread (lambda () (let loop () (speed-control) (loop))))))))
          
    (define (dispatch m)
      (cond
        ((eq? m 'start) (start!))
        ((eq? m 'stop) ((train 'speed!) zero-speed) (kill-thread speed-thread))))
    dispatch))

(define dynamic (dynamic-speed))

(check-equal? (train 'speed) 0)
(dynamic 'start)
;begin op bocht
(check-equal? (abs (train 'speed)) slow-speed)
;nu is de trein op een lang stuk spoor
((train 'current-dtb!) '1-6)
(sleep 1.0) ;wacht tot de trein versnelt
(check-equal? (abs (train 'speed)) max-speed)
;doodlopend stuk spoor
((train 'current-dtb!) '2-7)
(sleep 1.0)
(check-equal? (abs (train 'speed)) zero-speed)
;nog eens een bocht
((train 'current-dtb!) '2-3)
(sleep 1.0)
(check-equal? (abs (train 'speed)) slow-speed)
;en als laatst nog een lang stuk spoor
((train 'current-dtb!) '1-1)
(sleep 1.0)
(check-equal? (abs (train 'speed)) max-speed)
(dynamic 'stop)

