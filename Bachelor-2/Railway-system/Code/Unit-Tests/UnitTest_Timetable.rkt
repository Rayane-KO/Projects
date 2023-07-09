#lang racket
(require "../helpfunctions.rkt"
         rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      UNIT-TEST: Time-table                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (open filename)
  (open-input-file filename))
      
(define (cut-last string) ;wanneer het een lijn is er steeds op het einde een spatie dat we niet nodig hebben
  (substring string 0 (- (string-length string) 1)))

(define (railway)
  (define (dispatch m)
    (cond
      ((eq? m 'train-curr-dtb) (lambda (train) '1-1))))
  dispatch)

(define railway-nmbs (railway))

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
    (map (lambda (path)
                (let ((train-id (car path))
                      (trajectory (append (list ((railway-nmbs 'train-curr-dtb) train-id)) (map car (filter (lambda (checkpoint) (pair? checkpoint)) path)))))
                  trajectory)) paths-info)))
;;
;;Time-table test 
;;
(check-equal? (make-trajectory! "../trajectories/traject.txt") '((1-1 S-26 S-28 1-1 1-6)))
(check-equal? (make-trajectory! "../trajectories/trajectory-test.txt") '((1-1 S-2-3 S-23 S-11 S-10 1-1 S-12 S-5 1-6 S-6 2-4 S-20 1-4)))
(check-equal? (make-trajectory! "../trajectories/dead-end-test.txt") '((1-1 S-26 S-12 2-3 S-11 S-10)))
(check-equal? (make-trajectory! "../trajectories/multiple-trajectories-test.txt") '((1-1 1-2) (1-1 2-4) (1-1 S-2-3 1-5)))