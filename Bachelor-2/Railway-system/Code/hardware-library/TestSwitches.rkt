#lang racket
(require "interface.rkt")


(define (get-reversed-switch-position position)
  (if (= 1 position)
      2
      1))


(define (test-switch switch-name)
  (display "Testing switch ")
  (display (symbol->string switch-name))
  
  (let* ((start-position (get-switch-position switch-name))
           (reversed-position (get-reversed-switch-position start-position)))
      (set-switch-position! switch-name reversed-position)
      (sleep 1)
      (cond ((= reversed-position (get-switch-position switch-name))
             (set-switch-position! switch-name start-position)
             (sleep 1)
             (cond ((= start-position (get-switch-position switch-name))
                    (displayln " OK"))
                   (else (displayln " PROBLEM"))))
            (else (displayln " PROBLEM"))))
  (sleep 2))

(define (get-previous-switch-name switch-type) (vector-ref switch-type 0))
(define (get-position-switch-position switch-type) (vector-ref switch-type 1))

(define (test-switches)
  (for [((switch-name switch-type)  #hash((S-1  . ())
                                             (S-3  . #(S-2 2))
                                             (S-4  . ())
                                             (S-5  . ())
                                             (S-6  . ())
                                             (S-7  . ())
                                             (S-8  . ())
                                             (S-9  . ())
                                             (S-10 . ())
                                             (S-11 . ())
                                             (S-12 . ())
                                             (S-16 . ())
                                             (S-20 . ())
                                             (S-23 . ())
                                             (S-24 . ())
                                             (S-25 . ())
                                             (S-26 . ())
                                             (S-27 . ())
                                             (S-28 . ())))]
    
    (cond ((null? switch-type)
           (test-switch switch-name))
          (else
           
           (let ((name (get-previous-switch-name switch-type))
                 (position (get-position-switch-position switch-type)))
             (set-switch-position! name position)
             (cond ((= (get-switch-position name) position)
                    (test-switch switch-name))
                   (else
                    (display "Cannot test ")
                    (display switch-name)
                    (display " by ")
                    (displayln name))))))))
                   
            

        
       