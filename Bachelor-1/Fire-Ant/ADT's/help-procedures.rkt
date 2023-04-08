;;;--------------------HELP PROCEDURES--------------------;;;

(#%require (only racket/base random))
(load "adt-position.rkt")

;;1.make-matrix hebben we gezien tijdens de lessen van Algoritmen&Datastructuren1
;;(Wolfgang De Meuter)

;make-matrix zal een matrix van grootte n x m aanmaken
(define (make-matrix n m)
  (define rows (make-vector n '()))
  (do ((row 0 (+ row 1)))
    ((= row n) rows)
    (vector-set! rows row (make-vector m 0))))

;procedure om het aantal rijen terug te geven
(define (rows v)
  (vector-length v))

;procedure om het aantal kolommen terug te geven
(define (columns v)
  (vector-length (vector-ref v 0)))

;procedure om de waarde van een element (op rij i, kolom j) terug te geven
(define (value v i j)
  (vector-ref (vector-ref v i) j))

;procedure om de waarde van een element te veranderen
(define (value! v i j new-value)
  (vector-set! (vector-ref v i) j new-value))

;dit is een procedure dat ik geschreven heb
;om een betere inzicht te krijgen tijdens het implementeren van andere adts
(define (display-m matrix)
  (define (hulp m i)
    (if (< i (rows m))
        (begin (display (vector-ref m i))
               (newline)
               (hulp m (+ i 1)))))
  (hulp matrix 0))

(define (random-direction)
  (let ((n (random 4)))
    (cond
      ((= n 0) 'left)
      ((= n 1) 'right)
      ((= n 2) 'up)
      ((= n 3) 'down))))

(define (search-power-up type power-ups-list)
  (define (search lst res)
    (if (null? lst)
        (reverse res)
        (if (eq? ((car lst) 'type) type)
            (search (cdr lst) (cons (car lst) res))
            (search (cdr lst) res))))
  (search power-ups-list '()))

(define (move-maze! position object-direction direction-proc maze . other-position)
  (define (no-wall? feel-further-x feel-further-y)
    (zero? (value maze (round (+ (position 'y) feel-further-y)) (round (+ (position 'x) feel-further-x)))))
  
  (define (set-position!)
    ((position 'x!) (position 'x))
    ((position 'y!) (position 'y)))
  
  (define (set-correct-position! feel-further-x feel-further-y)
    (if (and (no-wall? feel-further-x feel-further-y) (if (not (null? other-position))
                                                          (((car other-position) 'same?) (make-position (position 'x) (position 'y)))
                                                          #t))
        (begin
          (direction-proc object-direction)
          ((position 'move!) object-direction step))))

  (define (go-to-correct-position! direction further-x further-y)
    (if (eq? object-direction direction)
        (set-correct-position! further-x further-y)
        (set-position!)))
  (go-to-correct-position! 'up 0 (- step))
  (go-to-correct-position! 'down 0 step)
  (go-to-correct-position! 'left (- step) 0)
  (go-to-correct-position! 'right step 0))

(define (char->number char)
  (- (char->integer char) 48))
    



