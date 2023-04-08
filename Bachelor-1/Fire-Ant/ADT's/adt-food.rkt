(load "adt-position.rkt")

(#%require (only racket/base random))
(define (make-food position)
  (let ((collected? #f)
        (n (random 100))
        (type 'none))
    ;;
    ;;collect!
    ;;
    (define (collect!)
      (set! collected? #t))
    ;;
    ;;random-type
    ;;
    (define (random-type)
      (cond
        ((<= n 50) 'egg)
        ((< 50 n 75) 'rare)
        ((<= 75 n 90) 'epic)
        ((< 90 n 99) 'legendary)
        ((<= 99 n 100) 'mythical)))
    (set! type (random-type))
    ;;
    ;;bonus
    ;;
    (define (bonus)
      (cond
        ((eq? type 'egg) 100)
        ((eq? type 'rare) 250)
        ((eq? type 'epic) 500)
        ((eq? type 'legendary) 1000)
        ((eq? type 'mythical) 5000)
        (else (display "unknown food-type"))))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-food m)
      (cond
        ((eq? m 'position) position)
        ((eq? m 'collected?) collected?)
        ((eq? m 'collect!) (collect!))
        ((eq? m 'type) type)
        ((eq? m 'bonus) (bonus))))
    dispatch-food))

    

