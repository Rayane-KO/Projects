;;;--------------------SCORPION PATHS--------------------;;;

(define (paths scorpion path walls-position time)
  (let* ((position (scorpion 'position))
         (x (position 'x))
         (y (position 'y))
         (direction (scorpion 'direction))
         (direction! (scorpion 'direction!))
         (opposite? (scorpion 'opposite?))
         (follow! ((scorpion 'follow!) direction time)))

    (define (if-same-go-to new-position new-direction)
      (if ((position 'same?) new-position)
          (direction! new-direction)))
    ;;
    ;;random-path
    ;;
    (define (random-path)
      (define (change-direction given-direction feel-further-x feel-further-y w-position)
        (if (eq? direction given-direction)
            (if (((make-position (round (+ x feel-further-x)) (round (+ y feel-further-y))) 'same?) w-position)
                (direction! (random-direction)))))
      (map (lambda (w-position)
             (change-direction 'up zero (- step) w-position)
             (change-direction 'down zero step w-position)
             (change-direction 'left (- step) zero w-position)
             (change-direction 'right step zero w-position))
           walls-position)
      follow!)
    ;;
    ;;neemt een lijst van kruispunten en richtingen en gaat het pad van de schorpioen updaten
    ;;
    (define (make-path intersection-list)
      (for-each (lambda (intersection)
                  (let ((x (car intersection))
                        (y (cadr intersection))
                        (direction (caddr intersection)))
                    (if-same-go-to (make-position x y) direction))) intersection-list)
      follow!)
    ;;
    ;;path 1 (level 1)
    ;;
    (define (path-1)
      (let ((path-1 '((12 3 right) (18 3 down) (18 7 left) (12 7 up))))
        (make-path path-1)))
    ;;
    ;;path 2 (level 1)
    ;;
    (define (path-2)
      (let ((path-2 '((10 6 down) (10 14 up))))
        (make-path path-2)))
    ;;
    ;;path 3 (level 2)
    ;;
    (define (path-3)
      (let ((path-3 '((8 3 right) (18 3 down) (18 4 left) (14 4 down) (14 5 left) (8 5 up))))
        (make-path path-3)))
    ;;
    ;;path 4 (level 2)µ
    ;;
    (define (path-4)
      (let ((path-4 '((18 16 up) (18 11 left) (16 11 down) (16 16 left) (14 16 up) (14 11 left) (12 11 down)
                                 (12 16 left) (10 16 up) (10 11 left) (8 11 down) (8 16 left)))
            (opposite '((18 16 right) (18 11 down) (16 11 right) (16 16 up) (14 16 right) (14 11 down) (12 11 right)
                                      (12 16 up) (10 16 right) (10 11 down) (8 11 right) (8 16 up))))
        (if (not opposite?)
            (begin
              (if ((position 'same?) (make-position 8 16))
                  (scorpion 'switch!))
              (make-path path-4))
            (begin
              (if ((position 'same?) (make-position 18 16))
                  (scorpion 'switch!))
              (make-path opposite)))))
    ;;
    ;;PATH 5 (level 2)
    ;;
    (define (path-5)
      (let ((path-5 '((3 11 right) (6 11 down) (6 16 left) (3 16 up))))
        (make-path path-5)))
    ;;
    ;;path 6 (level 3)
    ;;
    (define (path-6)
      (let ((path-6 '((5 10 right) (11 10 down) (11 11 left) (5 11 up))))
        (make-path path-6)))
    ;;
    ;;path 7 (level 3)
    ;;
    (define (path-7)
      (let ((path-7 '((18 16 up) (18 11 left) (13 11 down) (13 15 left) (3 15 down) (3 16 right)
                                 (15 16 up) (15 12 right) (17 12 down) (17 16 right))))
        (make-path path-7)))
    ;;
    ;;path 8 (level 3)
    ;;
    (define (path-8)
      (let ((path-8 '((1 11 down) (1 13 right) (3 13 up) (3 11 left))))
        (make-path path-8)))
    ;;
    ;;
    ;;
    (cond
      ((eq? path 'path-1) (path-1))
      ((eq? path 'path-2) (path-2))
      ((eq? path 'path-3) (path-3))
      ((eq? path 'path-4) (path-4))
      ((eq? path 'path-5) (path-5))
      ((eq? path 'path-6) (path-6))
      ((eq? path 'path-7) (path-7))
      ((eq? path 'path-8) (path-8))
      ((eq? path 'random-path) (random-path))
      (else (display "UNKNOWN PATH")))))
        
