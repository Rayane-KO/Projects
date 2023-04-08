;;;--------------------SCORE ADT--------------------;;;
(define (make-score)
  (let ((current-score 0)
        (high-score 0)
        (lives 3)
        (extra-lives 0))
    ;;
    ;;gaat de huige score en hoogste score updaten
    ;;
    (define (update-current-score! n)
      (set! current-score (+ current-score n))
      (if (> current-score high-score)
          (set! high-score current-score)))
    ;;
    ;;reset van de huidige score
    ;;
    (define (reset-current-score!)
      (set! current-score 0))
    ;;
    ;;levens updaten
    ;;
    (define (update-lives! n)
      (set! lives n))
    ;;
    ;;aantal levens resetten
    ;;
    (define (reset-lives!)
      (set! lives 3))
    ;;
    ;;aantal extra-levens updaten
    ;;
    (define (update-extra-lives! n)
      (set! extra-lives n))
    ;;
    ;;aantal extra-levens resetten
    ;;
    (define (reset-extra-lives!)
      (set! extra-lives 0))
    ;;
    ;;dispatch
    ;;
    (define (dispatch-score m)
      (cond
        ((eq? m 'get-current-score) current-score)
        ((eq? m 'get-high-score) high-score)
        ((eq? m 'update-current-score!) update-current-score!)
        ((eq? m 'reset-current-score!) (reset-current-score!))
        ((eq? m 'lives) lives)
        ((eq? m 'extra-lives) extra-lives)
        ((eq? m 'update-lives!) update-lives!)
        ((eq? m 'reset-lives!) (reset-lives!))
        ((eq? m 'update-extra-lives!) update-extra-lives!)
        ((eq? m 'reset-extra-lives!) (reset-extra-lives!))
        (else (display "MESSAGE NOT UNDERSTOOD (score-adt)"))))
    dispatch-score))

