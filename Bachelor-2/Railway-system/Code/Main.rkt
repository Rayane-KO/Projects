#lang racket
(require "infrabel.rkt")
(require "nmbs.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              MAIN                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define INFRABEL (make-infrabel))
(define NMBS (make-nmbs INFRABEL))
((INFRABEL 'add-nmbs!) NMBS) 


(define (start)
  (NMBS 'start)
  (NMBS 'stop))

(start)
















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GEBRUIKTE BRONNEN
;
;GUI:     https://docs.racket-lang.org/gui/
;Threads: https://docs.racket-lang.org/reference/threads.html
;         https://docs.racket-lang.org/guide/concurrency.html
;         https://gist.github.com/tfidfwastaken/dbcd525e03b7fb1561342a80f54371a0
;I/O:     https://docs.racket-lang.org/guide/i_o.html
;Date:    https://docs.racket-lang.org/reference/time.html
;Structs: https://stackoverflow.com/questions/58994448/access-a-struct-field-if-the-struct-is-part-of-a-list
;(hoe krijg je de waarden van een struct terug)