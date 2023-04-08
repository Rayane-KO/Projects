;;;--------------------CONSTANTS--------------------;;;
(define zero 0)
;;
;;grootte van mijn matrix/maze
;;
(define matrix-width 20)
(define matrix-height 20)
;;
;;grootte van een cel
;;
(define cel-width 35)
(define cel-height 35)
;;
;;grootte van het venster
;;
(define window-width (* matrix-width cel-width))
(define window-height (* matrix-height cel-height))
;;
;;food-px
;;
(define egg-width-px (+ cel-width 5))
(define egg-height-px (+ cel-height 5))
(define food-width-px (- cel-width 10))
(define food-height-px (- cel-width 10))
;;
;;1 stap in het doolhof
;;
(define step 1)
;;
;;grootte van een blok in mijn matrix
;;
(define block 1)
;;
;;grootte van mijn mier
;;
(define ant-width 1)
(define ant-height 1)
;;
;;grootte van een schorpioen
;;
(define scorpion-width 1)
(define scorpion-height 1)
;;
;;groote van een ei
;;
(define egg-width 1)
(define egg-height 1)
;;
;;
;;
(define object-width 1)
(define object-height 1)
;;
;;snelheid van de schorpioenen (bewegen elke 250ms)
;;
(define scorpion-speed 250)
(define normal-speed scorpion-speed)
(define fast-speed 150)
;;
;;lijst van alle mogelijke richtingen
;;
(define direction-list '(up down left right))
;;
;;
;;
(define max-level 3)
;;
;;
;;
(define zero 0)


