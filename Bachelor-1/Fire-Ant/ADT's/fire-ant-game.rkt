;;;--------------------GAME ADT--------------------;;;

(load "adt-fire-ant.rkt")
(load "adt-position.rkt")
(load "adt-draw.rkt")
(load "adt-scorpion.rkt")
(load "adt-key.rkt")
(load "adt-food.rkt")
(load "adt-pressure-plate.rkt")
(load "adt-power-up.rkt")
(load "adt-level.rkt")
(load "adt-game.rkt")
(load "mazes.rkt")
(load "constants.rkt")
(load "adt-timer.rkt")
(load "adt-score.rkt")
(load "help-procedures.rkt")
(load "scorpion-paths.rkt")
(#%require "Graphics.rkt")
(#%require (only racket/base random))

(define fire-ant-game (make-game))

;;start het spel
(fire-ant-game 'start!)




;;ik haalde mijn inspiratie uit de implementatie van het spel 'Snake' die we tijdens het WPO-massaprogrameren hebben besproken.
;;ant https://forums.rpgmakerweb.com/index.php?threads/would-someone-or-some-folks-help-create-some-map-enemy-sprites-for-me.73047/
;;scorpion https://www.google.com/url?sa=i&url=https%3A%2F%2Fforums.rpgmakerweb.com%2Findex.php%3Fthreads%2Fchalkdust-resources.47335%2F&psig=AOvVaw1b-zzGdb3D-zTfvdNceTi2&ust=1645399833982000&source=images&cd=vfe&ved=0CAsQjRxqFwoTCLCf88f4jPYCFQAAAAAdAAAAABAD
;;background https://www.dreamstime.com/crystals-area-alien-planet-pixel-art-game-location-seamless-vector-background-crystals-area-alien-planet-pixel-art-game-image161688227
;;egg https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.deviantart.com%2Fkaijira%2Fart%2FXenomorph-Egg-XP-Style-639388281&psig=AOvVaw0kFI2E_pPYZaGNhDOBJlhb&ust=1645445518509000&source=images&cd=vfe&ved=0CAsQjRxqFwoTCNCRwa-gjvYCFQAAAAAdAAAAABAD
;;key https://www.google.com/url?sa=i&url=https%3A%2F%2Fforum.stabyourself.net%2Fviewtopic.php%3Ft%3D2380%26start%3D950&psig=AOvVaw0QBJ1Ps5IXRHbNsnWd20Vc&ust=1653574404229000&source=images&cd=vfe&ved=0CAwQjRxqFwoTCNjozuzq-vcCFQAAAAAdAAAAABAD
