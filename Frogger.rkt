;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Grader : David Padawer padawer.d@husky.neu.edu
; grade : 63 / 64
; comments : overall great job, but don't call main in your code. Nothing should start when I click
; run, but rather wait until I type it in (style) <-1>
; also, if you put a number like .1 after the on-tick it will play slowable so that it's not
; impossible to win :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Frogger game
; Alex Agudelo & Jesse Bates


; Design Recipe
;  - Constants 
;  - World State
;  - Wish List
;    - draw objects
;         - draw player
;         - draw vehicles
;    - move player
;         - key for direction   
;         - can't go off the screen
;    - move vehicles
;      - on-tick move in direction
;      - when end of screen make a new one
;    - game over
;       - collision, lose
;       - get to the end, win
;    - create vehicles
;         - 4 vechicles on road
;  - Main Function

; Design Recipe
;  - Constants
;  - World State


;**********************************************************
(require 2htdp/image)
(require 2htdp/universe)
; Constants
(define HH 1100)
(define WW 750)
(define SPEED 10)
(define PLAYSPEED (+ SPEED 15))
(define VEHICLELENGTH 80)
(define VEHICLEWIDTH 40)
(define VEHICLE (rectangle VEHICLELENGTH VEHICLEWIDTH 'solid 'red))
(define TRILEN 10)
(define PLAYER (triangle TRILEN 'outline 'black))
(define PLANK (rectangle VEHICLELENGTH VEHICLEWIDTH 'solid 'Brown))
(define TURTLE (ellipse VEHICLELENGTH VEHICLEWIDTH "solid" 'green))
(define WATER (rectangle WW VEHICLEWIDTH 'solid 'blue))
(define ROWS 5)
(define GAMEOVER 50)
(define SPACING (+ (/ (+ (- WW (* VEHICLELENGTH 4)) 80) 4) 80)) 
(define BG (empty-scene WW HH))
(define START (make-posn (/ WW 2) (- HH 30)))
(define ROW1 100)
(define ROW2 200)
(define ROW3 300)
(define ROW4 400)
(define ROW5 500)
(define ROW6 600)
(define ROW7 700)
(define ROW8 800)
(define ROW9 900)
(define ROW10 1000)

; World Sate

;; Data Definitions
;; A Player is a (make-player Number Number Direction)
(define-struct player (x y dir))
; x is a number
; Interpretation: the center x coordinate of the player
; y is a number
; Interpretiation: the center y coordinate of the player
; dir is one of:
;   - 'right
;   - 'left
;   - 'up
;   - 'down

#;(define (player-temp a-player)
    ... (player-x a-player) ...
    ... (player-y a-player) ...
    ... (player-dir a-player) ...)

;; A Vehicle is a (make-vehicle Number Number Direction)
(define-struct vehicle (x y dir))
; x is a number
; Interpretation: the center x coordinate of the vehicle
; y is a number
; Interpretation: the center y oordinate of the vehicle
; dir is one of:
;   - 'left
;   - 'right

#;(define (vehicle-temp a-vehicle)
    ... (vehicle-x a-vehicle) ...
    ... (vehicle-y a-vehicle) ...
    ... (vehicle-dir a-vehicle) ...)

;; A Set of Vehicles (VSet) or (LOV) List of Vehicles is one of:
;; - empty
;; - (cons Vehicle VSet(LOV))

#;(define (VSet-temp a-Vset)
    (cond
      [(empty? a-VSet) ...]
      [(cons? a-VSet) ...
       (first a-VSet) ...
       (rest a-Vest) ...]))

;; A World is a (make-world Player VSet)
;;The VSet represents the set of vehicles moving across the screen
(define-struct world (player vehicles planks turtles))
; player is a structure player
; Interpretation: (make-player Number Number Direction)
; vehicles is a list of structure vehicle
; Interpretation: (list (make-vehicle Number Number Direction))
; planks is a list of structure plank
; Interpretation: (list (make-plank Number Number Direction))

#; (define (world-temp a-World)
     ... (world-player a-World) ...
     ... (world-vehicles a-World) ...
     ... (world-planks a-World) ...
     ... (world-turtles a-World) ...)


;; A plank is (make-plank Number Number Direction)
(define-struct plank [x y dir])
; x is a Number
; Interpretation: x position of the plank
; y is a Number
; Interpretation: y position of the plank
; dir is a symbol
; Interpretation: direction of the plank 
;   -always 'right

#; (define (plank-temp a-Plank)
     ... (plank-x a-Plank) ...
     ... (plank-y a-Plank) ...
     ... (plank-dir a-plank) ...)

;; A turtle is (make-turtle Number Number Direction)
(define-struct turtle [x y dir])
; x is a Number
; Interpretation: x position of the turtle
; y is a Number
; Interpretation: y position of the turtle
; dir is a symbol
; Interpretation: direction of the turtle
;   - always 'left

#; (define (turtle-temp a-Turtle)
     ... (turtle-x a-Turtle) ...
     ... (turtle-y a-Turtle) ...
     ... (turtle-dir a-Turtle) ...)

;; A Set of Turtles (TSet) or (LOT) List of Turtles is one of:
;;  - empty
;;  - (cons Turtle TSet(LOT)

#; (define (TSet-temp a-TSet)
     (cond
       [(empty? a-TSet) ...]
       [(cons? a-TSet) ... (first a-Test)...
                       ... (rest a-Test) ...]))

;; A Set of Planks (PSet) or (LOP) List of Planks is one of:
;; - empty
;; - (cons Plank PSet(LOP))

#;(define (PSet-temp a-Pset)
    (cond
      [(empty? a-PSet) ...]
      [(cons? a-PSet) ...
       (first a-PSet) ...
       (rest a-Pest) ...]))

;; A Set of Obstacles (OSet) or (LOO) List of Obstacles is one of:
;; - empty
;; - (cons Plank PSet(LOP))
;; - (cons Vehicle VSet(LOV))

#;(define (OSet-temp a-Oset)
    (cond
      [(empty? a-OSet) ...]
      [(cons? a-PSet) 
       (cond
         [(vehicle? a-OSet) ...]
         [(plank? a-OSet) ..]
         [(turtle> a-OSet) ...])]))
  
; Definitions
(define player1 (make-player (posn-x START) (posn-y START) 'up))
(define player2 (make-player (posn-x START) (posn-y START) 'right))
(define player3 (make-player (posn-x START) (posn-y START) 'left))
(define vehicle0 (make-vehicle 420 ROW6 'left))
(define vehicle10 (make-vehicle -30 ROW6 'left))
(define vehicle11 (make-vehicle (- SPACING 30) ROW6 'left))
(define vehicle12 (make-vehicle (- (* SPACING 2) 30) ROW6 'left))
(define vehicle13 (make-vehicle (- (* SPACING 3) 30) ROW6 'left))
(define vehicle1 (make-vehicle 130 ROW7 'right))
(define vehicle20 (make-vehicle 70 ROW7 'right))
(define vehicle21 (make-vehicle (+ SPACING 70) ROW7 'right))
(define vehicle22 (make-vehicle (+ (* SPACING 2) 70) ROW7 'right))
(define vehicle23 (make-vehicle (+ (* SPACING 3) 70) ROW7 'right))
(define vehicle2 (make-vehicle 340 ROW8 'right))
(define vehicle30 (make-vehicle 0 ROW8 'left))
(define vehicle31 (make-vehicle  SPACING ROW8 'left))
(define vehicle32 (make-vehicle (* SPACING 2) ROW8 'left))
(define vehicle33 (make-vehicle (* SPACING 3) ROW8 'left))
(define vehicle3 (make-vehicle 270 ROW9 'right))
(define vehicle40 (make-vehicle 60 ROW9 'right))
(define vehicle41 (make-vehicle (+ SPACING 60) ROW9 'right))
(define vehicle42 (make-vehicle (+ (* SPACING 2) 60) ROW9 'right))
(define vehicle43 (make-vehicle (+ (* SPACING 3) 60) ROW9 'right))
(define vehicle50 (make-vehicle 10 ROW10 'left))
(define vehicle51 (make-vehicle (+ SPACING 10) ROW10 'left))
(define vehicle52 (make-vehicle (+ (* SPACING 2) 10) ROW10 'left))
(define vehicle53 (make-vehicle (+ (* SPACING 3) 10) ROW10 'left))
(define plank10 (make-plank -20 ROW1 'right))
(define plank11 (make-plank (- SPACING 20) ROW1 'right))
(define plank12 (make-plank (- (* SPACING 2) 20) ROW1 'right))
(define plank13 (make-plank (- (* SPACING 3) 20) ROW1 'right))
(define plank20 (make-plank 80 ROW3 'right))
(define plank21 (make-plank (+ SPACING 80) ROW3 'right))
(define plank22 (make-plank (+ (* SPACING 2) 80) ROW3 'right))
(define plank23 (make-plank (+ (* SPACING 3) 80) ROW3 'right))
(define plank30 (make-plank 0 ROW5 'right))
(define plank31 (make-plank SPACING ROW5 'right))
(define plank32 (make-plank (* SPACING 2) ROW5 'right))
(define plank33 (make-plank (* SPACING 3) ROW5 'right))
(define turtle10 (make-turtle 40 ROW2 'left))
(define turtle11 (make-turtle (+ SPACING 40) ROW2 'left))
(define turtle12 (make-turtle (+ (* SPACING 2) 40) ROW2 'left))
(define turtle13 (make-turtle (+ (* SPACING 3) 40) ROW2 'left))
(define turtle20 (make-turtle 0 ROW4 'left))
(define turtle21 (make-turtle SPACING ROW4 'left))
(define turtle22 (make-turtle (* SPACING 2) ROW4 'left))
(define turtle23 (make-turtle (* SPACING 3) ROW4 'left))

(define LOV0 empty)
(define LOV1 (list vehicle0))
(define LOV2 (list vehicle1 vehicle0)) 
(define LOV3 (list vehicle3 vehicle2 vehicle1 vehicle13 vehicle12 
                   vehicle11 vehicle10))

(define LOVSTARTVEHICLES (list vehicle53 vehicle52 vehicle51 vehicle50
                               vehicle43 vehicle42 vehicle41 vehicle40                 
                               vehicle33 vehicle32 vehicle31 vehicle30  
                               vehicle23 vehicle22 vehicle21 vehicle20 
                               vehicle13 vehicle12 vehicle11 vehicle10))
(define LOP1 (list plank10))
(define LOVSTARTPLANKS (list plank10 plank11 plank12 plank13
                             plank20 plank21 plank22 plank23
                             plank30 plank31 plank32 plank33))
(define LOT1 (list turtle10))
(define LOVSTARTTURTLES (list turtle10 turtle11 turtle12 turtle13
                              turtle20 turtle21 turtle22 turtle23))
(define WORLD0 (make-world player1 LOV2 empty empty))
(define WORLD1 (make-world player1 LOVSTARTVEHICLES empty empty))
(define player4 (make-player 200 200 'up))
(define player5 (make-player 200 214 'up))
(define player6 (make-player 0 0 'down))
(define vehicle4 (make-vehicle 200 200 'down))
(define LOV4 (list vehicle4))
(define WORLD2 (make-world player4 LOV4 empty empty))
(define WORLD3 (make-world player6 LOV4 empty empty))
(define WORLD4 (make-world player1 LOVSTARTVEHICLES LOVSTARTPLANKS LOVSTARTTURTLES))
(define WORLD5 (make-world player1 LOV2 LOP1 LOT1))

; Helper functions

; Purpose
; Draw the world (Player and vehicles and the board)
; World -> Image
(check-expect (draw-world WORLD0)
              (place-image
               (text "FINISH LINE" 30 'blue)
               (/ (- HH 300) 2)
               15
               (add-line (place-image PLAYER (/ WW 2) (- HH 30) 
                                      (place-image VEHICLE
                                                   130 ROW7
                                                   (place-image VEHICLE
                                                                420 ROW6 (drawWater BG))))
                         0 25 WW 25 'blue)))
(define (draw-world w)
  (drawBoard w (drawGameOver w (place-image 
                                (draw-player (world-player w)) 
                                (player-x (world-player w)) 
                                (player-y (world-player w))
                                (draw-obstacles (world-vehicles w) 
                                                (draw-obstacles (world-planks w) 
                                                                (draw-obstacles (world-turtles w) 
                                                                                (drawWater BG))))))))
; Purpose:
; Draw the water!
; World Image -> Image

(define (drawWater img)
  (place-image WATER (/ WW 2) ROW1
               (place-image WATER (/ WW 2) ROW2
                            (place-image WATER (/ WW 2) ROW3
                                         (place-image WATER (/ WW 2) ROW4
                                                      (place-image WATER (/ WW 2) ROW5 img))))))

; Purpose
; Draw the board (contains finish line)
; World Image -> Image
(check-expect (drawBoard WORLD1 BG)
              (place-image (text "FINISH LINE" 30 'blue)
                           (/ (- HH 300) 2)
                           15
                           (add-line BG 0 25 WW 25 'blue)))

(define (drawBoard w img)
  (place-image (text "FINISH LINE" 30 'blue)
               (/ (- HH 300) 2)
               15
               (add-line img 0 25 WW 25 'blue)))

; Draw Game Over if the player ends the game
; If it is a win then draw win else lose
; World Image -> Image
(check-expect (drawGameOver WORLD2 BG)
              (place-image (text "Game Over YOU LOST" GAMEOVER 'orange)
                           (/ WW 2) (/ (- HH GAMEOVER) 2) BG))
(check-expect (drawGameOver WORLD3 BG)
              (place-image (text "Game Over YOU WIN" GAMEOVER 'orange)
                           (/ WW 2) (/ (- HH GAMEOVER) 2) BG))
(define (drawGameOver w img)
  (cond
    [(and (game-over? w) (reachEnd w))
     (place-image (text "Game Over YOU WIN" GAMEOVER 'orange)
                  (/ WW 2) (/ (- HH GAMEOVER) 2) img)]
    [(or (game-over? w) (hitSomeVehicule (world-vehicles w) (world-player w)) (inRiver? (world-player w) w) (offBoard? (world-player w)))
     (place-image (text "Game Over YOU LOST" GAMEOVER 'orange)
                  (/ WW 2) (/ (- HH GAMEOVER) 2) img)]
    
    [else img]))

; Purpose:
; Draw the player  
; Player -> Image
(check-expect (draw-player player2)
              (rotate 270 PLAYER))
(check-expect (draw-player player3)
              (rotate 90 PLAYER))
(check-expect (draw-player player6)
              (rotate 180 PLAYER))
(check-expect (draw-player player5)
              PLAYER)
(define (draw-player a-player)
  (cond
    [(symbol=? (player-dir a-player) 'up)
     PLAYER]
    [(symbol=? (player-dir a-player) 'left)
     (rotate 90 PLAYER)]
    [(symbol=? (player-dir a-player) 'down)
     (rotate 180 PLAYER)]
    [(symbol=? (player-dir a-player) 'right)
     (rotate 270 PLAYER)]))



; Purpose:
; Draw the vehicles on the world 
; LOO (List of Obstacles) -> Image
(check-expect (draw-obstacles LOV0 BG)
              BG)
(check-expect (draw-obstacles LOV1 BG)
              (place-image VEHICLE
                           420
                           ROW6
                           BG))
(check-expect (draw-obstacles LOV2 BG)
              (place-image VEHICLE
                           130 ROW7
                           (place-image VEHICLE
                                        420 ROW6 BG)))

(check-expect (draw-obstacles LOP1 BG)
              (place-image PLANK
                           -20 ROW1
                           BG))
(check-error (draw-obstacles (list 5 10) BG))
                           
(define (draw-obstacles LOO img)
  (foldr (lambda (x y)
           (cond
             [(vehicle? x)
              (place-image
               VEHICLE
               (vehicle-x x)
               (vehicle-y x)
               y)]
             [(plank? x)
              (place-image
               PLANK
               (plank-x x)
               (plank-y x)
               y)]
             [(turtle? x)
              (place-image
               TURTLE
               (turtle-x x)
               (turtle-y x)
               y)]
             [else (error "In draw-vehicles, not given a vehicle, turtle
or a plank")]))
         img LOO))

; Purpose:
; move the vehicles at the current SPEED in their direction
; LOO (List of Obstalces) -> LOO (List of Obstacles)
(check-expect (move-obstacles LOV0)
              empty)
(check-expect (move-obstacles LOV1)
              (list (make-vehicle 410 600 'left)))
(check-expect (move-obstacles LOV2)
              (list
               (make-vehicle 140 700 'right)
               (make-vehicle 410 600 'left))) 
(check-expect (move-obstacles LOT1)
              (list (make-turtle 30 200 'left)))

(define (move-obstacles LOO)
  (map (lambda (x)
         (cond
           [(vehicle? x)
            (make-vehicle 
             (cond
               [(symbol=? (vehicle-dir x) 'left)
                (- (vehicle-x x) SPEED)]
               [(symbol=? (vehicle-dir x) 'right)
                (+ (vehicle-x x) SPEED)])
             (vehicle-y x) 
             (vehicle-dir x))]
           [(plank? x)
            (make-plank
             (+ (plank-x x) SPEED)
             (plank-y x)
             (plank-dir x))]
           [(turtle? x)
            (make-turtle
             (- (turtle-x x) SPEED)
             (turtle-y x) 
             (turtle-dir x))]))
       LOO))


; Purpose
; Key Handler
; World -> World
(check-expect (key-handler WORLD0 "up")
              (make-world
               (make-player 375 1045 'up)
               (list
                (make-vehicle 130 700 'right)
                (make-vehicle 420 600 'left))
               empty
               empty)) 
(check-expect (key-handler WORLD0 "down")
              (make-world
               (make-player 375 1095 'down)
               (list
                (make-vehicle 130 700 'right)
                (make-vehicle 420 600 'left))
               empty
               empty)) 
(check-expect (key-handler WORLD0 "right")
              (make-world
               (make-player 400 1070 'right)
               (list
                (make-vehicle 130 700 'right)
                (make-vehicle 420 600 'left))
               empty
               empty))
(check-expect (key-handler WORLD0 "left")
              (make-world
               (make-player 350 1070 'left)
               (list
                (make-vehicle 130 700 'right)
                (make-vehicle 420 600 'left))
               empty
               empty))
(define (key-handler w key)
  (make-world (move-player (world-player w) key) (world-vehicles w) (world-planks w) (world-turtles w)))

; Purpose 
; Move the player
; Player -> Player
(define player7 (make-player HH WW 'right))
(define player8 (make-player HH WW 'down))
(define player9 (make-player HH WW 'down))
(define player10 (make-player 0 0 'left))
(check-expect (move-player player1 "up")
              (make-player (posn-x START) (- (posn-y START) PLAYSPEED) 'up))
(check-expect (move-player player1 "right")
              (make-player (+ (posn-x START) PLAYSPEED) (posn-y START) 'right))
(check-expect (move-player player7 "right")
              (make-player 1125 750 'right))
(check-expect (move-player player1 "down")
              (make-player 375 1095 'down))
(check-expect (move-player player8 "down")
              (make-player 1100 775 'down))
(check-expect (move-player player1 "left")
              (make-player (- (posn-x START) PLAYSPEED) (posn-y START) 'left))
(check-expect (move-player player10 "left")
              (make-player -25 0 'left))
(check-expect (move-player player9 "me")
              player9)
(define (move-player a-player key)
    (cond
      [(string=? key "up")
       (make-player (player-x a-player)
                    (- (player-y a-player) PLAYSPEED)
                    'up)]
      [(string=? key "right")
       (make-player
        (+ (player-x a-player) PLAYSPEED)
        (player-y a-player)
        'right)]
      [(string=? key "down")
       (make-player (player-x a-player)
                    (+ (player-y a-player) PLAYSPEED)
                    'down)]
       [(string=? key "left")
        (make-player (- (player-x a-player) PLAYSPEED)
                     (player-y a-player)
                     'left)]
       [else a-player]))


; Purpose
; move the vehicles in the world
; World-> World
(check-expect (move-world WORLD5)
              (make-world
               (make-player 375 1070 'up)
               (list
                (make-vehicle 140 700 'right)
                (make-vehicle 410 600 'left))
               (list (make-plank -10 100 'right))
               (list (make-turtle 30 200 'left))))

(define (move-world w)
  (make-world (world-player w) 
              (move-obstacles (pruneObstacles (world-vehicles w))) 
              (move-obstacles (pruneObstacles (world-planks w))) 
              (move-obstacles (pruneObstacles (world-turtles w)))))

; Purpose:
; if the obstacle goes to the edge of the screen redraw it
; LOV (List of Vehicles) -> (List of Vehicles)
(define LOV5 (list (make-vehicle -100 -100 'left)))
(define LOV6 (list (make-vehicle 800 800 'right)))
(check-expect (pruneObstacles LOV3) 
              (list
               (make-vehicle 270 900 'right)
               (make-vehicle 340 800 'right)
               (make-vehicle 130 700 'right)
               (make-vehicle 592.5 600 'left)
               (make-vehicle 385 600 'left)
               (make-vehicle 177.5 600 'left)
               (make-vehicle -30 600 'left)))
(check-expect (pruneObstacles LOV5)
              (list (make-vehicle 790 -100 'left)))
(check-expect (pruneObstacles LOV6)
              (list (make-vehicle -40 800 'right)))
(check-expect (pruneObstacles LOT1)
              (list (make-turtle 40 200 'left)))
(check-expect (pruneObstacles LOP1)
              (list (make-plank -20 100 'right)))


(define (pruneObstacles LOO)
  (local (; takes a list of obstacles and
          ; determiens if it is off the edge of the 
          ; screen if it is, draw a new obstacles
          ; [List-of Obstalces] -> [List-of Obstalces]
          (define (newOb x)
            (cond
              [(vehicle? x)
               (cond
                 [(and (< (vehicle-x x) (- 0 (/ VEHICLELENGTH 2))) 
                       (symbol=? (vehicle-dir x) 'left))
                  (addObstacle x)]  
                 [(and (> (vehicle-x x) (+ WW (/ VEHICLELENGTH 2))) 
                       (symbol=?(vehicle-dir x) 'right))
                  (addObstacle x)]
                 [else x])]
              [(plank? x)
               (cond                
                 [(and (> (plank-x x) (+ WW (/ VEHICLELENGTH 2))) 
                       (symbol=?(plank-dir x) 'right))
                  (addObstacle x)]
                 [else x])]
              [(turtle? x)
               (cond
                 [(and (< (turtle-x x) (- 0 (/ VEHICLELENGTH 2))) 
                       (symbol=?(turtle-dir x) 'left))
                  (addObstacle x)]
                 [else x])])))   
    (map newOb LOO)))

; Purpose:
; Add a vehicle to the lsit
; Obstacle -> Obstacle
(check-expect (addObstacle vehicle0) (make-vehicle 790 600 'left))
(check-expect (addObstacle vehicle1) (make-vehicle -40 700 'right))
(check-expect (addObstacle plank10) (make-plank -40 100 'right))
(check-expect (addObstacle turtle10) (make-turtle 790 200 'left)) 
(check-error (addObstacle 5))
(define (addObstacle a-O)
  (cond
    [(vehicle? a-O)
     (cond
       [(symbol=? (vehicle-dir a-O) 'left)
        (make-vehicle (+ WW (/ VEHICLELENGTH 2)) (vehicle-y a-O) 
                      (vehicle-dir a-O))]
       [(symbol=? (vehicle-dir a-O) 'right)    
        (make-vehicle (- 0 (/ VEHICLELENGTH 2)) (vehicle-y a-O) 
                      (vehicle-dir a-O))])]
     [(plank? a-O)
      (make-plank (- 0 (/ VEHICLELENGTH 2)) (plank-y a-O) 
                    (plank-dir a-O))]
     [(turtle? a-O)
      (make-turtle (+ WW (/ VEHICLELENGTH 2)) (turtle-y a-O) 
                      (turtle-dir a-O))]
     [else (error "In addObstacle, not given a vehicle, turtle
or a plank")]))
     


; Purpose
; Did the player crash with an object?
; LOV Player -> Boolean

(check-expect (hitVehicle vehicle4 player4)
              true)
(check-expect (hitVehicle vehicle0 player1)
              false)
(check-expect (hitVehicle vehicle4 player5)
              true)
(define (hitVehicle LOV a-player)
  (and 
   (>= (+ (player-x a-player) (/ TRILEN 4)) (- (vehicle-x LOV) (/ VEHICLELENGTH 2)))
   (<= (- (player-x a-player) (/ TRILEN 4)) (+ (vehicle-x LOV) (/ VEHICLELENGTH 2)))
   (>= (+ (player-y a-player) (/ TRILEN 4)) (- (vehicle-y LOV) (/ VEHICLEWIDTH 2)))
   (<= (- (player-y a-player) (/ TRILEN 4))  (+ (vehicle-y LOV) (/ VEHICLEWIDTH 2)))))

   
; Purpose
; Game over conditions of hitting any vehicle
; LOV Player -> boolean
(check-expect (hitSomeVehicule LOV5 player1)
              false)
(check-expect (hitSomeVehicule LOV4 player4)
              true)

(define (hitSomeVehicule LOV a-player)
  (foldr (lambda (x y)
           (or (hitVehicle x a-player) y)) false LOV))

; Purpose
; Is the player in the river?
; Player Obstacle -> Boolean
(check-expect (inRiver? player1 WORLD4)
              false)
(check-expect (inRiver? player1 WORLD5)
              false)
(define (inRiver? a-player world)
  (cond
    [(or (and (< (player-y a-player) (+ ROW2 (/ VEHICLEWIDTH 2)))
              (> (player-y a-player) (- ROW2 (/ VEHICLEWIDTH 2))))
         (and (< (player-y a-player) (+ ROW1 (/ VEHICLEWIDTH 2)))
              (> (player-y a-player) (- ROW1 (/ VEHICLEWIDTH 2))))
         (and (< (player-y a-player) (+ ROW3 (/ VEHICLEWIDTH 2)))
              (> (player-y a-player) (- ROW3 (/ VEHICLEWIDTH 2))))
         (and (< (player-y a-player) (+ ROW4 (/ VEHICLEWIDTH 2)))
              (> (player-y a-player) (- ROW4 (/ VEHICLEWIDTH 2))))
         (and (< (player-y a-player) (+ ROW5 (/ VEHICLEWIDTH 2)))
              (> (player-y a-player) (- ROW5 (/ VEHICLEWIDTH 2)))))
          (not (or (onTurtles? a-player (world-turtles world)) 
                    (onPlanks? a-player (world-planks world))))]
    [else false])) 

; Purpose:
; Is the player on any of the turtles?
; Player [List-of Turtle] -> Boolean
(define turtle3 (make-turtle 0 0 'right))
(define turtle4 (make-turtle 20 20 'right))
(define LOT2 (list turtle4 turtle3))
(check-expect (onTurtles? playerTurtleTest LOT2) true)
(define  (onTurtles? a-Player LOT)
  (foldr (lambda (x y)
           (or (onTurtle? a-Player x) y)) false LOT))

; Purpose:
; Is the player on a turtle?
; Player Turtle -> Boolean
(define playerTurtleTest (make-player 0 0 'up))
(define turtle5 (make-turtle 50 50 'right))
(check-expect (onTurtle? playerTurtleTest turtle3) true)
(check-expect (onTurtle? playerTurtleTest turtle4) true)
(check-expect (onTurtle? playerTurtleTest turtle5) false)

(define (onTurtle? a-Player a-Turtle)
  (>= 1 (+ (/ (* (- (+ (player-x a-Player) (/ TRILEN 4)) (turtle-x a-Turtle)) 
                 (- (+ (player-x a-Player) (/ TRILEN 4)) (turtle-x a-Turtle)))
              (* VEHICLEWIDTH VEHICLEWIDTH)) 
           (/ (* (- (+ (player-y a-Player) (/ TRILEN 4)) (turtle-y a-Turtle)) 
                 (- (+ (player-y a-Player) (/ TRILEN 4)) (turtle-y a-Turtle)))
              (* VEHICLELENGTH VEHICLELENGTH)))))
; Purpose:
; Is the player on any plank?
; Player Plank -> Boolean
(check-expect (onPlanks? player1 LOP1) false)
(define (onPlanks? a-player LOP)
  (foldr (lambda (x y)
           (or (onPlank? a-player x) y)) false LOP))

; Purpose:
; Is the player on a plank?
; Player Plank -> Boolean
(define playerPlankTest (make-player 10 10 'up))
(define plankTest1 (make-plank 10 10 'up))
(define plankTest2 (make-plank 50 50 'up))
(check-expect (onPlank? playerPlankTest plankTest1) true)
(check-expect (onPlank? playerPlankTest plankTest2) false)
(define (onPlank? a-player a-Plank)
  (and 
   (>= (+ (player-x a-player) (/ TRILEN 4)) (- (plank-x a-Plank) (/ VEHICLELENGTH 2)))
   (<= (- (player-x a-player) (/ TRILEN 4)) (+ (plank-x a-Plank) (/ VEHICLELENGTH 2)))
   (>= (+ (player-y a-player) (/ TRILEN 4)) (- (plank-y a-Plank) (/ VEHICLEWIDTH 2)))
   (<= (- (player-y a-player) (/ TRILEN 4))  (+ (plank-y a-Plank) (/ VEHICLEWIDTH 2)))))
   
; Purpose:
; Did the player move off the board?
; Player -> Boolean
(define playerOFFBOARD (make-player 1500 1500 'right))
(check-expect (offBoard? player1)
              false)
(check-expect (offBoard? playerOFFBOARD)
              true)
(define (offBoard? a-player)
    (or
     (> (player-x a-player) (- WW TRILEN))
     (> (player-y a-player) (- HH TRILEN))
     (< (player-x a-player) TRILEN)))   
    
; Purpose:
; Game over condition of getting to the end
; World -> boolean
(check-expect (reachEnd WORLD1)
              false)
(check-expect (reachEnd WORLD2)
              false)
(check-expect (reachEnd WORLD3)
              true)
(define (reachEnd w)
  (< (player-y (world-player w)) 30))

; Purpose
; Find game over conditions
; World -> Boolean
(check-expect (game-over? WORLD1)
              false)
(check-expect (game-over? WORLD3)
              true)
(define (game-over? w)
  (or (hitSomeVehicule (world-vehicles w) (world-player w))
      (reachEnd w)
      (inRiver? (world-player w) w)
      (offBoard? (world-player w))))
; Main
; Define main function that will start
; the frogger game
(define (main worldStart)
  (big-bang worldStart
            [to-draw draw-world]
            [on-tick move-world .1]
            [on-key key-handler]
            [stop-when game-over?]))

(main WORLD4)






