;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |HTC Final Project - Space Invaders|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =================================================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)


(define HIT-RANGE 10)

(define INVADE-RATE 100)
(define INVADE-CHANCE 2)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define MISSILE-START (- HEIGHT TANK-HEIGHT/2))
(define MISSILE (ellipse 5 15 "solid" "red"))


;; =================================================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (ListOfInvader) (ListOfMissile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader i)
  (... (invader-x i) (invader-y i) (invader-dx i)))

;; ListOfInvader is one of:
;;  - empty
;;  - (cons invader ListOfInvader)
;; interp. a list of invader
(define LOI1 empty)
(define LOI2 (cons (make-invader 150 100 12) (cons (make-invader 150 HEIGHT -10) empty)))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons invader ListOfInvader)
;;  - reference: (first loi) is invader 
;;  - self-reference: (rest loi) is ListOfInvader


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;;  - empty
;;  - (cons missle ListOfMissile)
;; interp. a list of missle

(define LOM1 empty)
(define LOM2 (cons (make-missile 150 300) (cons (make-missile 50 50) empty)))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons missile ListOfMissile)
;;  - reference: (first lom) is missile 
;;  - self-reference: (rest lom) is ListOfMissile

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; =================================================
;; Function Definitions:

;; game -> game
;; start the world with (main G0)
(define (main s)
  (big-bang s                ; game
    (on-tick   advance-game) ; game -> game
    (to-draw   render)       ; game -> Image
    (stop-when end?)         ; game -> Boolean
    (on-key    handle-key))) ; game KeyEvent -> game

;; game -> game
;; produce the next game state
;; (check-expects will not work given that invaders are added randomly)
;(define (advance-game game) G0) ;stub

(define (advance-game s)
  (make-game
   (advance-invaders (destroy-invaders (add-invader (game-invaders s)) (game-missiles s)))
   (advance-missiles (destroy-missiles (game-invaders s) (game-missiles s)))
   (advance-tank (game-tank s))))


;; ListOfInvader -> ListOfInvader
;; advances a list of invaders to their next positions
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (cons (make-invader (- WIDTH 5) HEIGHT 10) empty))
              (cons (make-invader WIDTH (+ HEIGHT INVADER-Y-SPEED) -10) empty))
(check-expect (advance-invaders (cons (make-invader (- WIDTH 5) HEIGHT 10)
                                      (cons (make-invader (- WIDTH 15) (- HEIGHT 50) 5) empty)))
              (cons (make-invader WIDTH (+ HEIGHT INVADER-Y-SPEED) -10)
                    (cons (make-invader (+ (- WIDTH 15) 5) (+ (- HEIGHT 50) INVADER-Y-SPEED) 5) empty)))
                                
; (define (advance-invaders loi) empty) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi))
               (advance-invaders (rest loi)))]))

;; invader -> invader
;; advances an invader to its next position
(check-expect (advance-invader (make-invader (- WIDTH 5) HEIGHT 10)) (make-invader WIDTH (+ HEIGHT INVADER-Y-SPEED) -10))
(check-expect (advance-invader (make-invader 3(/ HEIGHT 2) -10)) (make-invader 0 (+ (/ HEIGHT 2) INVADER-Y-SPEED) 10))
(check-expect (advance-invader (make-invader (/ WIDTH 2) HEIGHT 10)) (make-invader (+ (/ WIDTH 2) 10) (+ HEIGHT INVADER-Y-SPEED) 10))
(check-expect (advance-invader (make-invader (/ WIDTH 2) HEIGHT -10)) (make-invader (+ (/ WIDTH 2) -10) (+ HEIGHT INVADER-Y-SPEED) -10))

;(define (advance-invader i) (make-invader 0 0 0)) ;stub

(define (advance-invader i)
  (cond [(> (+ (invader-x i) (invader-dx i)) WIDTH)
         (make-invader WIDTH (+ INVADER-Y-SPEED (invader-y i)) (- (invader-dx i)))]
        [(< (+ (invader-x i) (invader-dx i)) 0)
         (make-invader 0 (+ INVADER-Y-SPEED (invader-y i)) (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))

;; ListOfInvader -> ListOfInvader
;; randomly adds an invader
;; (check-expects will not work for random numbers)

(define (add-invader loi)
  (if (< (random INVADE-RATE) INVADE-CHANCE)
      (cons (make-invader (random WIDTH) 0 (* (list-ref (list -1 1) (random 2)) INVADER-X-SPEED)) loi)
      loi))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; destroys any invaders that have been hit by missiles
(check-expect (destroy-invaders empty empty) empty)
(check-expect (destroy-invaders (list I1 I2 I3) empty) (list I1 I2 I3))
(check-expect (destroy-invaders (list I1 I2 I3) (list M1 M2 M3)) (list I2 I3))

;(define (destroy-invaders loi lom) empty) ;stub

(define (destroy-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom)   loi]
        [else
         (if (not (hit? (first loi) lom))
             (cons (first loi) (destroy-invaders (rest loi) lom))
             (destroy-invaders (rest loi) lom))]))

;; invader ListOfMissle -> Boolean
;; returns true if invader has been hit by a missile
(check-expect (hit? I1 (cons M1 empty)) false)
(check-expect (hit? I1 (cons M1 (cons M2 empty))) true)

;(define (hit? i lom) false) ;stub

(define (hit? i lom)
  (cond [(empty? lom) false]
        [else
         (or (hit-invader? i (first lom))
              (hit? i (rest  lom)))]))

;; invader missile -> Boolean
;; returns true if the missile hit an invader
(check-expect (hit-invader? I1 M1) false)
(check-expect (hit-invader? I1 M2) true)
(check-expect (hit-invader? I1 M3) true)

;(define (hit-invader? i m) false) ;stub
(define (hit-invader? i m)
  (and
   (<= (abs (- (missile-x m) (invader-x i))) HIT-RANGE) 
   (<= (abs (- (missile-y m) (invader-y i))) HIT-RANGE)))

;; ListOfMissile -> ListOfMissile
;; advances a list of missiles to their next positions if they are still on the screen
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2)) empty))
              (cons (make-missile (/ WIDTH 2) (- (/ HEIGHT 2) MISSILE-SPEED)) empty))
(check-expect (advance-missiles (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2))
                                      (cons (make-missile (/ WIDTH 3) HEIGHT) empty)))
              (cons (make-missile (/ WIDTH 2) (- (/ HEIGHT 2) MISSILE-SPEED))
                    (cons (make-missile (/ WIDTH 3) (- HEIGHT MISSILE-SPEED)) empty)))

;(define (advance-missiles lom) empty) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (> (missile-y (first lom)) 0)
             (cons (advance-missile (first lom))
                   (advance-missiles (rest lom)))
             (advance-missiles (rest lom)))]))

;; missile -> missile
;; advances a missile to its next position
(check-expect (advance-missile (make-missile 0 0)) (make-missile 0 (- 0 MISSILE-SPEED)))
(check-expect (advance-missile (make-missile 0 HEIGHT)) (make-missile 0 (- HEIGHT MISSILE-SPEED)))

;(define (advance-missile m) (make-missile 0 0 MISSILE-SPEED)) ;stub

(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfInvader ListOfMissile -> ListOfMissile
;; destroys any missiles that have hit invaders
(check-expect (destroy-missiles empty empty) empty)
(check-expect (destroy-missiles
               (cons (make-invader (/ WIDTH 3) (/ HEIGHT 3) 10) empty)
               (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2)) empty))
              (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2)) empty))
(check-expect (destroy-missiles
               (cons (make-invader (/ WIDTH 2) (/ HEIGHT 2) 10) empty)
               (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2)) empty))
              empty)
(check-expect (destroy-missiles
               (cons (make-invader (/ WIDTH 3) (/ HEIGHT 3) -10) (cons (make-invader (/ WIDTH 2) (/ HEIGHT 2) 10) empty))
               (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2)) empty))
              empty)
(check-expect (destroy-missiles
               (cons (make-invader (/ WIDTH 3) (/ HEIGHT 3) 10) (cons (make-invader (/ WIDTH 2) (/ HEIGHT 2) -10) empty))
               (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2)) (cons (make-missile (/ WIDTH 3) (/ HEIGHT 3)) empty)))
              empty)

;(define (destroy-missiles loi lom) empty) ;stub

(define (destroy-missiles loi lom)
  (cond [(empty? lom) empty]
        [(empty? loi)   lom]
        [else
         (if (not-hit-invader? loi (first lom))
             (cons (first lom) (destroy-missiles loi (rest lom)))
             (destroy-missiles loi (rest lom)))]))

;; ListOfInvader missile -> Boolean
;; returns true if missile has not hit any invaders
(check-expect (not-hit-invader? empty (make-missile (/ WIDTH 2) (/ HEIGHT 2))) true)
(check-expect (not-hit-invader? (cons (make-invader (/ WIDTH 3) (/ HEIGHT 3) 10) empty) (make-missile (/ WIDTH 2) (/ HEIGHT 2))) true)
(check-expect (not-hit-invader? (cons (make-invader (/ WIDTH 3) (/ HEIGHT 3) 10) (cons (make-invader (/ WIDTH 2) (/ HEIGHT 2) -10) empty)) (make-missile (/ WIDTH 2) (/ HEIGHT 2))) false)

;(define (not-hit-invader? loi m) false) ;stub

(define (not-hit-invader? loi m)
  (cond [(empty? loi) true]
        [else
         (and (not (hit-invader? (first loi) m))
              (not-hit-invader?  (rest loi)  m))]))

;; tank -> tank
;; advances the tank to its next position
(check-expect (advance-tank (make-tank (/ WIDTH 2) 1)) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (advance-tank (make-tank WIDTH  1)) (make-tank WIDTH                 1))
(check-expect (advance-tank (make-tank WIDTH -1)) (make-tank (- WIDTH TANK-SPEED) -1))
(check-expect (advance-tank (make-tank 0     -1)) (make-tank 0                    -1))
(check-expect (advance-tank (make-tank 0      1)) (make-tank TANK-SPEED            1))

;(define (advance-tank t) T0) ;stub

(define (advance-tank t)
  (cond [(> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH)
         (make-tank WIDTH (tank-dir t))]
        [(< (+ (tank-x t) (* (tank-dir t) TANK-SPEED))     0)
         (make-tank     0 (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))

;; game -> Image
;; render an image of the current game state
(check-expect (render (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render (make-game
                       (cons (make-invader 150 100 12) empty)
                       (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2)) empty)
                       (make-tank (/ WIDTH 2) 1)))
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)
                           (place-image INVADER 150 100         
                                        (place-image MISSILE (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))))

;(define (render s) BACKGROUND) ;stub
(define (render s)
  (render-tank (game-tank s)
               (render-missiles (game-missiles s)
                                (render-invaders (game-invaders s)))))

;; game Image -> Image
;; add an image of the tank to the rendered display
(check-expect (render-tank (make-tank (/ WIDTH 2) 1) BACKGROUND) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank (make-tank WIDTH -1) BACKGROUND) (place-image TANK WIDTH (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t img) BACKGROUND) ;stub
(define (render-tank t img)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               img))

;; game Image -> Image
;; render an image of the missiles in the current game state
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (cons (make-missile (/ WIDTH 2) (/ HEIGHT 2)) empty) BACKGROUND)
              (place-image MISSILE (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))
(check-expect (render-missiles (cons (make-missile (/ WIDTH 2) HEIGHT) (cons (make-missile (/ WIDTH 3) (- HEIGHT 20)) empty)) BACKGROUND)
              (place-image MISSILE (/ WIDTH 3) (- HEIGHT 20) (place-image MISSILE (/ WIDTH 2) HEIGHT BACKGROUND)))

;(define (render-missiles lom img) BACKGROUND) ;stub
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) img))]))

;; ListOfInvader -> Image
;; render an image of the missiles in the current game state
(check-expect (render-invaders empty) BACKGROUND)
(check-expect (render-invaders (cons (make-invader (/ WIDTH 2) HEIGHT 10) empty))
              (place-image INVADER (/ WIDTH 2) HEIGHT BACKGROUND))
(check-expect (render-invaders (cons (make-invader (/ WIDTH 2) HEIGHT 10) (cons (make-invader (/ WIDTH 3) (- HEIGHT 20) 10) empty)))
              (place-image INVADER (/ WIDTH 3) (- HEIGHT 20) (place-image INVADER (/ WIDTH 2) HEIGHT BACKGROUND)))

;(define (render-invaders loi) BACKGROUND) ;stub
(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi)))]))

;; game -> Boolean
;; return true if an invader has landed and the game is over
(check-expect (end? (make-game (cons (make-invader (/ WIDTH 2) HEIGHT 10) empty) empty (make-tank (/ WIDTH 2) 1))) true)
(check-expect (end? (make-game (cons (make-invader (/ WIDTH 2) (+ HEIGHT 1) 10) empty) empty (make-tank (/ WIDTH 2) 1))) true)
(check-expect (end? (make-game (cons (make-invader (/ WIDTH 2) (- HEIGHT 1) 10) empty) empty (make-tank (/ WIDTH 2) 1))) false)

;(define (end? s) false) ;stub

(define (end? s)
  (landed? (game-invaders s)))

;; ListOfInvaders -> Boolean
;; return true if an invader has landed and the game is over
(check-expect (landed? empty) false)
(check-expect (landed? (cons (make-invader (/ WIDTH 2) HEIGHT 10) empty)) true)
(check-expect (landed? (cons (make-invader (/ WIDTH 2) (+ HEIGHT 1) 10) empty)) true)
(check-expect (landed? (cons (make-invader (/ WIDTH 2) (- HEIGHT 1) 10) empty)) false)
              
;(define (landed? loi) false) ;stub
(define (landed? loi)
  (cond [(empty? loi) false]
        [else
         (or (>= (invader-y (first loi)) HEIGHT)
             (landed? (rest loi)))]))

;; game KeyEvent -> game
;; handle L/R arrow keys for tank movement and SPACE for firing missiles
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) 1)) " ")
              (make-game empty (cons (make-missile (/ WIDTH 2)  MISSILE-START) empty) (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game empty (cons (make-missile WIDTH 50) empty) (make-tank (/ WIDTH 2) 1)) " ")
              (make-game empty (cons (make-missile (/ WIDTH 2) MISSILE-START) (cons (make-missile WIDTH 50) empty)) (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) 1)) "left")
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) -1)) "left")
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) 1)) "right")
              (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) -1)) "right")
              (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game empty empty (make-tank (/ WIDTH 2) 1)) "m")
              (make-game empty empty (make-tank (/ WIDTH 2) 1)))

;(define (handle-key s ke) G0) ;stub

(define (handle-key s ke)
  (cond [(key=? ke " ") (make-game
                         (game-invaders s)
                         (cons (make-missile (tank-x (game-tank s)) MISSILE-START) (game-missiles s))
                         (game-tank s))]
        [(key=? ke "left") (make-game
                            (game-invaders s)
                            (game-missiles s)
                            (make-tank (tank-x (game-tank s)) -1))]
        [(key=? ke "right") (make-game
                             (game-invaders s)
                             (game-missiles s)
                             (make-tank (tank-x (game-tank s))  1))]
        [else s]))