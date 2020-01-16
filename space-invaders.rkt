;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.3)
(define TANK-SPEED 4)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "dark grey"))
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

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position


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
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListofInvaders is one of:
;; - (empty)
;; - (cons Invader empty)
(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond
    [(empty? loi) (...)]
    [else
     (... (fn-for-invader (first loi))
          (fn-for-loinvaders (rest loi)))]))

;; Template Rules Used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first loi) is Invader
;; - self-reference: (rest loi) is ListofInvader



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListofMissile is one of:
;; - empty
;; - (cons Missile empty)
(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M1 (cons M2 empty)))

#;
(define (fn-for-lom lom)
  (... [(empty? lom) (...)]
       [else
        (... (fn-for-missile (first lom))
             (fn-for-lom     (rest lom)))]))


;; Functions:

;; =====================
;; Game -> Game
;; start the world with (main loinvaders tank)
;; =====================
(define (main game)
  (big-bang (make-game empty empty (make-tank (/ WIDTH 2) 0))                          ; game
    (on-tick   advance-game)                 ; game -> game
    (to-draw   render-game)               ; game -> Image
    (on-key    game-actions)            ; game KeyEvent -> game (tank moves)
    (stop-when end-game game-over-screen)))       ; y-pos missiles -> Boolean


;; =====================
;; advance-game
;; game -> game
;; interp. takes game and produce next state of game
;; note: beta version, only includes tank and tank movement
;; =====================

; (define-struct game (invaders missiles tank))

(define (advance-game s)
  (make-game (destroy-invaders (game-missiles s) (create-invader (next-invaders (game-invaders s))))
             (filtered-missiles (game-missiles s))
             (next-tank     (game-tank s))))

;; =====================
;; next-invaders
;; ListofInvaders -> ListofInvaders
;; interp. construct next list of invaders
;; ====================

; (define (next-invaders loi) empty) ; stub

(define (next-invaders loi)
  (cond
    [(empty? loi) empty]
    [else
     (cons (next-invader (first loi))
           (next-invaders (rest loi)))]))


;; =================
;; next-invader
;; Invader -> Invader
;; interp. make next invader
;; ==================

; (define (next-invader i) i) ; stub

(define (next-invader i)
  (cond [(>  (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i))) WIDTH)
         (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [(< (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i))) 0)
         (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))


;; ================
;; create-invader
;; ListofInvader -> ListofInvader
;; interp. returns a new list of  Invader at INVADE-RATE
;; ================

; (define (create-invader loi) empty) ; stub

(define (create-invader loi)
  (cond[(< (random 1000) INVADE-RATE)
        (cons (make-invader (random WIDTH) 0 (random-direction 1)) loi)]
       [else loi]))

;; =================
;; random-direction
;; Integer -> Integer
;; Take integer and return integer of positive or negative value, randomly

; (define (random-direction i) i) ; stub

(define (random-direction i)
  (if (even? (random 10))
      i
      (- i)))

;; ==================
;; destroy-invaders
;; ListofMissiles ListofInvaders -> ListofMissiles ListofInvaders
;; interp. if missile in lom of missile comes within 10 pixels of invader in loi, remove both missile and invader
;; ==============



; (define (destroy-invaders loi lom) loi lom)

(define (destroy-invaders lom loi)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (find-invader (first loi) lom)
             (rest loi)
             (cons (first loi) (destroy-invaders lom (rest loi))))]))


;; ==================
;; find-invader
;; Invader ListofMissiles -> Boolean
;; interp. if invader is wihtin hitbox of missile, return true
;; =================


;(define (find-invader i lom) true)

(define (find-invader i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<  (- (invader-x i) HIT-RANGE) (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
                  (<  (- (invader-y i) HIT-RANGE) (missile-y (first lom)) (+ (invader-y i) HIT-RANGE)))
             true
             (find-invader i (rest lom)))]))
      
      



;; ===================
;; filtered-missiles
;; ListofMissiles -> ListofMissiles
;; interp. calls helper functions to only store on-screen missiles in listofmissiles
;; ===================

(define (filtered-missiles lom)
  (onscreen-only (next-missiles lom)))

;; ====================
;; onscreen-only
;; ListofMissiles -> ListofMissiles
;; interp. produce only missiles that are onscreen
;; ===================

; (define (onscreen-only lom) lom) ;

(define (onscreen-only lom)
  (cond [(empty? lom) empty]
        [else
         (if (onscreen? (first lom))
             (cons (first lom) (onscreen-only (rest lom)))
             (onscreen-only (rest lom)))]))

;; ===================
;; onscreen?
;; Missile -> Boolean
;; interp. returns True if missile is within BACKGROUND
;; ==================

; (define (onscreen? m) true) ; stub

(define (onscreen? m)
  (> (missile-y m) 0))

;; ===================
;; next-missiles
;; ListofMissiles -> ListofMissiles
;; interp. produce next list of missiles
;; ========================

; (define (next-missiles loi) empty) ; stub


(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (next-missile      (first lom))
               (next-missiles     (rest lom)))]))


;; ==================
;; next-missile
;; Missile -> Missile
;; interp. make next missile
;; ===================

; (define (next-missile m) m) ; stub

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))




;; ==================
;; next-tank
;; Tank -> Tank
;; interp. if tank hits boundary, reverse direction
;; =================
(define (next-tank t)
  (cond [(> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH) (make-tank WIDTH (- (tank-dir t)))]
        [(< (+ (tank-x t) (* (tank-dir t) TANK-SPEED))     0) (make-tank 0 (- (tank-dir t)))]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))

; (define (next-tank t) t) ; stub

;; =========================
;; render-frame
;; Game -> Image
;; interp. takes combination of all images overlays onto background
;; =========================

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))




;; ====================
;; render-invaders
;; ListofInvaders IMg -> Image
;; interp. render list of invaders onto background
;; ====================

;(define (render-invaders loi) BACKGROUND) ;

(define (render-invaders loi img)
  (cond
    [(empty? loi) img]
    [else
     (place-image INVADER
                  (invader-x (first loi))
                  (invader-y (first loi))
                  (render-invaders (rest loi) img))]))



;; ====================
;; render-missiles
;; ListofMissiles -> Image
;; interp. render list of missiles onto background
;; !!!
;; =====================

; (define (render-missiles lom) BACKGROUND) ; stub


(define (render-missiles lom img)
  (cond
    [(empty? lom) img]
    [else
     (place-image MISSILE
                  (missile-x (first lom))
                  (missile-y (first lom))
                  (render-missiles (rest lom) img))]))


;; =====================
;; render-tank
;; Tank -> Image
;; interp. render tank in given x pos
;; =====================

; (define (render-frame tank) BACKGROUND) ; stub

(define (render-tank t)
  (place-image TANK
               (tank-x t) (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))


;; ========================
;; game-actions
;; Tank KeyEvent -> tank
;; interp. change direction of tank, left arrow moves tank left, right arrow moves tank right
;; =======================

; (define (game-actions t ke) t) ; stub


(define (game-actions s ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders s) (game-missiles s) (change-dir?  (game-tank s) ke))]
        [(key=? ke "right")
         (make-game (game-invaders s) (game-missiles s) (change-dir?  (game-tank s) ke))]
        [(key=? ke " ")
         (make-game (game-invaders s) (add-missile (game-missiles s) (game-tank s)) (game-tank s))]
        [else s]))


;; ==============================
;; change-dir?
;; tank ke -> boolean
;; interp. if tank direction (tank-dir t) is opposite value of key event (ke), inverse (tank-dir t)
;; HELPER to game-actions
;; ==============================

; (define (change-dir? t ke) t) ; stub

(define (change-dir? t ke)
  (cond
    [(key=? ke "left")
     (make-tank (tank-x t) -1)]
    [(key=? ke "right")
     (make-tank (tank-x t) 1)]))

;; =====================
;; add-missile
;; ListofMissile Tank -> ListofMissile
;; interp. add missile to ListofMissile at x of tank
;; =====================

; (define (add-missile lom t) empty) ; stub


(define (add-missile lom t)
  (cond [(empty? lom) (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) empty)]
        [else
         (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) lom)]))
               
;; ================
;; end-game
;; Game -> Boolean
;; interp. if invader reaches HEIGHT, stop game and render game over screen (gos)
;; ================

; (define (end-game s) BACKGROUND) ;

(define (end-game s)
  (cond [(empty? (game-invaders s)) false]
        [else
         (landed? (game-invaders s))]))


;; =============
;; landed?
;; ListofInvaders -> Boolean
;; interp. if invader has reached HEIGHT, return true
;; ===========

(define (landed? loi)
  (cond
    [(empty? loi) false]
    [else
     (if  (>= (invader-y (first loi)) HEIGHT)
          true
          (landed? (rest loi)))]))

;; ============
;; game-over-screen
;; Game -> Image
;; interp. produce game over screen
;; ===============

; (define (game-over-screen s) BACKGROUND) ; stub

(define (game-over-screen s)
  (place-image (text/font "GAME \nOVER,\n MAN" 45 "green" #f "system" "normal" "bold" #f) (/ WIDTH 2) (/ HEIGHT 2) (render-game s)))