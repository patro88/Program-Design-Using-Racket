;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;-------------------------------------------------------------------------;
;                       FILE NAME: balls-in-box.rkt                       ;
;-------------------------------------------------------------------------;


;-------------------------------------------------------------------------;
;                         PROBLEM STATEMENT                               ;
;-------------------------------------------------------------------------;
;                                                                         ;
; Program should display some balls that you can drag on a canvas. The    ;
; program starts with a canvas 400 pixels wide and 300 pixels high, with  ;
; no balls. Hitting the "n" key should create a new ball  in the center   ;
; of the canvas. You can select a ball by doing a button-down inside the  ;
; ball. When a ball is selected, you can drag it with the mouse. It       ;
; becomes unselected when you do a mouse-up. The balls should be displayed;
; as a circle with radius 20. An unselected ball should be displayed as   ;
; an outline; a selected ball should be displayed solid. In addition to   ;
; the balls, you should display the number of balls currently on the      ;
; canvas. As per the new features a new ball should start moving to the   ;
; right and when touch the canvas walls move tanget to it.                ;
;-------------------------------------------------------------------------;

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

; start with (run 8 0.25)

(provide run)
(provide initial-world)
(provide world-after-key-event)
(provide world-after-mouse-event)
(provide world-balls)
(provide ball-x-pos)
(provide ball-y-pos)
(provide ball-selected?)
(provide world-after-tick)
(provide world-to-scene)

;-------------------------------------------------------------------------;
;                             RUN FUNCTION                                ;
;-------------------------------------------------------------------------;

; run : PosInt PosReal -> World
; GIVEN: a ball speed and a frame rate, in secs/tick.
; EFFECT: runs the world.
; RETURNS: the final state of the world.
; EXAMPLE: (run 8 .25) creates and runs a world in which each ball 
;           travels at 8 pixels per tick and each tick is 0.25 secs.

(define (run speed rate)
  (big-bang (initial-world speed)
            (on-tick world-after-tick rate)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;-------------------------------------------------------------------------;
;                             CONSTANTS                                   ;
;-------------------------------------------------------------------------;

; dimensions and attributes of canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

; dimensions and attributes of circle
(define CIRCLE-RADIUS 20)
(define CIRCLE-COLOR "green")
(define SOLID-MODE   "solid")
(define OUTLINE-MODE "outline")

; solid circle image
(define SOLID-CIRCLE   (circle CIRCLE-RADIUS SOLID-MODE CIRCLE-COLOR))
(define OUTLINE-CIRCLE (circle CIRCLE-RADIUS OUTLINE-MODE CIRCLE-COLOR))

; mouse coordinates default value
(define DEFAULT-MOUSE-X 0)
(define DEFAULT-MOUSE-Y 0)

; mouse-events
(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define OTHER "enter")

; counter attributes
(define COUNTER-X-POS HALF-CANVAS-WIDTH)
(define COUNTER-Y-POS 30)
(define COUNTER-COLOR "black")
(define COUNTER-FONT-SIZE 24)

; key button
(define N "n")
(define Q "q")
(define PAUSE " ")

; other constants
(define ZERO 0)
(define ONE 1)

; direction
(define RIGHT "right")
(define LEFT "left")

;-------------------------------------------------------------------------;
;                            DATA DEFINITIONS                             ;
;-------------------------------------------------------------------------;

(define-struct ball (x-pos y-pos selected? mouse-x mouse-y direction?))

; A Ball is a (make-ball Real Real Boolean Integer Integer String)

; INTERPRETATION:
;  x-pos     : tells how far the center of ball is from x-axis, in pixels
;  y-pos     : tells how far the center of ball is from y-axis, in pixels
;  selected? : tells whether or not the ball is selected
;  mouse-x   : tells how far the mouse is from x-axis, in pixels
;  mouse-y   : tells how far the mouse in from y-axis, in pixels
;  direction?: tells the current direction of the ball i.e. left or right

; TEMPLATE:
; ball-fn : Ball -> ??
;(define (ball-fn b)
;  (... (ball-x-pos b) (ball-y-pos b) (ball-selected? b) 
;       (ball-mouse-x b) (ball-mouse-y b) (ball-direction? b))

; EXAMPLES for tests:
(define initial-unselected-ball 
  (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT false
             DEFAULT-MOUSE-X DEFAULT-MOUSE-Y RIGHT))
(define initial-selected-ball
  (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT true
             DEFAULT-MOUSE-X DEFAULT-MOUSE-Y RIGHT))
(define initial-unselected-ball-after-tick
  (make-ball (+ HALF-CANVAS-WIDTH 8) HALF-CANVAS-HEIGHT false
             DEFAULT-MOUSE-X DEFAULT-MOUSE-Y RIGHT))
(define unselected-ball-at-120-moving-left
  (make-ball 120 HALF-CANVAS-HEIGHT false
             DEFAULT-MOUSE-X DEFAULT-MOUSE-Y LEFT))
(define unselected-ball-at-112-moving-left
  (make-ball 112 HALF-CANVAS-HEIGHT false
             DEFAULT-MOUSE-X DEFAULT-MOUSE-Y LEFT))
(define ball1 (make-ball 60 20 false 0 0 RIGHT))

; default ball to be used in keyevent function
(define default-ball 
  (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT false 
             DEFAULT-MOUSE-X DEFAULT-MOUSE-Y RIGHT))
;-------------------------------------------------------------------------;

; A ListOfBall (LOB) is one of
; -- empty            interp: a sequence with no elements
; -- (cons Ball LOB)  interp: (cons Ball LOB) represents a sequence whose
;                             first element is Ball and whose other 
;                             elements are represented by LOB

; TEMPLATE:
; lob-fn : LOB -> ??
;(define (lob-fn lob)
;  (cond
;    [(empty? lob) ...]
;    [else (... (ball-fn (first lob))
;               (lob-fn (rest lob)))]))

; EXAMPLES for tests:
(define lob1 (cons initial-unselected-ball 
                   (cons initial-selected-ball empty)))
;-------------------------------------------------------------------------;

(define-struct world (lob speed paused?))

; A World is a (make-world ListOfBall PosInt Boolean)

; INTERPRETATION:
;  lob     : list of balls
;  speed   : any ball created in the world will travel at the given speed
;  paused? : tells whether the world is paused or not

; TEMPLATE:
; world-fn : World -> ??
;(define (world-fn b)
;  (... (world-lob w) (world-speed w) (world-paused? w)))

; EXAMPLES for tests:
(define unpaused-empty-world 
  (make-world empty 8 false))
(define paused-empty-world 
  (make-world empty 8 true))
(define unpaused-world-with-1-ball 
  (make-world (list initial-unselected-ball) 8 false))
(define unpaused-world-with-1-ball-1-tick 
  (make-world (list initial-unselected-ball-after-tick) 8 false))
(define unpaused-world-with-1-ball-at-120-moving-left
  (make-world (list unselected-ball-at-120-moving-left) 8 false))
(define unpaused-world-with-1-ball-at-112-moving-left
  (make-world (list unselected-ball-at-112-moving-left) 8 false))
;-------------------------------------------------------------------------;

; A Direction is one of
;  - "left"  (interp: ball move towards left direction)
;  - "right" (interp: ball move towards right direction)

; TEMPLATE:
; direction-fn : Direction -> ??
; (define (direction-fn d)
;  (cond
;    [(string=? d LEFT)...]
;    [(string=? d RIGHT)...]))

;-------------------------------------------------------------------------;
;                         END DATA DEFINITIONS                            ;
;-------------------------------------------------------------------------;


;-------------------------------------------------------------------------;
;                         FUNCTION DEFINITIONS                            ;
;-------------------------------------------------------------------------;

; initial-world : PosInt -> World
; GIVEN: a ball speed
; RETURNS: a world with no balls, but with the property that any balls 
;          created in that world will travel at the given speed.
; EXAMPLES: (initial-world 8) => (make-world empty 8 false)
; STRATEGY: Function Composition

(define (initial-world speed)
  (make-world empty speed false))

; TESTS
(begin-for-test 
  (check-equal? (initial-world 8) (make-world empty 8 false)) 
                "Test Failed for initial-world")
;-------------------------------------------------------------------------;

; world-with-paused-toggled : World -> World
; GIVEN: a world
; RETURNS: a world just like the given one, but with paused? toggled
; EXAMPLE: (world-with-paused-toggled unpaused-empty-world) =>
;           paused-empty-world
; STRATEGY: Structural Decomposition on w : World

(define (world-with-paused-toggled w)
  (make-world (world-lob w) (world-speed w) (not (world-paused? w))))

; TESTS
(begin-for-test
  (check-equal? 
   (world-with-paused-toggled unpaused-empty-world) paused-empty-world 
   "Test Failed for world-with-paused-toggled, should return pause world"))
;-------------------------------------------------------------------------;

; world-after-tick : World -> World
; GIVEN: a world
; RETURNS: the world that should follow the given world after a tick.
; EXAMPLES: (world-after-tick paused-empty-world) => paused-empty-world
; STRATEGY: Structural Decomposition on w : World

(define (world-after-tick w)
  (if (world-paused? w) w (make-world 
                           (balls-after-tick (world-lob w) (world-speed w))
                           (world-speed w) 
                           (world-paused? w))))

; TESTS
(begin-for-test
  (check-equal? 
   (world-after-tick paused-empty-world) paused-empty-world 
   "Test Failed for world-after-tick, should return pause world")
  (check-equal? 
   (world-after-tick unpaused-world-with-1-ball-at-120-moving-left)
    unpaused-world-with-1-ball-at-112-moving-left 
    "Test Failed for world-after-tick, ball must move 8 pixels left")
  (check-equal? 
   (world-after-tick unpaused-world-with-1-ball)
    unpaused-world-with-1-ball-1-tick 
    "Test Failed for world-after-tick, ball must move 8 pixels right"))
;-------------------------------------------------------------------------;

; balls-after-tick : ListOfBall PosInt -> ListOfBall
; GIVEN: a listofball and the speed of the world
; RETURNS: a listofball with the state of given balls after a tick if it 
; were in an unpaused world
; EXAMPLES: (balls-after-tick initial-unselected-ball) =>
;            initial-unselected-ball-after-tick
; STRATEGY: Higher Order Function Composition

(define (balls-after-tick lob speed)
  (map
   ; Ball -> Ball
   ; GIVEN: a ball
   ; RETURNS: a ball after a tick and move with world speed
   (lambda (b) (balls-after-tick-helper b speed)) 
   lob))

; TESTS
; tests follow helper funtions
;-------------------------------------------------------------------------;

; balls-after-tick-helper : Ball PosInt -> Ball
; GIVEN: a ball and the speed of the world
; RETURNS: the ball after a tick if it were in an unpaused world
; EXAMPLES: (balls-after-tick-helper initial-selected-ball 8) =>
;            initial-selected-ball
; STRATEGY: Structural Decomposition on b : Ball

(define (balls-after-tick-helper b speed)
  (if (ball-selected? b) b (move-ball 
                           (ball-x-pos b) (ball-y-pos b) (ball-mouse-x b) 
                           (ball-mouse-y b) (ball-direction? b) speed) ))

; TESTS
(begin-for-test
  (check-equal? 
   (balls-after-tick-helper initial-selected-ball 8) initial-selected-ball 
   "Test failed for balls-after-tick-helper, ball shouldn't move"))
;-------------------------------------------------------------------------;

; move-ball : Real Real Integer Integer String PosInt -> Ball
; GIVEN: x, y position of ball, x, y position of mouse, direction of the
;        ball and the speed of the world
; RETURNS: the ball that should follow one in the given position in an
; unpaused world 
; EXAMPLES: (move-ball 100 200 0 0 RIGHT 8) => 
;           (make-ball 108 200 false 0 0 RIGHT)
; STRATEGY: Structural Decomposition on direction : Direction

(define (move-ball x-pos y-pos mouse-x mouse-y direction speed)
  (cond
    [(string=? direction LEFT) 
     (left-helper  x-pos y-pos mouse-x mouse-y speed)]
    [(string=? direction RIGHT) 
     (right-helper  x-pos y-pos mouse-x mouse-y speed)]))

; TESTS
; tests follow helper function
;-------------------------------------------------------------------------;

; left-helper : Real Real Integer Integer PosInt -> Ball
; north-helper: Real Real Integer Integer PosInt -> Ball

; GIVEN: x,y position of ball, x,y position of mouse and the world speed
; RETURNS: the ball that should follow one in the given position in an
; unpaused world 
; EXAMPLES: (left-helper 25 80 0 0 8) => 
;           (make-ball CIRCLE-RADIUS 80 false 0 0 RIGHT)
; STRATEGY: Function Composition

(define (left-helper  x-pos y-pos mx my speed)
  (if (< (- x-pos speed) CIRCLE-RADIUS) 
         (make-ball CIRCLE-RADIUS y-pos false mx my RIGHT) 
         (make-ball (- x-pos speed) y-pos false mx my LEFT)))


(define (right-helper x-pos y-pos mx my speed)
  (if (> (+ x-pos speed) (- CANVAS-WIDTH CIRCLE-RADIUS))
         (make-ball (- CANVAS-WIDTH CIRCLE-RADIUS) y-pos false mx my LEFT) 
         (make-ball (+ x-pos speed) y-pos false mx my RIGHT)))

;; TESTS
(begin-for-test
  (check-equal? 
   (left-helper 25 80 0 0 8)
   (make-ball CIRCLE-RADIUS 80 false 0 0 RIGHT)
   "Test Failed for left-helper, ball shall start from CIRCLE-RADIUS")
  (check-equal? 
   (right-helper 400 80 0 0 8)
   (make-ball (- CANVAS-WIDTH CIRCLE-RADIUS) 80 false 0 0 LEFT)
   "Test Failed for north-helper, ball shall start from 380"))
;-------------------------------------------------------------------------;

;; world-to-scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene world1) default-canvas
;; STRATEGY: Structural Decomposition on w : World

(define (world-to-scene w)
  (world-to-scene-helper w (number->string (length (world-lob w)))))

;; TESTS
;; tests follow helper function
;-------------------------------------------------------------------------;

;; world-to-scence-helper : World String -> Scene
;; GIVEN: a world and the current count of the ball (in string)
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene world1) = canvas-with-1-ball
;; STRATEGY: Structural Decomposition on w : World

(define (world-to-scene-helper w count)
  (world-of-balls (world-lob w) count))

; TESTS
(begin-for-test
  (check-equal? 
   (world-to-scene unpaused-world-with-1-ball) canvas-with-1-ball
   "Test Failde for world-to-scene, expected world with a ball"))
;-------------------------------------------------------------------------;

; world-of-balls : ListOfBall PosInt -> Scene
; GIVEN: world-of-balls
; RETURNS: a scence with the counter and the balls
; EXAMPLES: (world-of-balls empty 0) => default-canvas
; STRATEGY: Higher Order Function Composition

(define (world-of-balls lob count)
  (foldr
   ; Scene Ball -> Scene
   ; GIVEN: a scene and a ball
   ; RETURNS: the scene with the required no of ball
   (lambda (a b) (place-image (ball-mode a) (ball-x a) (ball-y a) b))
   (counter-canvas count)
   lob))

;; TESTS
;; canvas examples to be used for test
(define default-canvas
  (place-image (text (number->string ZERO) COUNTER-FONT-SIZE COUNTER-COLOR)
               COUNTER-X-POS COUNTER-Y-POS EMPTY-CANVAS))

(define canvas-with-1-ball
  (place-image (text (number->string ONE) COUNTER-FONT-SIZE COUNTER-COLOR) 
               COUNTER-X-POS COUNTER-Y-POS 
               (place-image OUTLINE-CIRCLE HALF-CANVAS-WIDTH
                            HALF-CANVAS-HEIGHT EMPTY-CANVAS)))
;-------------------------------------------------------------------------;

; ball-x : Ball -> Real
; ball-y : Ball -> Real

; GIVEN: a ball
; RETURNS: the x or y position of the ball (in pixels)
; EXAMPLES: (ball-x ball1) => HALF-CANVAS-WIDTH
; STRATEGY: Structural Decomposition on b : Ball

(define (ball-x b)
  (ball-x-pos b))

(define (ball-y b)
  (ball-y-pos b))

; TESTS
(begin-for-test
  (check-equal? (ball-x ball1) 60
                "Test Failed for ball-x")
  (check-equal? (ball-y ball1) 20
                "Test Failed for ball-y"))
;-------------------------------------------------------------------------;

; counter-canvas : String -> Image
; GIVEN: present count of the balls converted to string
; RETURNS: the image, consits of a canvas and the count of the balls 
; EXAMPLES: (counter-canvas "0") => canvas-with-counter-0
; STRATEGY: Function Composition

(define (counter-canvas cnt)
  (place-image (text cnt COUNTER-FONT-SIZE COUNTER-COLOR)
               COUNTER-X-POS COUNTER-Y-POS EMPTY-CANVAS))

; TESTS
(define canvas-with-counter-0 
  (place-image (text (number->string 0) COUNTER-FONT-SIZE COUNTER-COLOR)
               COUNTER-X-POS COUNTER-Y-POS EMPTY-CANVAS))
(begin-for-test
  (check-equal? (counter-canvas "0") canvas-with-counter-0 
                "Test Failed for counter-canvas"))
;-------------------------------------------------------------------------;

; ball-mode : Ball -> Image
; GIVEN: a ball
; RETURNS: the image of the ball, if ball is selected then solid circle
; else outline circle with radius 20
; EXAMPLES: (ball-mode initial-unselected-ball) => OUTLINE-CIRCLE
; STRATEGY: Structural Decomposition on b : Ball

(define (ball-mode b)
  (if (ball-selected? b) SOLID-CIRCLE OUTLINE-CIRCLE))

; TESTS
(begin-for-test
  (check-equal? (ball-mode initial-unselected-ball) OUTLINE-CIRCLE 
                "Test Failed for ball-mode")
  (check-equal? (ball-mode initial-selected-ball) SOLID-CIRCLE 
                "Test Failed for ball-mode"))
;-------------------------------------------------------------------------;

; world-after-key-event : World KeyEvent -> World
; GIVEN: a world and a keyevent
; RETURNS: the world that should follow the given world after the given
; key event.
; EXAMPLES: (world-after-key-event unpaused-empty-world Q) => 
;            unpaused-empty-world
; STRATEGY: Cases on kev : KeyEvent

(define (world-after-key-event w ke)
  (cond
    [(key=? ke PAUSE) (world-with-paused-toggled w)]
    [(key=? ke N) (key-event-helper w)]
    [else w]))

; TESTS
(begin-for-test
  (check-equal? (world-after-key-event unpaused-empty-world PAUSE)
                paused-empty-world
                "Test Failed for world-after-key-event, keyevent pause")
  (check-equal? (world-after-key-event unpaused-empty-world N) 
                unpaused-world-with-1-ball
                "Test Failed for world-after-key-event, keyevent n")
  (check-equal? (world-after-key-event unpaused-empty-world Q) 
                unpaused-empty-world
                "Test Failed for world-after-key-event, other keyevent"))
;-------------------------------------------------------------------------;

; key-event-helper : World -> World
; GIVEN: a world
; RETURNS: the given world with one ball added to the given world
; EXAMPLES: (world-after-key-event unpaused-empty-world N) =>
;            unpaused-world-with-1-ball
; STRATEGY: Structural Decomposition on w : World

(define (key-event-helper w)
  (make-world (append (cons default-ball empty) (world-lob w)) 
              (world-speed w) (world-paused? w)))

; TESTS
(begin-for-test
  (check-equal? (world-after-key-event unpaused-empty-world N) 
                unpaused-world-with-1-ball
                "Test Failed for world-after-key-event, keyevent n"))
;-------------------------------------------------------------------------;

; world-after-mouse-event : World Integer Integer MouseEvent -> World
; GIVEN: a world, the x y coordinates of the mouse and the mouse event
; RETURNS: the world that should follow the given world after the given
; mouse event.
; EXAMPLES: (world-after-mouse-event world1 100 200 BUTTON-DOWN) => world1
; STRATEGY: Structural Decomposition on w : World

(define (world-after-mouse-event w mx my mev)
  (make-world
   (balls-after-mouse-event (world-lob w) mx my mev)
   (world-speed w)
   (world-paused? w)))

; TESTS
(begin-for-test
  (check-equal? 
   (world-after-mouse-event unpaused-world-with-1-ball 200 150 BUTTON-DOWN)
   (make-world (list (make-ball 200 150 true 200 150 RIGHT)) 8 false)  
   "Test Failed for world-after-mouse-event for button-down"))
;-------------------------------------------------------------------------;

; balls-after-mouse-event : LOB Integer Integer MouseEvent -> LOB
; GIVEN: a listofball, x y coordinates of mouse and the mouseevent
; RETURNS: the listofball following the respective mouseevent 
; EXAMPLES: (balls-after-mouse-event (list initial-unselected-ball)
;           200 150) => (list initial-selected-ball)
; STRATEGY: Higher-Order Function Composition

(define (balls-after-mouse-event lob mx my mev)
  (map
   ; Ball -> Ball
   ; GIVEN: a ball
   ; RETURNS: the ball that should follow the given mouse event
   (lambda (b) (ball-after-mouse-event b mx my mev))
   lob))

; TESTS
; tests follow helper functions
;-------------------------------------------------------------------------;

; ball-after-mouse-event : Ball Integer Integer MouseEvent -> Ball
; GIVEN: a ball, x y coordinates of mouse in pixels and mouseevent
; RETURNS: the ball that should follow the given mouse event
; EXAMPLES:(ball-after-mouse-event initial-selected-ball 200 150 
;          BUTTON-DOWN) => (make-ball 200 150 true 200 150 RIGHT)
; STRATEGY: Cases on mev : MouseEvent

(define (ball-after-mouse-event b mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (ball-after-button-down b mx my)]
    [(mouse=? mev DRAG)        (ball-after-drag b mx my)]
    [(mouse=? mev BUTTON-UP)   (ball-after-button-up b)]
    [else b]))

; TESTS
(begin-for-test
  (check-equal? 
   (ball-after-mouse-event initial-selected-ball 200 150 BUTTON-DOWN)
   (make-ball 200 150 true 200 150 RIGHT)
   "Test Failed for ball-after-mouse-event, button-down event")
  (check-equal? 
   (ball-after-mouse-event initial-selected-ball 200 150 DRAG)
   (make-ball 400 300 true 200 150 RIGHT)
   "Test Failed for ball-after-mouse-event, drag event")
  (check-equal? 
   (ball-after-mouse-event initial-selected-ball 200 150 BUTTON-UP)
   initial-unselected-ball 
   "Test Failed for ball-after-mouse-event, button-up event")
  (check-equal? 
   (ball-after-mouse-event initial-unselected-ball 200 150 OTHER)
   initial-unselected-ball 
   "Test Failed for ball-after-mouse-event, other event"))
;-------------------------------------------------------------------------;

; ball-after-button-down : Ball Integer Integer -> Ball
; GIVEN: a ball, x and y coordinates of mouse in pixels
; RETURNS: the ball following a button-down at the given location.
; EXAMPLES: (ball-after-mouse-event initial-unselected-ball 00 150 
;            BUTTON-DOWN) => initial-unselected-ball
; STRATEGY: Structural Decomposition on b : Ball

(define (ball-after-button-down b x y)
  (if (in-ball? b x y)
      (make-ball 
       (ball-x-pos b) (ball-y-pos b) true x y (ball-direction? b)) b))

; TESTS
(begin-for-test
  (check-equal? 
   (ball-after-mouse-event initial-unselected-ball 00 150 BUTTON-DOWN)
   initial-unselected-ball "Test Failed for ball-after-button-down"))
;-------------------------------------------------------------------------;

; in-ball? : Ball Integer Integer -> Boolean
; GIVEN: a ball, x and y coordinates of the mouse.
; RETURNS: true iff the mouse coordinates is on the ball else false
; EXAMPLES: (in-ball? initial-unselected-ball 200 150) => true
; STRATEGY: Structural Decomposition on b : Ball

(define (in-ball? b mouse-x mouse-y)
  (>= CIRCLE-RADIUS
      (sqrt (+ (sqr(- mouse-x (ball-x-pos b)))
               (sqr(- mouse-y (ball-y-pos b)))))))

; TESTS
(begin-for-test
  (check-equal? (in-ball? initial-unselected-ball 200 150) true 
                "Test Failed for in-ball? output should be true")
  (check-equal? (in-ball? initial-unselected-ball 300 150) false 
                "Test Failed for in-ball? output should be false"))
;-------------------------------------------------------------------------;

; ball-after-drag : Ball Integer Integer -> Ball
; GIVEN: a ball, x and y coordinates of mouse in pixels
; RETURNS: a ball following a drag at the given location
; EXAMPLES: (ball-after-drag ball1 300 250) => ball1
; STRATEGY: Structural Decomposition on b : Ball

(define (ball-after-drag b mouse-new-x mouse-new-y)
  (if (ball-selected? b)
      (make-ball  (- mouse-new-x (- (ball-mouse-x b) (ball-x-pos b)))
                  (- mouse-new-y (- (ball-mouse-y b) (ball-y-pos b)))
                  true mouse-new-x mouse-new-y (ball-direction? b)) b))

; TESTS
(begin-for-test
  (check-equal? 
   (ball-after-drag ball1 300 250) ball1 
   "Test Failed for ball-after-drag"))
;-------------------------------------------------------------------------;

; ball-after-button-up : Ball -> Ball
; GIVEN: a ball
; RETURNS: the ball following a button-up at the given location 
; EXAMPLES: (ball-after-button-up ball1) => ball1
; STRATEGY: Structural Decomposition on b : Ball

(define (ball-after-button-up b)
  (if (ball-selected? b) (ball-after-button-up-helper b) b))

; TESTS
(begin-for-test
  (check-equal? (ball-after-button-up (make-ball 60 20 false 0 0 RIGHT))
                (make-ball 60 20 false 0 0 RIGHT) 
                "Test Failed for ball-after-button-up"))
;-------------------------------------------------------------------------;

; ball-after-button-up-helper : Ball -> Ball
; GIVEN: a ball
; RETURNS: the ball following a button-up at the given location
; EXAMPLES: (ball-after-button-up-helper initial-selected-ball) =>
;           initial-unselected-ball
; STRATEGY: Structural Decomposition on b : Ball

(define (ball-after-button-up-helper b)
  (make-ball (ball-x-pos b) (ball-y-pos b) false 
             DEFAULT-MOUSE-X DEFAULT-MOUSE-Y (ball-direction? b)))

; TESTS
(begin-for-test
  (check-equal? (ball-after-button-up-helper initial-selected-ball)
                initial-unselected-ball 
                "Test Failed for ball-after-button-up-helper, shall return 
                 unselected ball"))
;-------------------------------------------------------------------------;

; world-balls : World -> ListOf<Ball>
; GIVEN: a world
; RETURNS: the list of balls that are in the box.
; EXAMPLE: (world-balls unpaused-world-with-1-ball) =>
;          (list initial-unselected-ball)
; STRATEGY: Structural Decomposition on w : World

(define (world-balls w)
  (world-lob w))

; TESTS
(begin-for-test
  (check-equal? (world-balls unpaused-world-with-1-ball) 
                (list initial-unselected-ball)
                "Test Failed for world-balls, expected list of 1 ball"))
;-------------------------------------------------------------------------;