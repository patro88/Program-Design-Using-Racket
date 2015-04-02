;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; balls-in-box.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;; PROBLEM STATEMENT :                                                   ;;
;;                                                                       ;;
;; Program should display some balls that you can drag on a canvas.      ;;
;; The program starts with a canvas 400 pixels wide and 300 pixels       ;;
;; high, with no balls. Hitting the "n" key should create a new ball     ;;
;; in the center of the canvas. You can select a ball by doing a         ;;
;; button-down inside the ball. When a ball is selected, you can drag    ;;
;; it with the mouse. It becomes unselected when you do a mouse-up.      ;;
;; The balls should be displayed as a circle with radius 20. An          ;;
;; unselected ball should be displayed as an outline; a selected ball    ;;
;; should be displayed solid. In addition to the balls, you should       ;;
;; display the number of balls currently on the canvas.                  ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; start with (run 0)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RUN FUNCTION.

;; run : Any -> World
;; GIVEN: An argument, which is ignored.
;; EFFECT: runs the world at tick rate of 0.25 secs/tick.
;; RETURNS: the final state of the world. Note that the world does not 
;; respond to time passing, so the tick rate doesn't make a difference.

(define (run any)
  (big-bang (initial-world any)
            (on-tick world-after-tick 0.25)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; dimensions and attributes of canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; dimensions and attributes of circle
(define CIRCLE-RADIUS 20)
(define CIRCLE-COLOR "green")
(define SOLID-MODE   "solid")
(define OUTLINE-MODE "outline")

;; solid circle image
(define SOLID-CIRCLE   (circle CIRCLE-RADIUS SOLID-MODE CIRCLE-COLOR))
(define OUTLINE-CIRCLE (circle CIRCLE-RADIUS OUTLINE-MODE CIRCLE-COLOR))

;; mouse coordinates default value
(define DEFAULT-MOUSE-X 0)
(define DEFAULT-MOUSE-Y 0)

;; mouse-events
(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")

;; counter attributes
(define COUNTER-X-POS HALF-CANVAS-WIDTH)
(define COUNTER-Y-POS 30)
(define COUNTER-COLOR "black")
(define COUNTER-FONT-SIZE 24)

;; key button
(define N "n")
(define Q "q")
(define ZERO 0)
(define ONE 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct ball (x-pos y-pos selected? mouse-x mouse-y))

;; A Ball is a (make-ball Integer Integer Boolean Integer Integer)

;; INTERPRETATION:
;;  x-pos     : tells how far the center of ball is from x-axis, in pixels
;;  y-pos     : tells how far the center of ball is from y-axis, in pixels
;;  selected? : tells whether or not the ball is selected
;;  mouse-x   : tells how far the mouse is from x-axis, in pixels
;;  mouse-y   : tells how far the mouse in from y-axis, in pixels

;; TEMPLATE:
;; ball-fn : Ball -> ??
;;(define (ball-fn b)
;;  (... (ball-x-pos b) (ball-y-pos b) (ball-selected? b) 
;;       (ball-mouse-x b) (ball-mouse-y b)))

;; EXAMPLES for tests:
(define ball1 (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT false 
                         DEFAULT-MOUSE-X DEFAULT-MOUSE-Y))
(define ball2 (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT true 
                         DEFAULT-MOUSE-X DEFAULT-MOUSE-Y))

;; default ball to be used in keyevent function
(define default-ball (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT
                                false DEFAULT-MOUSE-X DEFAULT-MOUSE-Y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfBall (LOB) is one of
;; -- empty            interp: a sequence with no elements
;; -- (cons Ball LOB)  interp: (cons Ball LOB) represents a sequence whose
;;                             first element is Ball and whose other 
;;                             elements are represented by LOB

;; TEMPLATE:
;; lob-fn : LOB -> ??
;;(define (lob-fn lob)
;;  (cond
;;    [(empty? lob) ...]
;;    [else (... (first lob)
;;               (lob-fn (rest lob)))]))

;; EXAMPLES for tests:
(define lob1 (cons ball1 (cons ball2 empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An World is a ListOfBalls (LOB).

;; INTERPRETATION:
;; It is the list of balls on the canvas in the order they are created.

;; EXAMPLES for testing:

(define world1 (list ball1))
(define world2 (list ball2))

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any  -> World
;; GIVEN: An argument, which is ignored.
;; RETURNS: a world with no balls.
;; EXAMPLES: (initial-world "Abhishek") => empty
;; STRATEGY: Function Composition

(define (initial-world any)
  empty)

;; TESTS
(begin-for-test 
  (check-equal? (initial-world 12345) empty 
                "Test Failed for initial-world")
  (check-equal? (initial-world "ABCD") empty 
                "Test Failed for initial-world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow w after a tick.
;; EXAMPLES: (world-after-tick lob1) => lob1 
;; STRATEGY: Function Composition

(define (world-after-tick w)
  w)

;; TESTS
(begin-for-test
  (check-equal? (world-after-tick lob1) lob1
                "Test Failed for world-after-tick"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene world1) default-canvas
;; STRATEGY: Function composition

(define (world-to-scene w)
  (world-to-scence-helper w (number->string (length w))))

;; TESTS
(begin-for-test
  (check-equal? (world-to-scene world1) canvas-with-1-ball 
                "Test Failde for world-to-scene"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scence-helper : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene world1) = canvas-with-1-ball
;; STRATEGY: Structural Decomposition on lob : World

(define (world-to-scence-helper lob count )
  (cond
    [(empty? lob)(counter-canvas count)]
    [else (place-image 
           (ball-mode(first lob)) (ball-x (first lob)) (ball-y (first lob))
           (world-to-scence-helper (rest lob) count))]))

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

(begin-for-test
  (check-equal? (world-to-scene empty) default-canvas 
                "Test Failed for world-to-scene")
  (check-equal? (world-to-scene world1) canvas-with-1-ball 
                "Test Failed for world-to-scene"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; counter-canvas : String -> Image
;; GIVEN: present count of the balls converted to string
;; RETURNS: the image, consits of a canvas and the count of the balls 
;; EXAMPLES: (counter-canvas "0") => canvas-with-counter-0
;; STRATEGY: Function Composition

(define (counter-canvas cnt)
  (place-image (text cnt COUNTER-FONT-SIZE COUNTER-COLOR)
               COUNTER-X-POS COUNTER-Y-POS EMPTY-CANVAS))

;; TESTS

(define canvas-with-counter-0 
  (place-image (text (number->string 0) COUNTER-FONT-SIZE COUNTER-COLOR)
               COUNTER-X-POS COUNTER-Y-POS EMPTY-CANVAS))

(begin-for-test
  (check-equal? (counter-canvas "0") canvas-with-counter-0 
                "Test Failed for counter-canvas"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; ball-x : Ball -> Integer
;; ball-y : Ball -> Integer

;; GIVEN: a ball
;; RETURNS: the x or y position of the ball (in pixels)
;; EXAMPLES: (ball-x ball1) => HALF-CANVAS-WIDTH
;; STRATEGY: Structural Decomposition on b : Ball

(define (ball-x b)
  (ball-x-pos b))

(define (ball-y b)
  (ball-y-pos b))

;; TESTS
(begin-for-test
  (check-equal? (ball-x ball1) HALF-CANVAS-WIDTH
                "Test Failed for ball-x")
  (check-equal? (ball-y ball1) HALF-CANVAS-HEIGHT
                "Test Failed for ball-y"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-mode : Ball -> Image
;; GIVEN: a ball
;; RETURNS: the image of the ball, if ball is selected then solid circle
;; else outline circle with radius 20
;; EXAMPLES: (ball-mode ball1) => OUTLINE-CIRCLE
;; STRATEGY: Structural Decomposition on b : Ball

(define (ball-mode b)
  (if (ball-selected? b) SOLID-CIRCLE OUTLINE-CIRCLE))

;; TESTS
(begin-for-test
  (check-equal? (ball-mode ball1) OUTLINE-CIRCLE 
                "Test Failed for ball-mode")
  (check-equal? (ball-mode ball2) SOLID-CIRCLE 
                "Test Failed for ball-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world and a keyevent
;; RETURNS: the world that should follow the given world after the given
;; key event.
;; EXAMPLES: (world-after-key-event lob1 "q") => lob1
;; STRATEGY: Cases on kev : KeyEvent

(define (world-after-key-event w ke)
  (cond
    [(key=? ke N) (append (cons default-ball empty) w)]
    [else w]))

;; TESTS
(begin-for-test
  (check-equal? (world-after-key-event lob1 N) (cons default-ball lob1) 
                "Test Failed for world-after-key-event")
  (check-equal? (world-after-key-event lob1 Q) lob1 
                "Test Failed for world-after-key-event"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world, the x y coordinates of the mouse and the mouse event
;; RETURNS: the world that should follow the given world after the given
;; mouse event.
;; EXAMPLES: (world-after-mouse-event world1 100 200 BUTTON-DOWN) => world1
;; STRATEGY: Cases on mev : MouseEvent

(define (world-after-mouse-event w mouse-x mouse-y mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (world-after-button-down w mouse-x mouse-y)]
    [(mouse=? mev DRAG)        (world-after-drag w mouse-x mouse-y)]
    [(mouse=? mev BUTTON-UP)   (world-after-button-up w)]
    [else w]))

;; TESTS
(begin-for-test
  (check-equal? (world-after-mouse-event world1 100 200 BUTTON-DOWN) 
                world1 "Test Failed for world-after-mouse-event"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-ball? : World Integer Integer -> World
;; GIVEN: a ball, x and y coordinates of the mouse.
;; RETURNS: true iff the mouse coordinates is on the ball else false
;; EXAMPLES: (in-ball? ball1 200 150) => true
;; STRATEGY: Structural Decomposition on b : Ball

(define (in-ball? b mouse-x mouse-y)
  (>= CIRCLE-RADIUS
      (sqrt (+ (sqr(- mouse-x (ball-x-pos b)))
               (sqr(- mouse-y (ball-y-pos b)))))))

;; TESTS
(begin-for-test
  (check-equal? (in-ball? ball1 200 150) true "Test Failed for in-ball?")
  (check-equal? (in-ball? ball1 120 150) false "Test Failed for in-ball?"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-down : World Integer Integer -> World
;; GIVEN: a world, mouse-x and mouse-y coordinates of the mouse.
;; RETURNS: produce the world that should follow the given mouse event
;; EXAMPLES: (world-after-mouse-event world1 200 150 BUTTON-DOWN) =>
;;     (list (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT true 200 150))
;; STRATEGY: Structural Decomposition on lob : World

(define (world-after-button-down lob mouse-x mouse-y)
  (cond
    [(empty? lob) empty]
    [else (cons (button-down-helper (first lob) mouse-x mouse-y)
                (world-after-button-down (rest lob) mouse-x mouse-y))]))

;; TESTS
(begin-for-test
  (check-equal? (world-after-mouse-event world1 200 150 BUTTON-DOWN)
                (list (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT 
                                 true 200 150)) 
                "Test Failed for world-after-button-down"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; button-down-helper : Ball Integer Integer -> Ball
;; GIVEN: a ball, mouse-x and mouse-y coordinates of the mouse.
;; RETURNS: produce the ball that should follow the given mouse event. If
;; the mouse is on the ball then ball-selected? is true else false
;; EXAMPLES: (button-down-helper ball1 200 150) =>
;;         (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT true 200 150)
;; STRATEGY: Structural Decomposition on b : Ball

(define (button-down-helper b mouse-x mouse-y)
  (if (in-ball? b mouse-x mouse-y)
      (make-ball (ball-x-pos b) (ball-y-pos b) true mouse-x mouse-y) b))

;; TESTS
(begin-for-test
  (check-equal? (button-down-helper ball1 200 150)
                (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT 
                           true 200 150)
                "Test Failed for button-down-helper")
  (check-equal? (button-down-helper ball1 0 0) ball1
                "Test Failed for button-down-helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-drag : World Integer Integer -> World
;; GIVEN: a world, mouse-x and mouse-y coordinates of the mouse.
;; RETURNS: produce the world that should follow the given mouse event
;; EXAMPLES:  (world-after-mouse-event world1 200 150 "enter") => world1
;; STRATEGY: Structural Decomposition on lob : World

(define (world-after-drag lob mouse-new-x mouse-new-y)
  (cond
    [(empty? lob) empty]
    [else (cons (drag-helper (first lob) mouse-new-x mouse-new-y)
                (world-after-drag (rest lob) mouse-new-x mouse-new-y))]))

;; TESTS
(begin-for-test
  (check-equal? (world-after-mouse-event world1 200 150 DRAG)
                (list (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT 
                                 false 0 0))
                "Test Failed for world-after-drag")
  (check-equal? (world-after-mouse-event world1 200 150 "enter") world1
                "Test Failed for world-after-drag"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; drag-helper : Ball Integer Integer -> Ball
;; GIVEN: a ball, mouse-x and mouse-y coordinates of the mouse.
;; RETURNS: produce the ball that should follow the given mouse event
;; EXAMPLES: (drag-helper ball1 200 150) => ball1;; 
;; STRATEGY: Structural Decomposition on b : Ball

(define (drag-helper b mouse-new-x mouse-new-y)
  (if (ball-selected? b)
      (make-ball (- mouse-new-x (- (ball-mouse-x b) (ball-x-pos b)))
                 (- mouse-new-y (- (ball-mouse-y b) (ball-y-pos b)))
                  true mouse-new-x mouse-new-y) b))

(begin-for-test
  (check-equal? (drag-helper ball2 200 150)
                (make-ball 400 300 true 200 150) 
                "Test Failed for drag-helper")
  (check-equal? (drag-helper ball1 200 150) ball1
                "Test Failed for drag-helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-up : World -> World
;; GIVEN: A world
;; RETURNS: produce the world that should follow the given mouse event
;; EXAMPLES: (world-after-mouse-event world2 0 0 BUTTON-UP) =>
;;        (list (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT false 0 0))
;; STRATEGY: Structural Decomposition on lob: World

(define (world-after-button-up lob)
  (cond
    [(empty? lob) empty]
    [else (cons (button-up-helper (first lob)) 
                (world-after-button-up (rest lob)))]))

;; TESTS
(begin-for-test
  (check-equal? (world-after-mouse-event world2 0 0 BUTTON-UP)
                (list (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT 
                                 false 0 0)) 
                "Test Failed for world-after-button-up"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; button-up-helper : Ball -> Ball
;; GIVEN: a ball
;; RETURNS: produce a ball that should follow the given mouse event
;; EXAMPLES: (button-up-helper ball1) => ball1
;; STRATEGY: Structural Decomposition on b : Ball

(define (button-up-helper b)
  (if (ball-selected? b) (make-ball (ball-x-pos b) (ball-y-pos b) false
                                    DEFAULT-MOUSE-X DEFAULT-MOUSE-Y) b))

;; TESTS
(begin-for-test
  (check-equal? (button-up-helper ball1) ball1
                "Test Failed for button-up-helper")
  (check-equal? (button-up-helper ball2) 
                (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT false 
                 DEFAULT-MOUSE-X DEFAULT-MOUSE-Y) 
                "Test Failed for button-up-helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; world-balls : World -> ListOfBalls
;; GIVEN: a world
;; RETURNS: the list of balls that are in the box.
;; EXAMPLE: (world-balls world1) => world1
;; STRATEGY: Function Composition

(define (world-balls w)
  w)

;; TESTS
(begin-for-test
  (check-equal? (world-balls world1) world1 
                "Test Failed for world-balls"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;