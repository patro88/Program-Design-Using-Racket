;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; rectangle.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PROBLEM STATEMENT :
;; 
;; - Selectable and Draggable rectangle.
;; - Depressing the mouse button within the rectangle causes the rectangle
;;   to be "selected". This is shown visually by replacing the solid green
;;   rectangle by a green outline rectangle. The location where the mouse 
;;   grabbed the rectangle should be indicated by a solid red circle of 
;;   radius 5 pixels.
;; - Once the rectangle has been grabbed, you should be able to drag it 
;;   around the Universe canvas with the mouse. As you drag it, the position
;;   of the mouse within the rectangle (as indicated by the red circle), 
;;   should not change. When the mouse button is released, the rectangle 
;;   should go back to its unselected state (solid green) in its new location. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; start with (run 0)

(provide run)
(provide initial-world)
(provide world-x)
(provide world-y)
(provide world-selected?)
(provide world-after-mouse-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RUN FUNCTION.

;; run : Any -> World
;; GIVEN: any value
;; EFFECT: ignores its argument and starts the interactive program.
;; RETURNS: the final state of the world.

(define (run any)
  (big-bang (initial-world any)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; dimensions and attributes of canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; dimensions and attributes of rectangle
(define RECTANGLE-WIDTH 100)
(define RECTANGLE-HIGH 60)
(define RECTANGLE-COLOR "green")
(define SOLID-RECTANGLE-MODE "solid")
(define OUTLINE-RECTANGLE-MODE "outline")
(define HALF-RECTANGLE-WIDTH  (/ RECTANGLE-WIDTH 2))
(define HALF-RECTANGLE-HEIGHT (/ RECTANGLE-HIGH 2))
(define RECTANGLE-X (/ CANVAS-WIDTH 2))
(define RECTANGLE-Y (/ CANVAS-HEIGHT 2))

;; solid rectangle image
(define SOLID-RECTANGLE
  (rectangle RECTANGLE-WIDTH 
             RECTANGLE-HIGH
             SOLID-RECTANGLE-MODE 
             RECTANGLE-COLOR))

;; outline rectangle image
(define OUTLINE-RECTANGLE 
  (rectangle RECTANGLE-WIDTH 
             RECTANGLE-HIGH
             OUTLINE-RECTANGLE-MODE 
             RECTANGLE-COLOR))

;; dimensions and attributes of circle
(define CIRCLE-RADIUS 5)
(define CIRCLE-COLOR "red")
(define CIRCLE-MODE "solid")

;; solid circle image
(define SOLID-CIRCLE 
  (circle CIRCLE-RADIUS 
          CIRCLE-MODE 
          CIRCLE-COLOR))

;; mouse coordinates default value
(define DEFAULT-MOUSE-X 0)
(define DEFAULT-MOUSE-Y 0)

;; mouse events
(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct world (x y selected? mouse-x mouse-y))

;; A World is a (make-world Integer Integer Boolean Integer Integer)

;; INTERPRETATION: 
;;  x describes how far the center of rectangle is from x-axis, in pixels.
;;  y describes how far the center of rectangle is from y-axis, in pixels.
;;  selected? describes whether or not the rectangle is selected.
;;  mouse-x describes how far the mouse is from x-axis, in pixels.
;;  mouse-y describes how far the mouse in from y-axis, in pixels.

;; TEMPLATE:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-x w) (world-y w) (world-selected? w)
;;        (world-mouse-x w) (world-mouse-y)))

;; EXAMPLES:
;; rectangle is unselected:
;; (define unselected-world 
;;  (make-world RECTANGLE-X RECTANGLE-Y false DEFAULT-MOUSE-X DEFAULT-MOUSE-Y))
;; rectangle is selected:
;; (define selected-world 
;;  (make-world RECTANGLE-X RECTANGLE-Y true DEFAULT-MOUSE-X DEFAULT-MOUSE-Y))

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: the initial world. Ignores its argument.
;; EXAMPLES:
;;  (initial-world "AAA") => (make-world 200 150 false 0 0)
;;  (initial-world 123)   => (make-world 200 150 false 0 0)
;; STRATEGY: Function Composition

(define (initial-world any)
  (make-world RECTANGLE-X RECTANGLE-Y false DEFAULT-MOUSE-X DEFAULT-MOUSE-Y))

;; TESTS
(begin-for-test
  (check-equal? (initial-world "XYZ") (make-world 200 150 false 0 0)
                "Test Failed for initial-world with alphabets")
  (check-equal? (initial-world -123) (make-world 200 150 false 0 0)
                "Test Failed for initial-world with negative numbers"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: A world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLES: 
;;  (world-to-scene (make-world 200 150 false 0 0))
;;          => ((place-image SOLID-RECTANGLE 200 150 EMPTY-CANVAS)
;; STRATEGY: Function Composition

(define (world-to-scene w)
  (if(world-selected? w) (on-selected-helper w) (on-unselected-helper w)))

;; TESTS 
(begin-for-test
  (check-equal? 
   (world-to-scene (make-world 100 200 true 120 220))
   (place-image SOLID-CIRCLE 120 220
                (place-image OUTLINE-RECTANGLE 100 200 EMPTY-CANVAS)))
(check-equal? 
   (world-to-scene (make-world 100 200 false 120 220))
   (place-image SOLID-RECTANGLE 100 200 EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; on-selected-helper : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLES: 
;;  (on-selected-helper (make-world 100 200 true 120 220))
;;  (place-image SOLID-CIRCLE 120 220 
;;    => (place-image OUTLINE-RECTANGLE 100 200 EMPTY-CANVAS))
;; STRATEGY: Structural Decomposition on w : World.

(define (on-selected-helper w)
  (place-image SOLID-CIRCLE (world-mouse-x w) (world-mouse-y w) 
               (place-image OUTLINE-RECTANGLE (world-x w) (world-y w) 
                            EMPTY-CANVAS)))

;; TESTS
(begin-for-test
  (check-equal? 
   (on-selected-helper (make-world 100 200 true 120 220))
   (place-image SOLID-CIRCLE 120 220 
               (place-image OUTLINE-RECTANGLE 100 200 EMPTY-CANVAS))
   "Test Failed for on-selected-helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; on-unselected-helper : World -> Scene
;; GIVEN: a world
;; RETURNS: a scene that portrays the given world.
;; EXAMPLES: 
;;  (on-unselected-helper (make-world 100 200 false 120 220))
;;          => (place-image SOLID-RECTANGLE 100 200 EMPTY-CANVAS)
;; STRATEGY: Structural Decomposition on w : World

(define (on-unselected-helper w)
  (place-image SOLID-RECTANGLE (world-x w) (world-y w) EMPTY-CANVAS))

;; TESTS
(begin-for-test
  (check-equal? 
   (on-unselected-helper (make-world 100 200 false 120 220))
   (place-image SOLID-RECTANGLE 100 200 EMPTY-CANVAS)
   "Test Failed for on-unselected-helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world, x, y coordinates of the mouse and mouseevent.
;; RETURNS: produce the world that should follow the given mouse event
;; EXAMPLES: 
;;  (world-after-mouse-event (make-world 100 200 false 0 0) 150 150) "drag")
;;  => (make-world 250 350 true 150 150) 
;; STRATEGY: Cases on mev : MouseEvent

(define (world-after-mouse-event w mouse-x mouse-y mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (world-after-button-down w mouse-x mouse-y)]
    [(mouse=? mev DRAG)        (world-after-drag w mouse-x mouse-y)]
    [(mouse=? mev BUTTON-UP)   (world-after-button-up w)]
    [else w]))

;; TESTS
(begin-for-test
  (check-equal? 
   (world-after-mouse-event (make-world 100 200 true 0 0) 150 150 "drag")
   (make-world 250 350 true 150 150) 
   "Test Faile for world-after-mouse-event for drag")
  (check-equal? 
   (world-after-mouse-event (make-world 100 200 false 0 0) 
                            140 210 "button-down")
   (make-world 100 200 true 140 210) 
   "Test Faile for world-after-mouse-event for button-down")
  (check-equal? 
   (world-after-mouse-event (make-world 100 200 true 0 0) 150 150 "button-up")
   (make-world 100 200 false 0 0) 
   "Test Faile for world-after-mouse-event for button-up")
  (check-equal? 
   (world-after-mouse-event (make-world 100 200 true 0 0) 150 150 "enter")
   (make-world 100 200 true 0 0) 
   "Test Faile for world-after-mouse-event for enter"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-down : World Integer Integer -> World
;; GIVEN: A world, mouse-x and mouse-y coordinates of the mouse.
;; RETURNS: produce the world that should follow the given mouse event
;; EXAMPLES: (world-after-button-down (make-world 10 20 false 0 0) 30 40)
;;        => (make-world 10 20 true 30 40)
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-button-down w mouse-x mouse-y)
  (if (in-rectangle? w mouse-x mouse-y)
      (make-world (world-x w) (world-y w) true mouse-x mouse-y ) w))

;; TESTS
(begin-for-test
  (check-equal? 
   (world-after-button-down (make-world 10 20 false 0 0) 30 40)
   (make-world 10 20 true 30 40) "Test Failed for world-after-button-down")
  (check-equal? 
   (world-after-button-down (make-world 10 20 false 0 0) 300 400)
   (make-world 10 20 false 0 0) "Test Failed for world-after-button-down"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-drag : World Integer Integer -> World
;; GIVEN: A world, mouse-x and mouse-y coordinates of the mouse.
;; RETURNS: produce the world that should follow the given mouse event
;; EXAMPLES: (world-after-drag (make-world 100 200 true 0 0) 150 150)
;;        => (make-world 250 350 true 150 150)
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-drag w mouse-new-x mouse-new-y)
  (if (world-selected? w)
      (make-world (- mouse-new-x (- (world-mouse-x w) (world-x w)))
                  (- mouse-new-y (- (world-mouse-y w) (world-y w)))
                  true mouse-new-x mouse-new-y) w))

;; TESTS
(begin-for-test
  (check-equal? (world-after-drag (make-world 100 200 true 0 0) 150 150)
                (make-world 250 350 true 150 150))
  (check-equal? (world-after-drag (make-world 100 200 false 0 0) 150 150)
                (make-world 100 200 false 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-up : World -> World
;; GIVEN: A world
;; RETURNS: produce the world that should follow the given mouse event
;; EXAMPLES: (world-after-button-up (make-world 200 150 true 0 0))
;;         => (make-world 200 150 false 0 0)
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-button-up w)
  (if (world-selected? w) (world-after-button-up-helper w) w))

;; TESTS
(begin-for-test
  (check-equal? (world-after-button-up (make-world 200 150 true 0 0))
                (make-world 200 150 false 0 0) 
                "Test Failed for world-after-button-up with true as input")
  (check-equal? (world-after-button-up (make-world 200 150 false 0 0))
                (make-world 200 150 false 0 0)
                "Test Failed for world-after-button-up with false as input"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-up-helper : World -> World
;; GIVEN: A world
;; RETURNS: produce the world that should follow the given mouse event
;; EXAMPLES: (world-after-button-up-helper (make-world 100 200 true 100 100)
;;            => (make-world 100 200 false 0 0)
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-button-up-helper w)
  (make-world (world-x w) (world-y w) false DEFAULT-MOUSE-X DEFAULT-MOUSE-Y))

;; TESTS
(begin-for-test
  (check-equal? (world-after-button-up-helper (make-world 100 200 true 20 40))
                (make-world 100 200 false 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-rectangle? : World Integer Integer -> Boolean
;; GIVEN: A world, x and y coordinates of the mouse.
;; RETURNS: true if the mouse coordinates is on the rectangle else false
;; EXAMPLES: (in-rectangle? (make-world 200 150 true 0 0) 240 170) => True
;;           (in-rectangle? (make-world 200 150 true 0 0) 130 120) => False
;; STRATEGY: Function Composition

(define (in-rectangle? w mouse-x mouse-y)
  (and (in-rectangle-width? w mouse-x mouse-y)
       (in-rectangle-height? w mouse-x mouse-y)))

;; TESTS
(begin-for-test
  (check-equal? (in-rectangle? (make-world 200 150 true 0 0) 240 170) true
                "Test Failed for in-rectangle? Expected true")
  (check-equal? (in-rectangle? (make-world 200 150 true 0 0) 290 120) false
                "Test Failed for in-rectangle? Expected true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-rectangle-width? : World Integer Integer -> Boolean
;; in-rectangle-height?: World Integer Integer -> Boolean

;; GIVEN: A world, x and y coordinates of the mouse.
;; RETURNS: true or false based on below scenario.
;; in-rectangle-width? returns True if the mouse x-cord in within the width
;; of the rectangle else false. 
;; in-rectangle-height? returns True if the mouse y-cord is within the height
;; of the rectangle.
;; EXAMPLES: 
;;  (in-rectangle-width? (make-world 200 150 true 0 0) 220 200) => true
;;  (in-rectangle-height? (make-world 200 150 true 0 0) 220 100) => false
;; STRATEGY: Structural Decomposition on w : World

(define (in-rectangle-width? w mouse-x mouse-y)
  (<= (- (world-x w) HALF-RECTANGLE-WIDTH) mouse-x 
      (+ (world-x w) HALF-RECTANGLE-WIDTH)))

(define (in-rectangle-height? w mouse-x mouse-y)
  (<= (- (world-y w) HALF-RECTANGLE-HEIGHT) mouse-y 
      (+ (world-y w) HALF-RECTANGLE-HEIGHT)))

;; TESTS
(begin-for-test
  (check-equal? (in-rectangle-width? (make-world 200 150 true 0 0) 220 200)
                true "Test Failed for in-rectangle-width? Expected true")
  (check-equal? (in-rectangle-width? (make-world 200 150 false 0 0) 90 200)
                false "Test Failed for in-rectangle-width? Expected false")
  (check-equal? (in-rectangle-height? (make-world 200 150 true 0 0) 220 180)
                true "Test Failed for in-rectangle-height? Expected true")
  (check-equal? (in-rectangle-height? (make-world 200 150 false 0 0) 220 310)
                false "Test Failed for in-rectangle-height? Expected false"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
