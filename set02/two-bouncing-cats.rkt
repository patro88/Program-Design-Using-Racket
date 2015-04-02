;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname two-bouncing-cats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; two-bouncing-cats.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; two bouncing cats.
;; Two bouncing cats are individually draggable. However, space pauses 
;; or unpauses the whole system.

;; draggable cat.
;; like falling cat, but user can drag the cat with the mouse. button-down
;; to select, drag to move, button-up to release.

;; falling cat.  
;; A cat falls from the top of the scene. The user can pause/unpause the
;; cat with the space bar.

;; bouncing cat.
;; A cat when hits the boundary it should start moving in the opposite
;; direction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; start with (main 100)

(provide initial-world)
(provide world-after-tick)
(provide world-after-mouse-event)
(provide world-after-key-event)
(provide world-cat1)
(provide world-cat2)
(provide world-paused?)
(provide cat-x-pos)
(provide cat-y-pos)
(provide cat-selected?)
(provide cat-north?)
(provide cat-east?)
(provide cat-south?)
(provide cat-west?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Integer -> World
;; GIVEN: the initial y-position of the cats
;; EFFECT: runs the simulation, starting with the cats falling
;; RETURNS: the final state of the world

(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 0.5)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define ZERO 0)
(define BUFFER 2)

;; cat image
(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD (/ CANVAS-WIDTH 3))
(define CAT2-X-COORD (* 2 CAT1-X-COORD))

;; dimensions of the cat
(define HALF-CAT-WIDTH  (/ (image-width  CAT-IMAGE) 2))
(define HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2))

;; directions
(define NORTH "NORTH")
(define SOUTH "SOUTH")
(define EAST "EAST")
(define WEST "WEST")

;; MouseEvents
(define BUTTON-DOWN "button-down")
(define DRAG "drag")
(define BUTTON-UP "button-up")
(define OTHER "enter")

;; KeyEvents
(define PAUSE " ")
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  DATA DEFINITIONS

(define-struct world (cat1 cat2 paused?))

;; A World is a (make-world Cat Cat Boolean)

;; INTERPRETATION:
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused

;; TEMPLATE:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-cat1 w) (world-cat2 w) (world-paused? w)))

;; EXAMPLES:
;; In cat struct example

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct cat (x-pos y-pos selected? direction))

;; A Cat is a (make-cat Integer Integer Boolean Direction)

;; INTERPRETATION: 
;; x-pos, y-pos give the position of the cat in pixels. 
;; selected? describes whether or not the cat is selected.
;; direction describes the direction of the cat.

;; TEMPLATE:
;; cat-fn : Cat -> ??
;; (define (cat-fn c)
;; (... (cat-x-pos c) (cat-y-pos c) (cat-selected? c) (cat-direction c))

;; EXAMPLES of cats, for testing
;; cat1
(define selected-cat1-at-120 (make-cat CAT1-X-COORD 120 true  SOUTH))
(define selected-cat1-at-128 (make-cat CAT1-X-COORD 1128 true SOUTH))
(define unselected-cat1-at-120 (make-cat CAT1-X-COORD 120 false SOUTH))
(define unselected-cat1-at-128 (make-cat CAT1-X-COORD 128 false SOUTH))
;; cat2
(define selected-cat2-at-120 (make-cat CAT2-X-COORD 120 true  SOUTH))
(define selected-cat2-at-128 (make-cat CAT2-X-COORD 128 true  SOUTH))
(define selected-cat2-at-135 (make-cat CAT2-X-COORD 135 true SOUTH))
(define unselected-cat2-at-120 (make-cat CAT2-X-COORD 120 false SOUTH))
(define unselected-cat2-at-128 (make-cat CAT2-X-COORD 128 false SOUTH))
(define unselected-cat2-at-135 (make-cat CAT2-X-COORD 135 false SOUTH))
(define selected-cat2-at-135-up (make-cat CAT2-X-COORD 135 true NORTH))
(define selected-cat2-at-135-down (make-cat CAT2-X-COORD 135 true SOUTH))
(define selected-cat2-at-135-right (make-cat CAT2-X-COORD 135 true EAST))
(define selected-cat2-at-135-left (make-cat CAT2-X-COORD 135 true WEST))

;; EXAMPLES of worlds, for testing
(define unpaused-unselected-world-at-120
  (make-world unselected-cat1-at-120 unselected-cat2-at-120 false))
(define unpaused-selected-world-at-120
  (make-world unselected-cat1-at-120 selected-cat2-at-120 false))
(define unpaused-unselected-world-at-128
  (make-world unselected-cat1-at-128 unselected-cat2-at-128 false))
(define unpaused-world-at-120
  (make-world unselected-cat1-at-120 selected-cat2-at-135 false))
(define unpaused-world-at-120-after-tick
  (make-world unselected-cat1-at-128 selected-cat2-at-135 false))
(define unpaused-world-at-120-up
  (make-world unselected-cat1-at-120 selected-cat2-at-135-up false))
(define unpaused-world-at-120-down
  (make-world unselected-cat1-at-120 selected-cat2-at-135-down false))
(define unpaused-world-at-120-right
  (make-world unselected-cat1-at-120 selected-cat2-at-135-right false))
(define unpaused-world-at-120-left
  (make-world unselected-cat1-at-120 selected-cat2-at-135-left false))
(define paused-world-at-120
  (make-world unselected-cat1-at-120 selected-cat2-at-135 true))
(define paused-unselected-world-at-120
  (make-world unselected-cat1-at-120 unselected-cat2-at-120 true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Direction is one of
;;  - "NORTH"  (interp: cat move towards NORTH i.e up)
;;  - "SOUTH"  (interp: cat move towards SOUTH i.e down)
;;  - "EAST"   (interp: cat move towards EAST i.e  right)
;;  - "WEST"   (interp: cat move towards WEST i.e  left)

;; TEMPLATE:
;; direction-fn : Direction -> ??
;; (define (direction-fn d)
;;  (cond
;;    [(string=? d NORTH)...]
;;    [(string=? d SOUTH)...]
;;    [(string=? d EAST) ...]
;;    [(string=? d WEST) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Integer -> World
;; GIVEN: a y-coordinate
;; RETURNS: a world with two unselected cats, spaced evenly across the
;; canvas in the x-direction, and falling, and placed at the given y
;; coordinate.
;; EXAMPLES: (initial-world 120) = unpaused-unselected-world-at-120

(define (initial-world y)
  (make-world
   (make-cat CAT1-X-COORD y false SOUTH)
   (make-cat CAT2-X-COORD y false SOUTH)
   false))

;; TESTS
(begin-for-test
  (check-equal? (initial-world 120) unpaused-unselected-world-at-120
                "Test Failed for Initial world at 120"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow w after a tick.
;; EXAMPLES: 
;; cat falling:(world-after-tick unpaused-world-at-120) 
;;   => unpaused-unselected-world-at-120
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-tick w)
  (if (world-paused? w) w (make-world 
                           (cat-after-tick (world-cat1 w)) 
                           (cat-after-tick (world-cat2 w))
                           (world-paused? w))))

;; TESTS
(begin-for-test
  (check-equal? (world-after-tick unpaused-unselected-world-at-120)
                unpaused-unselected-world-at-128 
                "Test Failed for unpaused world-after-tick at 120")
  (check-equal? (world-after-tick paused-unselected-world-at-120)
                paused-unselected-world-at-120 
                "Test Failed for paused world-after-tick at 120"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-after-tick : Cat -> Cat
;; GIVEN: a cat c
;; RETURNS: the state of given cat after a tick if it were in an unpaused world
;; EXAMPLES: 
;; cat selected:
;; (cat-after-tick selected-cat1-at-120) = selected-cat1-at-120
;; STRATEGY: Structural Decomposition on c : Cat

(define (cat-after-tick c)
  (cat-after-tick-helper (cat-x-pos c) (cat-y-pos c)
                         (cat-selected? c) (cat-direction c)))

;; TESTS: tests follow helper functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-after-tick-helper : Integer Integer Boolean Direction -> Cat
;; GIVEN: a position, value for selected? and direction
;; RETURNS: the cat that should follow one in the given position in an
;; unpaused world 
;; EXAMPLES: (cat-after-tick-helper 200 200 true NORTH)
;;            => (make-cat 200 200 true NORTH)
;; STRATEGY: Function Composition

(define (cat-after-tick-helper x-pos y-pos selected? dir)
  (if selected? 
      (make-cat x-pos y-pos selected? dir)
      (cat-after-tick-bounce-effect x-pos y-pos selected? dir)))

;; TESTS
(begin-for-test
  (check-equal? (cat-after-tick-helper 100 200 true NORTH)
                (make-cat 100 200 true NORTH)
                "Test Failed for cat-after-tick-helper")
  (check-equal? (cat-after-tick-helper 150 200 false SOUTH)
                (make-cat 150 208 false SOUTH)
                "Test Failed for cat-after-tick-helper"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-after-tick-bounce-effect: Integer Integer Boolean Direction -> Cat
;; GIVEN: a position, value for selected? and direction
;; RETURNS: the cat that should follow one in the given position in an
;; unpaused world 
;; EXAMPLES: (cat-after-tick-bounce-effect 200 200 false SOUTH)
;;           => (make-cat 200 208 false SOUTH)
;; STRATEGY: Structural Decomposition on dir : Direction

(define (cat-after-tick-bounce-effect x-pos y-pos selected? dir)
  (cond
    [(string=? dir NORTH) (north_helper x-pos y-pos selected? dir)]
    [(string=? dir SOUTH) (south_helper x-pos y-pos selected? dir)]
    [(string=? dir EAST)  (east_helper  x-pos y-pos selected? dir)]
    [(string=? dir WEST)  (west_helper  x-pos y-pos selected? dir)]))

;; TESTS
(begin-for-test
  (check-equal? (cat-after-tick-bounce-effect 200 200 false SOUTH) 
                (make-cat 200 208 false SOUTH) 
                "Test Failed for south_helper")
  (check-equal? (cat-after-tick-bounce-effect 200 200 false NORTH)
                (make-cat 200 192 false NORTH) 
                "Test Failed for north_helper")
  (check-equal? (cat-after-tick-bounce-effect 200 200 false EAST) 
                (make-cat 208 200 false EAST)  
                "Test Failed for east_helper")
  (check-equal? (cat-after-tick-bounce-effect 200 200 false WEST)  
                (make-cat 192 200 false WEST)  
                "Test Failed for west_helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; south_helper : Integer Integer Boolean Direction -> Cat
;; north_helper : Integer Integer Boolean Direction -> Cat
;; east_helper  : Integer Integer Boolean Direction -> Cat
;; west_helper  : Integer Integer Boolean Direction -> Cat

;; GIVEN: a position, value for selected? and direction
;; RETURNS: a cat that should follow one in the given position
;; EXAMPLES: (south_helper 200 460 false SOUTH)
;;        => (make-cat 200 452 false NORTH)
;; STRATEGY: Function Composition

(define (south_helper x-pos y-pos selected? dir)
  (if(> (+ y-pos CATSPEED) (- CANVAS-HEIGHT HALF-CAT-HEIGHT))
     (make-cat x-pos (- CANVAS-HEIGHT HALF-CAT-HEIGHT BUFFER) selected? NORTH)
     (make-cat x-pos (+ y-pos CATSPEED) selected? SOUTH)))

(define (north_helper x-pos y-pos selected? dir)
  (if(< (- y-pos CATSPEED) HALF-CAT-HEIGHT)
     (make-cat x-pos (+ HALF-CAT-HEIGHT BUFFER) selected? SOUTH) 
     (make-cat x-pos (- y-pos CATSPEED) selected? NORTH)))

(define (east_helper x-pos y-pos selected? dir)
  (if(> (+ x-pos CATSPEED) (- CANVAS-WIDTH HALF-CAT-WIDTH))
     (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH BUFFER) y-pos selected? WEST) 
     (make-cat (+ x-pos CATSPEED) y-pos selected? EAST)))

(define (west_helper x-pos y-pos selected? dir)
  (if(< (- x-pos CATSPEED) HALF-CAT-WIDTH)
     (make-cat (+ HALF-CAT-WIDTH BUFFER) y-pos selected? EAST) 
     (make-cat (- x-pos CATSPEED) y-pos selected? WEST)))

;; TESTS:

(begin-for-test
  (check-equal? (south_helper 200 460 false SOUTH)  
                (make-cat 200 679/2 false NORTH)  
                "Test Failed for south_helper")
  (check-equal? (north_helper 200 0 false NORTH)  
                (make-cat 200 121/2 false SOUTH)  
                "Test Failed for north_helper")
  (check-equal? (east_helper 412 200 false EAST)  
                (make-cat 821/2 200 false WEST)  
                "Test Failed for east_helper")
  (check-equal? (west_helper 0 200 true WEST)  
                (make-cat 79/2 200 true EAST)  
                "Test Failed for west_helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a scene that portrays the given world.
;; EXAMPLE: (world-to-scene paused-world-at-120) should return a canvas with
;; two cats, one at (150,120) and one at (300,120)
;; STRATEGY: Structural Decomposition on w : World

(define (world-to-scene w)
  (place-cat (world-cat1 w)
             (place-cat (world-cat2 w)
                        EMPTY-CANVAS)))

;; TESTS
(define image-of-paused-world-at-120
  (place-image CAT-IMAGE 150 120
    (place-image CAT-IMAGE 300 135
      EMPTY-CANVAS)))

(begin-for-test
  (check-equal?
    (world-to-scene paused-world-at-120)
    image-of-paused-world-at-120))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-cat : Cat Scene -> Scene
;; GIVEN: a cat and a scence
;; RETURNS: a scene like the given one, but with the given cat painted on it.
;; EXAMPLE: (place-image CAT-IMAGE CAT1-X-COORD 120 EMPTY-CANVAS)
;;           => image-at-120
;; STRATEGY: Structural Decomposition on c : Cat

(define (place-cat c s)
  (place-image
   CAT-IMAGE
   (cat-x-pos c) (cat-y-pos c)
   s))

;; TESTS

(define image-at-120 (place-image CAT-IMAGE CAT1-X-COORD 120 EMPTY-CANVAS))

(begin-for-test
 (check-equal? 
   (place-cat selected-cat1-at-120 EMPTY-CANVAS)
   image-at-120
   "(place-cat selected-cat1-at-120 EMPTY-CANVAS) i.e unexpected image")
 (check-equal?
   (place-cat unselected-cat1-at-120 EMPTY-CANVAS)   
   image-at-120
   "(place-cat unselected-ca1t-at-120 EMPTY-CANVAS) i.e unexpected image"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world after the given key
;; event.
;; EXAMPLES: (world-after-key-event unpaused-world-at-120 UP)
;;          => unpaused-world-at-120-up
;; STRATEGY: Cases on kev : KeyEvent

(define (world-after-key-event w kev)
  (cond
    [(key=? kev PAUSE) (world-with-paused-toggled w)]
    [(key=? kev UP)    (key-event-helper w NORTH)]
    [(key=? kev DOWN)  (key-event-helper w SOUTH)]
    [(key=? kev LEFT)  (key-event-helper w WEST)]
    [(key=? kev RIGHT) (key-event-helper w EAST)]
    [else w]))

;; TESTS
(begin-for-test
  (check-equal? 
   (world-after-key-event unpaused-world-at-120 LEFT)
   unpaused-world-at-120-left "Test Failed for world-after-key-event")
  (check-equal? 
   (world-after-key-event unpaused-world-at-120 RIGHT)
   unpaused-world-at-120-right "Test Failed for world-after-key-event")
  (check-equal? 
   (world-after-key-event unpaused-world-at-120 UP)
   unpaused-world-at-120-up "Test Failed for world-after-key-event")
  (check-equal? 
   (world-after-key-event unpaused-world-at-120 DOWN)
   unpaused-world-at-120-down "Test Failed for world-after-key-event"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key-event-helper : World Direction -> World
;; GIVEN: a world w and the direction d
;; RETURNS: the world that should follow the given world after the given key 
;; event.
;; EXAMPLES: (key-event-helper unpaused-world-at-120-left EAST)
;;          => unpaused-world-at-120-right
;; STRATEGY: Structural Decomposition on w : World

(define (key-event-helper w d)
  (make-world 
   (key-event-cat-helper (world-cat1 w) d)
   (key-event-cat-helper (world-cat2 w) d)
   (world-paused? w)))

;; TESTS
;; test follow helper functions
(begin-for-test
 (check-equal? (key-event-helper unpaused-world-at-120-left EAST)
               unpaused-world-at-120-right 
               "Test Failed for key-event-helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key-event-cat-helper : Cat Direction -> Cat
;; GIVEN: a cat and the direction d
;; RETURNS: the cat that should follow the given cat after the given key 
;; event.
;; EXAMPLES: (key-event-cat-helper (make-cat 100 200 true NORTH) NORTH)
;;          => (make-cat 100 200 true NORTH)
;; STRATEGY: Structural Decomposition on c : Cat

(define (key-event-cat-helper c d)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c) true d) c))

;; TESTS
(begin-for-test
  (check-equal? (key-event-cat-helper (make-cat 100 200 true NORTH) NORTH)
                (make-cat 100 200 true NORTH) 
                "Test failed for key-event-cat-helper"))
                              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-paused-toggled : World -> World
;; GIVEN: a world
;; RETURNS: a world just like the given one, but with paused? toggled
;; EXAMPLE: (world-with-paused-toggled paused-world-at-120)
;;          => unpaused-world-at-120
;; STRATEGY: Structural Decomposition on w : World

(define (world-with-paused-toggled w)
  (make-world
   (world-cat1 w)
   (world-cat2 w)
   (not (world-paused? w))))

;; TESTS
;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event.

(begin-for-test
  (check-equal?
    (world-after-key-event paused-world-at-120 pause-key-event)
    unpaused-world-at-120
    "after pause key, a paused world should become unpaused")

  (check-equal?
    (world-after-key-event unpaused-world-at-120 pause-key-event)
    paused-world-at-120
    "after pause key, an unpaused world should become paused")

  (check-equal?
    (world-after-key-event paused-world-at-120 non-pause-key-event)
    paused-world-at-120
    "after a non-pause key, a paused world should be unchanged")

  (check-equal?
    (world-after-key-event unpaused-world-at-120 non-pause-key-event)
    unpaused-world-at-120
    "after a non-pause key, an unpaused world should be unchanged"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; EXAMPLES: 
;; (world-after-mouse-event unpaused-unselected-world-at-120 100 100 BUTTON-UP)
;;   => unpaused-selected-world-at-120
;; STRATEGY: Structural Decomposition on w : World

(define (world-after-mouse-event w mx my mev)
  (make-world
   (cat-after-mouse-event (world-cat1 w) mx my mev)
   (cat-after-mouse-event (world-cat2 w) mx my mev)
   (world-paused? w)))

;; TESTS
(begin-for-test
  (check-equal? 
   (world-after-mouse-event unpaused-unselected-world-at-120 70 70 
            BUTTON-DOWN) unpaused-unselected-world-at-120 "Test Failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-after-mouse-event : Cat Integer Integer MouseEvent -> Cat
;; GIVEN: a cat and a description of a mouse event
;; RETURNS: the cat that should follow the given mouse event
;; EXAMPLES: (world-after-mouse-event 
;;     (make-world unselected-cat1-at-120 unselected-cat2-at-135 false)
;;      (+ CAT1-X-COORD 5) 150 "button-down")
;;   => (make-world selected-cat1-at-120 unselected-cat2-at-135 false)
;; STRATEGY: Cases on mev : MouseEvent

(define (cat-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (cat-after-button-down c mx my)]
    [(mouse=? mev DRAG)        (cat-after-drag c mx my)]
    [(mouse=? mev BUTTON-UP)   (cat-after-button-up c mx my)]
    [else c]))

;; TESTS

(begin-for-test
  ;; button-down:
  ;; button-down inside cat1
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-120
        unselected-cat2-at-135
        false)
      (+ CAT1-X-COORD 5) 150    ;; a coordinate inside cat1
      "button-down")
    (make-world
      selected-cat1-at-120
      unselected-cat2-at-135
      false)
    "button down inside cat1 should select it but didn't")

  ;; button-down inside cat2
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-120
        unselected-cat2-at-135
        false)
      (+ CAT2-X-COORD 5) 150    ;; a coordinate inside cat2
      "button-down")
    (make-world
      unselected-cat1-at-120
      selected-cat2-at-135
      false)
    "button down inside cat2 should select it but didn't")

  ;; button-down not inside any cat
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-120
        unselected-cat2-at-135
        false)
      (+ CAT1-X-COORD 5) 15    ;; a coordinate not inside cat1 or cat2
      "button-down")
    (make-world
      unselected-cat1-at-120
      unselected-cat2-at-135
      false)
    "button down outside any cat should leave world unchanged, but didn't")

  ;; tests for drag
  ;; don't care about paused, care only about which cat is selected. 

  ;; no cats selected: drag should not change anything
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-120
        unselected-cat2-at-135
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
        unselected-cat1-at-120
        unselected-cat2-at-135
        false)
    "drag with no cat selected didn't leave world unchanged")
    
  ;; cat1 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        selected-cat1-at-120
        unselected-cat2-at-135
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
      (make-cat (+ CAT1-X-COORD 100) 15 true SOUTH)
      unselected-cat2-at-135
      false)
    "drag when cat1 is selected should just move cat1, but didn't")

  ;; cat2 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat2-at-135
        selected-cat1-at-120
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
      unselected-cat2-at-135
      (make-cat (+ CAT1-X-COORD 100) 15 true SOUTH)
      false)
    "drag when cat2 is selected should just move cat2, but didn't")


  ;; tests for button-up
  ;; button-up always unselects both cats

  ;; unselect cat1
  (check-equal?
    (world-after-mouse-event
      (make-world
        selected-cat2-at-135
        unselected-cat1-at-120
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat2-at-135
        unselected-cat1-at-120
        true)
    "button-up failed to unselect cat")

  ;; unselect cat2
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-120
        selected-cat2-at-135
        true)
      (+ CAT1-X-COORD 100) 150    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat1-at-120
        unselected-cat2-at-135
        true)
    "button-up failed to unselect cat2")
  
  ;; unselect cat2
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-120
        unselected-cat2-at-135
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat1-at-120
        unselected-cat2-at-135
        true)
    "button-up with two unselected cats failed.")

  ;; tests for other mouse events

  (check-equal?
    (world-after-mouse-event unpaused-world-at-120 
      (+ CAT1-X-COORD 100) 15    ;; arbitrary coordinate
      "move")
    unpaused-world-at-120
    "other mouse events should leave the world unchanged, but didn't")

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-after-button-down : Cat Integer Integer -> Cat
;; GIVEN: a cat, x and y coordinates of mouse in pixels
;; RETURNS: the cat following a button-down at the given location.
;; EXAMPLES: (cat-after-button-down (make-cat 200 200 false NORTH) 210 210)
;;    => (make-cat 200 200 true NORTH)
;; STRATEGY: Structural Decomposition on c : Cat

(define (cat-after-button-down c x y)
  (if (in-cat? c x y)
      (make-cat (cat-x-pos c) (cat-y-pos c) true (cat-direction c))
      c))

;; TESTS
(begin-for-test
  (check-equal? (cat-after-button-down (make-cat 200 200 false NORTH) 210 210)
                (make-cat 200 200 true NORTH) 
                "Test Failed for cat-after-button-down"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-after-drag : Cat Integer Integer -> Cat
;; GIVEN: a cat, x and y coordinates of mouse in pixels
;; RETURNS: the cat following a drag at the given location
;; EXAMPLES: (cat-after-drag (make-cat 100 100 true NORTH) 110 110)
;;           => (make-cat 110 110 true NORTH)
;; STRATEGY:  Structural Decomposition on c : Cat

(define (cat-after-drag c x y)
  (if (cat-selected? c)
      (make-cat x y true (cat-direction c))
      c))

;; TESTS
(begin-for-test
  (check-equal? (cat-after-drag (make-cat 100 100 true NORTH) 110 110)
                (make-cat 110 110 true NORTH) 
                "Test Failed for cat-after-drag"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-after-button-up : Cat Integer Integer -> Cat
;; GIVEN: a cat and x,y coordinates of the mouse
;; RETURNS: the cat following a button-up at the given location
;; EXAMPLES: (cat-after-button-up (make-cat 100 100 true NORTH) 100 100)
;;          => (make-cat 100 100 false NORTH)
;; STRATEGY: Structural Decomposition on c : Cat

(define (cat-after-button-up c x y)
  (if (cat-selected? c) 
      (cat-after-button-up-helper c) 
      c))

;; TESTS
(begin-for-test
  (check-equal? (cat-after-button-up (make-cat 100 100 true NORTH) 100 100)
                (make-cat 100 100 false NORTH) 
                "Test Failed for cat-after-button-up"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-after-button-up-helper : Cat-> Cat
;; GIVEN: a cat c
;; RETURNS: the cat that should follow the given mouse event
;; EXAMPLES: (cat-after-button-up-helper (make-cat 500 200 false EAST))
;;         => (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH) 200 false WEST)
;; STRATEGY: Function Composition

(define (cat-after-button-up-helper c)
  (cond
    [(beyond-east c)  (beyond-east-new-cat c)]
    [(beyond-west c)  (beyond-west-new-cat c)]
    [(beyond-south c) (beyond-south-new-cat c)]
    [(beyond-north c) (beyond-north-new-cat c)]
    [else (cat-after-button-up-other c)]))

;; TESTS
(begin-for-test
  (check-equal? (cat-after-button-up-helper (make-cat 500 200 false EAST))
                (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH BUFFER) 
                          200 false WEST)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; beyond-east  : Cat-> Boolean
;; beyond-west  : Cat-> Boolean
;; beyond-north : Cat-> Boolean
;; beyond-south : Cat-> Boolean

;; GIVEN: a cat c
;; RETURNS: true if the cat position satisfies the condition
;; EXAMPLES: (beyond-east (make-cat 300 200 false EAST)) => true
;; STRATEGY: Structural Decomposition on c : Cat

(define (beyond-east c)
  (> (+ (cat-x-pos c) HALF-CAT-WIDTH) CANVAS-WIDTH))

(define (beyond-west c)
  (< (- (cat-x-pos c) HALF-CAT-WIDTH) ZERO))

(define (beyond-south c)
  (> (+ (cat-y-pos c) HALF-CAT-HEIGHT) CANVAS-HEIGHT))

(define (beyond-north c)
  (< (- (cat-y-pos c) HALF-CAT-HEIGHT) ZERO))

(define (cat-after-button-up-other c)
  (make-cat (cat-x-pos c) (cat-y-pos c) false (cat-direction c)))

;; TESTS
(begin-for-test
  (check-equal? (beyond-east (make-cat 500 200 true NORTH))
                true "Test Failed for beyond-east")
  (check-equal? (beyond-west (make-cat -1 200 true NORTH))
                true "Test Failed for beyond-west")
  (check-equal? (beyond-south (make-cat 200 600 true NORTH))
                true "Test Failed for beyond-south")
  (check-equal? (beyond-north (make-cat 200 -1 true NORTH))
                true "Test Failed for beyond-north"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; beyond-east-new-cat  : Cat-> Cat
;; beyond-west-new-cat  : Cat-> Cat
;; beyond-south-new-cat : Cat-> Cat
;; beyond-north-new-cat : Cat-> Cat

;; GIVEN: a cat c
;; RETURNS: the cat that look like the given one, but moved by the
;; specified number of pixels distance in required direction 
;; EXAMPLES: (beyond-east-new-cat (make-cat 500 200 false EAST))
;;         => (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH) 200 false EAST)
;; STRATEGY: Structural Decomposition on c : Cat

(define (beyond-east-new-cat c)
  (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH BUFFER) 
            (cat-y-pos c) false (direction-helper (cat-direction c))))

(define (beyond-west-new-cat c)
  (make-cat (+ HALF-CAT-WIDTH BUFFER) (cat-y-pos c) false 
            (direction-helper (cat-direction c))))

(define (beyond-south-new-cat c)
  (make-cat (cat-x-pos c)
            (- CANVAS-HEIGHT HALF-CAT-HEIGHT BUFFER) 
            false 
            (direction-helper (cat-direction c))))

(define (beyond-north-new-cat c)
  (make-cat (cat-x-pos c) (+ HALF-CAT-HEIGHT BUFFER) false 
            (direction-helper (cat-direction c))))

;;TESTS

(begin-for-test
  (check-equal? (cat-after-button-up-helper (make-cat 500 200 false EAST))
                (make-cat (- CANVAS-WIDTH HALF-CAT-WIDTH BUFFER) 
                          200 false WEST))
  (check-equal? (cat-after-button-up-helper (make-cat 0 200 false WEST))
                (make-cat (+ HALF-CAT-WIDTH BUFFER) 200 false EAST))
  (check-equal? (cat-after-button-up-helper (make-cat 200 0 false NORTH))
                (make-cat 200(+ HALF-CAT-HEIGHT BUFFER) false SOUTH))
  (check-equal? (cat-after-button-up-helper (make-cat 200 600 false SOUTH))
                (make-cat 200 (- CANVAS-HEIGHT HALF-CAT-HEIGHT BUFFER) 
                          false NORTH)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Had to write this function, Based on note PS02 clarifications / edge cases.
;; The video and discussion on piazza is ambigious. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; direction-helper : Direction -> Direction
;; GIVEN: direction of the cat
;; RETURNS: returns a new direction
;; EXAMPLES: (direction-helper NORTH)) => SOUTH
;; STRATEGY: Structural Decomposition on direction : Direction

(define (direction-helper d)
  (cond
    [(string=? d NORTH) SOUTH]
    [(string=? d SOUTH) NORTH]
    [(string=? d EAST)  WEST]
    [(string=? d WEST)  EAST]))

;; TESTS
(begin-for-test
  (check-equal? (direction-helper NORTH) SOUTH
                "Test Failed for direction-helper")
  (check-equal? (direction-helper SOUTH) NORTH
                "Test Failed for direction-helper")
  (check-equal? (direction-helper EAST) WEST
                "Test Failed for direction-helper")
  (check-equal? (direction-helper WEST) EAST
                "Test Failed for direction-helper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-cat? : Cat Integer Integer -> Boolean
;; GIVEN: a cat, x and y coordinates of mouse
;; RETURNS: true iff the given coordinate is inside the bounding box of
;; the given cat.
;; EXAMPLES: (in-cat? (make-cat 100 200 true NORTH)) => true
;; STRATEGY: Structural Decomposition on c : Cat

(define (in-cat? c x y)
  (and 
   (<= (- (cat-x-pos c) HALF-CAT-WIDTH) x (+ (cat-x-pos c) HALF-CAT-WIDTH))
   (<= (- (cat-y-pos c) HALF-CAT-HEIGHT) y (+ (cat-y-pos c) HALF-CAT-HEIGHT))))

;; TESTS
(begin-for-test
  ;; inside cat
  (check-equal?
   (in-cat? unselected-cat1-at-120 (+ CAT1-X-COORD 5) 70) 
    true "test of in-cat? with nearby point")
  ;; a coordinate not inside the cat
  (check-equal?
    (in-cat? unselected-cat1-at-120 (+ CAT1-X-COORD 100) 15)
    false "test of in-cat? with distant point")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cat-north? : Cat -> Boolean
;; cat-south? : Cat -> Boolean
;; cat-east?  : Cat -> Boolean
;; cat-west?  : Cat -> Boolean

;; GIVEN: a cat
;; RETURNS: true iff the given cat is in that direction
;; EXAMPLES: 
;;  (cat-north? (make-cat 200 200 false NORTH)) => true
;;  (cat-east?  (make-cat 200 200 false EAST))  => true
;; STRATEGY: Structural Decomposition on c : Cat

(define (cat-north? c)
  (string=? (cat-direction c) NORTH))

(define (cat-south? c)
  (string=? (cat-direction c) SOUTH))

(define (cat-east? c)
  (string=? (cat-direction c) EAST))

(define (cat-west? c)
  (string=? (cat-direction c) WEST))

;; TESTS
(begin-for-test
  (check-equal? (cat-north? (make-cat 100 200 false NORTH))
                true "Test Failed for cat-north?")
  (check-equal? (cat-south? (make-cat 100 200 false SOUTH))
                true "Test Failed for cat-north?")
  (check-equal? (cat-east? (make-cat 100 200 false  EAST))
                true "Test Failed for cat-north?")
  (check-equal? (cat-west? (make-cat 100 200 false  WEST))
                true "Test Failed for cat-north?"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
