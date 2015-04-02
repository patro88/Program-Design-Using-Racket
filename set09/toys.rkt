;--------------------------------------------------------------------------;
;                            FILE NAME : toys.rkt                          ;
;--------------------------------------------------------------------------;

#lang racket

; require
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

; provide
(provide World%)
(provide SquareToy%)
(provide CircleToy%)
(provide make-world)
(provide run) 
(provide make-square-toy)
(provide make-circle-toy)
(provide World<%>)
(provide Toy<%>)

;--------------------------------------------------------------------------;
;                            CONSTANTS                                     ;
;--------------------------------------------------------------------------;

; canvas properties
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

; target properties
(define TARGET-RADIUS 10)
(define TARGET-COLOR "blue")

; circle toy properties
(define CIRCLE-RADIUS 5)
(define SOLID "solid")
(define RED-COLOR "red")
(define GREEN-COLOR "green")

; square toy properties
(define SQUARE-LENGTH 40)
(define HALF-SQUARE-LENGTH 20)
(define OUTLINE "outline")

; key properties
(define KEY-S "s")
(define KEY-C "c")
(define OTHER-KEY-EVENT "q")

; mouse properties
(define BUTTON-UP "button-up")
(define BUTTON-DRAG "drag")
(define BUTTON-DOWN "button-down")
(define OTHER-MOUSE-EVENT "enter")

; direction properties
(define RIGHT "right")
(define LEFT "left")

; constants
(define ZERO 0)
(define FOUR-TICK 4)


;-----------------------------------------------------------------------------;
;                               DATA DEFINITIONS                              ;
;-----------------------------------------------------------------------------;

; A Direction is one of
;  - RIGHT  (interp: square-toy travels in the right direction)
;  - LEFT   (interp: square-toy travels in the left direction)

; TEMPLATE:
; direction-fn : Direction -> ??
; (define (direction-fn d)
;  (cond
;    [(string=? d RIGHT)...]
;    [(string=? d LEFT)...]))

; EXAMPLES
; LEFT
; RIGHT
;-----------------------------------------------------------------------------;
; A ColorString is one of -
; -- GREEN-COLOR (interp: represents the color of a toy as green)
; -- RED-COLOR   (interp: represents the color of a toy as red)

; TEMPLATE:
; clr-str-fn : ColorString -> ??
; (define (clr-str-fn cs)
;  (cond
;    [(string=? cs GREEN-COLOR)...]
;    [(string=? cs RED-COLOR)...]))

; EXAMPLES
; GREEN-COLOR
; RED-COLOR
;-----------------------------------------------------------------------------;
; A ListOfToy<%> is one of -
; - empty                                  (interp: empty list of toys)
; - (cons Toy<%> ListOfToy<%>)             (interp: non-empty list of toys) 

; TEMPLATE:
; lot-fn : ListOfToy<%> -> ??
;(define (lot-fn lot)
;  (cond
;    [(empty? lot) ...]
;    [else (... (first lot)
;               (lot-fn (rest lot)))]))

; EXAMPLE
; empty

;-----------------------------------------------------------------------------;
;                                 INTERFACES                                  ;
;-----------------------------------------------------------------------------;

; A World<%> interface has the folowing methods -
; on-tick, on-mouse, on-key, target-x, target-y, target-selected?, get-toys.
; Any class that implements the world interface should provide definitions of
; all the above methods.

(define World<%>
  (interface ()
    
    ; -> World<%>
    ; GIVEN: no arguments
    ; RETURNS: the World<%> that should follow this one after a tick
    on-tick                             
    
    ; Integer Integer MouseEvent -> World<%>
    ; GIVEN: the x, y coordinates of the mouse and the mouse event
    ; RETURNS: the World<%> that should follow this one after the given 
    ; mouse event
    on-mouse
    
    ; KeyEvent -> World<%>
    ; GIVEN: a key event
    ; RETURNS: the World<%> that should follow this one after the given 
    ; key event
    on-key
    
    ; -> Scene
    ; GIVEN: no arguments
    ; RETURNS: a scene depicting this world on it.
    on-draw
    
    ; -> Integer
    ; GIVEN: no arguments
    ; RETURN: the x coordinate of the target of this world
    target-x
    
    ; -> Integer
    ; GIVEN: no arguments
    ; RETURN: the y coordinate of the target of this world
    target-y
    
    ; -> Boolean
    ; GIVEN: no arguments
    ; RETURNS: the true is the target of this world is selected else false
    target-selected?
    
    ; -> ListOfToy<%>
    ; GIVEN: no arguments
    ; RETURNS: the list of toys of this world
    get-toys
    
    ))

;-----------------------------------------------------------------------------;

; The Toy<%> interface has the following methods -
; on-tick, add-to-scene, toy-x, toy-y and toy-color. 
; Any class that implements the Toy<%> should provide definitions for 
; all the above methods.

(define Toy<%> 
  (interface ()
    
    ; -> Toy<%>
    ; GIVEN: no arguments
    ; RETURNS: the Toy that should follow this one after a tick
    on-tick                             
    
    ; Scene -> Scene
    ; GIVEN: a Scene
    ; RETURNS: a Scene like the given one, but with this toy drawn
    ; on it.
    add-to-scene
    
    ; -> Int
    ; GIVEN: no arguments
    ; RETURNS: the current x co-ordinate this toy
    toy-x
    
    ; -> Int
    ; GIVEN: no arguments
    ; RETURNS: the current y co-ordinate this toy
    toy-y
    
    ; -> ColorString
    ; GIVEN: no arguments
    ; RETURNS: the current color of this toy
    toy-color
    
    ))

;-----------------------------------------------------------------------------;
;                                CLASSES                                      ;
;-----------------------------------------------------------------------------;

; World% -- a class that satisfies the World<%> interface.
; A World is a (new World% [toy-speed PosInt] 
;                          [x Integer]
;                          [y Integer]
;                          [mouse-x Integer] 
;                          [moust-y Integer] 
;                          [selected? Boolean] 
;                          [toys ListOfToy<%>]) 

; INTERPRETATION:
; represents a world that contains the following-
; toy-speed: represents the speed of the world
; x: is the x coordinate of the center of the target present in the world
; y: is the y coordinate of the center of the target present in the world
; mouse-x: represents the x coordinate of the mouse
; mouse-y: represents the y coordinate of the mouse
; selected?: represents whether the target of the world is selected or not.
; toys: represents the list of toys present in the world

(define World%
  (class* object% (World<%>)
    (init-field toy-speed              ; speed of the world
                [x HALF-CANVAS-WIDTH]  ; x coordinate of the target
                [y HALF-CANVAS-HEIGHT] ; y coordinate of the target
                [mouse-x ZERO]         ; x coordinate of the mouse
                [mouse-y ZERO]         ; y coordinate of the mouse
                [selected? false])     ; true iff the target is selected  
    
    
    (init-field [toys empty])          ; list of toys
    
    (field [TGT-IMG                    ; image for displaying the target
            (circle TARGET-RADIUS OUTLINE TARGET-COLOR)])
    
    (super-new)
    ;-------------------------------------------------------------------------;
    
    ; on-draw: -> Scene
    ; GIVEN: no arguments
    ; RETURNS: a scene as the given one, but with this world painted on it
    ; EXAMPLE: Image of the target on the centre of EMPTY-CANVAS
    
    (define/public (on-draw)
      (local
        ; first add the target to the scene
        ((define scene-with-target (place-image TGT-IMG x y EMPTY-CANVAS)))
        ; then tell each toy to add itself to the scene
        (foldr
         ; Toy<%> Scene -> Scene
         ; GIVEN: a toy and a scene
         ; RETURNS: a image of the given toy on top of the given scene
         (lambda (toy scene) (send toy add-to-scene scene))
         scene-with-target
         toys)))    
    ;-------------------------------------------------------------------------;
    
    ; on-mouse: Integer Integer MouseEvent -> World%
    ; GIVEN: x, y coordinates of the mouse and the mouse event
    ; RETURNS: a world that should follow this world after given mouse event.
    ; EXAMPLE: 1. a world in which the target is not selected will be selected
    ;             after the button-down event if its inside the target
    ;          2. a world in which the target moves relatively with the mouse 
    ;             location if the target is selected
    ;          3. a world in which the target is selected will be unselected
    ;             after the button-up event
    ; STRATEGY: Cases on evn: MouseEvent
    
    (define/public (on-mouse mx my evn)
      (cond
        [(mouse=? evn BUTTON-DOWN) (target-after-button-down mx my)]
        [(mouse=? evn BUTTON-DRAG) (target-after-drag mx my)]
        [(mouse=? evn BUTTON-UP)   (target-after-button-up)]   
        [else this])) 
    ;-------------------------------------------------------------------------;
    
    ; target-after-button-down: Integer Integer -> World%
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; RETURNS: a world that should follow this world after a button down at the
    ; given location of the mouse.
    ; DETAILS: If the event is inside the target, then it returns the world 
    ; just like this world, except that the target is selected. Otherwise 
    ; returns the world unchanged.
    ; EXAMPLE: a world in which the target is not selected will be selected on
    ; the button-down event
    
    (define/public (target-after-button-down mx my)
      (if (in-target? mx my)
          (new World% [toy-speed toy-speed] [x x][y y] [mouse-x mx] 
               [mouse-y my] [selected? true] [toys toys])
          this)) 
    ;-------------------------------------------------------------------------;
    
    ; in-target?: Integer Integer -> Boolean
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; RETURNS: true iff the mouse coordinates is inside the target
    ; EXAMPLE: true when mouse coordinates are (100, 200) and the target center
    ; is at (99, 203) as the mouse is within the area of the target 
    ; (circle radius 10) else returns false if the mouse coordinates are not 
    ; within the target area
    
    (define/public (in-target? mx my)
      (>= TARGET-RADIUS
          (sqrt (+ (sqr(- mx x))
                   (sqr(- my y))))))
    ;-------------------------------------------------------------------------;
    
    ; target-after-button-up: -> World%
    ; GIVEN: no arguments
    ; RETURNS: the world that should follow this world after the button-up
    ; DETAILS: button-up unselects the target
    ; EXAMPLE: a world in which the target is selected will be unselected on 
    ; the button-up event
    
    (define/public (target-after-button-up)
      (new World% [toy-speed toy-speed] [x x] [y y] [mouse-x ZERO]
           [mouse-y ZERO] [selected? false] [toys toys]))
    ;-------------------------------------------------------------------------;
    
    ; target-after-drag: Integer Integer -> World%
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; RETURNS: the world that should follow this world after a drag at the 
    ; given location
    ; DETAILS: if the target is selected, move the target to the mouse location
    ; otherwise ignore
    ; EXAMPLE:  a world in which the target moves relatively with the mouse 
    ;           location if the target is selected
    
    (define/public (target-after-drag mx my)
      (if selected?
          (new World% [toy-speed toy-speed] [x (+ x (- mx mouse-x))]
               [y (+ y (- my mouse-y))] [mouse-x mx] [mouse-y my] 
               [selected? selected?] [toys toys])
          this))
    ;-------------------------------------------------------------------------;
    
    ; on-tick: -> World%
    ; GIVEN: no arguments
    ; RETURNS: A world like this one, but as it should be after a tick
    ; EXAMPLE: 
    ; 1. a world in which the target is at (100,200) and it will remain
    ;    at (100,200) after the tick. 
    ; 2. A world in which a square-toy is at location (70,100) and is moving 
    ;    right will be at location (80,100) and will be moving in right 
    ;    direction considering the world speed to be 10
    
    (define/public (on-tick)
      (new World% [toy-speed toy-speed] [x x] [y y] [mouse-x mouse-x] 
           [mouse-y mouse-y] [selected? selected?]
           [toys (map
                  ; Toy<%> -> Toy<%>
                  ; GIVEN: a toy
                  ; RETURNS: the toy that should follow the given toy after
                  ; a tick 
                  (lambda (toy) (send toy on-tick))
                  toys)]))
    ;-------------------------------------------------------------------------;
    
    ; on-key: KeyEvent -> World%
    ; GIVEN: a key event
    ; RETURNS: a world like this world, but as it should be after the given key
    ; event.
    ; DETAILS: on pressing "s" a new square-toy should be added to this world.
    ;          on pressing "c" a new circle-toy should be added to this world.
    ; EXAMPLE: consider a world in which target exists at location (70, 100). 
    ; On pressing the "c" key, a new circle-toy is created in the given world 
    ; at the centre of the target location with the radius CIRCLE-RADIUS.
    ; STRATEGY: Cases on evn : KeyEvent
    
    (define/public (on-key evn)
      (cond
        [(key=? evn KEY-S) (square-creation)]
        [(key=? evn KEY-C) (circle-creation)]
        [else this]))
    ;-------------------------------------------------------------------------;
    
    ; square-creation: -> World%
    ; GIVEN: no arguments
    ; RETURNS: a world like this world, but with a square-toy added to this 
    ; world on target location and should start moving towards right on tick
    ; EXAMPLE: consider a world consists of target at (70, 100). On call of 
    ; the function, a new square-toy is created at (70,100) i.e. center of the
    ; target location with square length of SQUARE-LENGTH. 
    ; It will start moving towards right on every tick and once 
    ; reaches the boundary it bounces back and so on.
    
    (define/public (square-creation)
      (new World% [toy-speed toy-speed] [x x] [y y] [mouse-x mouse-x]
           [mouse-y mouse-y] [selected? selected?] 
           [toys (cons (make-square-toy x y toy-speed) toys)]))
    ;-------------------------------------------------------------------------;
    
    ; circle-creation: -> World%
    ; GIVEN: no arguments
    ; RETURNS: a world like this world, but with a circle-toy added to this 
    ; world at target location.
    ; EXAMPLE: consider a world consists of target at (70,100). On call of the
    ; method, a new circle-toy is created at (70,100) i.e. center of the 
    ; target location with circle radius of CIRCLE-RADIUS.
    
    (define/public (circle-creation)
      (new World% [toy-speed toy-speed] [x x] [y y] [mouse-x mouse-x]
           [mouse-y mouse-y] [selected? selected?] 
           [toys (cons (make-circle-toy x y) toys)]))
    ;-------------------------------------------------------------------------;
    
    ; target-x: -> Integer
    ; target-y: -> Integer
    
    ; GIVEN: no arguments
    ; RETURNS: target-x returns the x coordinate of the target and target-y
    ; returns the y coordinate of the target
    ; EXAMPLE: consider a world with target at location (70,100). On call of
    ; target-x method will return 70. On call of target-y method will return
    ; 100
    
    (define/public (target-x) x)
    (define/public (target-y) y)
    ;-------------------------------------------------------------------------;
    
    ; target-selected?: -> Boolean
    ; GIVEN: no arguments
    ; RETURNS: true if the target is selected else false
    ; EXAMPLE: consider a world with target at location (70,100) and is 
    ; selected. On call of target-selected? will return true
    
    (define/public (target-selected?) selected?)
    ;-------------------------------------------------------------------------;
    
    ; get-toys: -> ListOfToy<%>
    ; GIVEN: no arguments
    ; RETURNS: the list of toys which is present on this world
    ; EXAMPLE: 
    ; 1. Consider a world with target and no toys. On call of this method 
    ;    will return empty.
    ; 2. Coniser a world with target and circle-toy. On call of this method
    ;    will return a list with a single circle-toy.
    
    (define/public (get-toys) toys)
    
    ))
;-----------------------------------------------------------------------------;

; SquareToy% -- a class that satisfies the Toy<%> interface 

; A SquareToy is a (new SquareToy% [x Integer] [y Integer] [toy-speed PosInt] 
;                                  [dir Direction])
; INTERPRETATION: reperesents a square-toy, containing the x,y co-ordinates,
; speed and the direction of the square-toy.

(define SquareToy%
  (class* object% (Toy<%>)
    (init-field 
     x                  ; The x co-ordinate of the center of this square-toy
     y                  ; The y co-ordinate of the center of this square-toy
     toy-speed          ; The speed in which this square-toy is travelling
     [dir RIGHT])       ; The direction in which this square-toy is travelling
    
    (field [SQUARE-IMG                ; image for displaying a square-toy
            (square SQUARE-LENGTH OUTLINE GREEN-COLOR)])
    
    (super-new)
    ;-------------------------------------------------------------------------;
    ; on-tick : -> SquareToy%
    ; GIVEN: no arguments
    ; RETURNS: A square-toy like this one, but as it should be after a tick
    ; EXAMPLES: 1. a square-toy which is at a particular location moves 
    ;              right after the on-tick event.
    ;           2. a square-toy which is at a particular location moves 
    ;              left after the on-tick event.
    ; STRATEGY: Structural Decomposition on Direction
    
    (define/public (on-tick)
      (cond
        [(string=? dir RIGHT) (on-tick-right)]
        [(string=? dir LEFT) (on-tick-left)]))
    ;-------------------------------------------------------------------------;
    ; on-tick-right : -> SquareToy%
    ; GIVEN: no arguments
    ; RETURNS: A square-toy like this one, but as it should be after a tick
    ; EXAMPLE: A square-toy on the right boundary of the canvas will bounce 
    ;          back inside the canvas with its direction changed to left
    
    (define/public (on-tick-right)
      (if (>= (+ x toy-speed HALF-SQUARE-LENGTH) CANVAS-WIDTH)
          (new SquareToy% [x (- CANVAS-WIDTH HALF-SQUARE-LENGTH)] [y y]
               [toy-speed toy-speed] [dir LEFT])
          (on-tick-right-helper)))
    ;-------------------------------------------------------------------------;
    ; on-tick-right-helper : -> SquareToy%
    ; GIVEN: no arguments
    ; RETURNS: A square-toy like this one, but as it should be after a tick
    ; EXAMPLE: 1. A square-toy on the left boundary of the canvas will bounce 
    ;             back inside the canvas.
    ;          2. A square-toy inside the canvas moves right by 8 pixels/tick
    
    (define/public (on-tick-right-helper)
      (if (< x HALF-SQUARE-LENGTH)
          (new SquareToy% [x HALF-SQUARE-LENGTH] [y y] 
               [toy-speed toy-speed] [dir RIGHT])          
          (new SquareToy% [x (+ x toy-speed)] [y y] 
               [toy-speed toy-speed] [dir RIGHT]))) 
    ;-------------------------------------------------------------------------;
    ; on-tick-left : -> SquareToy%
    ; GIVEN: no arguments
    ; RETURNS: A square-toy like this one, but as it should be after a tick
    ; EXAMPLE: 1. A square-toy on the left boundary of the canvas will bounce 
    ;             back inside the canvas with its direction changed to right
    ;          2. A square-toy inside the canvas moves left by 8 pixels/tick
    
    (define/public (on-tick-left)
      (if (<= (- x toy-speed HALF-SQUARE-LENGTH) ZERO)
          (new SquareToy% [x HALF-SQUARE-LENGTH] [y y]
               [toy-speed toy-speed] [dir RIGHT])
          (new SquareToy% [x (- x toy-speed)] [y y] 
               [toy-speed toy-speed] [dir LEFT])))    
    ;-------------------------------------------------------------------------;
    ; add-to-scene : Scene -> Scene
    ; GIVEN: a scene 
    ; RETURNS: a scene like the given one, but with the image of this 
    ; square-toy painted on it.
    ; EXAMPLE: an image of a square-toy on an empty canvas.
    
    (define/public (add-to-scene scene)
      (place-image SQUARE-IMG x y scene))
    ;-------------------------------------------------------------------------;
    ; toy-x : -> Integer
    ; GIVEN: no arguments
    ; RETURNS: the x co-ordinate of the center of this square-toy
    ; EXAMPLE: when a square-toy is at (100,200), the toy-x returns 100
    ;          i.e. the x co-ordinate of the square-toy
    
    (define/public (toy-x) x)
    ;-------------------------------------------------------------------------;
    ; toy-y : -> Integer
    ; GIVEN: no arguments
    ; RETURNS: the y co-ordinate of the center of this square-toy
    ; EXAMPLE: when a square-toy is at (100,200), the toy-y returns 200
    ;          i.e. the y co-ordinate of the square-toy
    
    (define/public (toy-y) y)
    ;-------------------------------------------------------------------------;
    ; toy-color : -> ColorString
    ; GIVEN: no arguments
    ; RETURNS: the color of this square-toy which is always green.
    ; EXAMPLE: GREEN-COLOR
    
    (define/public (toy-color) GREEN-COLOR)
    
    ))
;-----------------------------------------------------------------------------;

; CircleToy% -- a class that satisfies the Toy<%> interface

; A CircleToy% is a (new CircleToy% [x Integer] [y Integer] 
;                                  [tick-count NonNegInt] [color ColorString])
; INTERPRETATION: reperesents a circle-toy, containing the x,y co-ordinates,
; tick-count and the current color of this circle-toy

(define CircleToy%
  (class* object% (Toy<%>)
    (init-field 
     x                    ; The x co-ordinate of the center of this circle-toy
     y                    ; The y co-ordinate of the center of this circle-toy
     [tick-count ZERO] ; The number of ticks wherein this circle-toy has
     ; maintained the same color. The color of this toy
     ; alternates between red and green when tick-count is 4
     [color GREEN-COLOR]) ; The current color of this circle-toy
    
    (field [CIRCLE-IMG          ; image for displaying this circle-toy
            (circle CIRCLE-RADIUS SOLID color)])
    
    (super-new)
    ;-------------------------------------------------------------------------;
    ; on-tick : -> CircleToy%
    ; GIVEN: no arguments
    ; WHERE: the tick-count increases at every tick, and if the tick-count
    ; is greater than or equal to 4 then it is reset to 0 and the color of 
    ; this circle-toy changes from red to green or vice-versa.
    ; RETURNS: a circle-toy like this one, but as it should be after a tick.
    ; EXAMPLE: a circle-toy which is green and with the tick-count as 4 will
    ;          change to red color with tick-count reset to 0 after on-tick   
    
    (define/public (on-tick)
      (if (>= tick-count FOUR-TICK)
          (new CircleToy% [x x] [y y] [tick-count ZERO] [color (color-toggle)])
          (new CircleToy% [x x] [y y] [tick-count (add1 tick-count)]
               [color color])))
    ;-------------------------------------------------------------------------;
    ; color-toggle : -> ColorString
    ; GIVEN: no arguments
    ; RETURNS: red color if the color of this circle-toy is green 
    ; or green color if the color of this circle-toy is red. 
    ; EXAMPLE: when the color is green, color-toggle method returns red color.
    ; STRATEGY: Structural Decomposition on ColorString
    
    (define/public (color-toggle)
      (cond
        [(string=? color GREEN-COLOR) RED-COLOR]
        [(string=? color RED-COLOR) GREEN-COLOR]))
    ;-------------------------------------------------------------------------;
    ; add-to-scene : Scene -> Scene
    ; GIVEN: a scene 
    ; RETURNS: a scene like the given one, but with this circle-toy painted
    ; on it.
    ; EXAMPLE: an image of a circle-toy on an empty canvas.
    
    (define/public (add-to-scene scene)
      (place-image CIRCLE-IMG x y scene))
    ;-------------------------------------------------------------------------;
    ; toy-x : -> Integer
    ; GIVEN: no arguments
    ; RETURNS: the x co-ordinate of the center of this circle-toy
    ; EXAMPLE: when a circle-toy is at (100,200), the toy-x returns 100
    ;          i.e. the x co-ordinate of the circle-toy
    
    (define/public (toy-x) x)
    ;-------------------------------------------------------------------------;
    ; toy-y : -> Integer
    ; GIVEN: no arguments
    ; RETURNS: the y co-ordinate of the center of this circle-toy
    ; EXAMPLE: when a circle-toy is at (100,200), the toy-y returns 200
    ;          i.e. the y co-ordinate of the circle-toy
    
    (define/public (toy-y) y)
    ;-------------------------------------------------------------------------;
    ; toy-color : -> ColorString
    ; GIVEN: no arguments 
    ; RETURNS: the color of this circle-toy 
    ; EXAMPLE: when this circle-toy is red, the toy-color returns red
    
    (define/public (toy-color) color)
    
    ))
;-----------------------------------------------------------------------------;

;make-world : PosInt -> World%
;GIVEN: a speed
;RETURNS: a world with a target, but no toys, and in which any
;toys created in the future will travel at the given speed (in pixels/tick).
;EXAMPLE: (make-world 8) => (new World% [toy-speed 8])
;STRATEGY : Function Composition

(define (make-world speed)
  (new World% [toy-speed speed]))

;-----------------------------------------------------------------------------;

;make-square-toy : PosInt PosInt PosInt -> SquareToy%
;GIVEN: an x and a y position, and a speed
;RETURNS: an object representing a square toy at the given position,
;travelling right at the given speed.
;EXAMPLE: (make-square-toy 100 200 8) => 
;         (new SquareToy% [x 100] [y 200] [toy-speed 8])
;STRATEGY : Function Composition

(define (make-square-toy new-x new-y new-speed)
  (new SquareToy% [x new-x] [y new-y] [toy-speed new-speed]))

;-----------------------------------------------------------------------------;
;make-circle-toy : PosInt PosInt -> CircleToy%
;GIVEN: an x and a y position
;RETURNS: an object represeenting a circle toy at the given position.
;EXAMPLE: (make-circle-toy 200 300) => (new CircleToy% [x 200] [y 300])
;STRATEGY : Function Composition

(define (make-circle-toy new-x new-y)
  (new CircleToy% [x new-x] [y new-y]))

;-----------------------------------------------------------------------------;

;run : PosNum PosInt -> World%
;GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;EFFECT: creates and runs a world
;RETURNS: the final state of the world.
;EXAMPLE: (run 0.25 8) create and runs a world in which the a target is 
;created at the center of the canvas and each square-toy travels at
;8 pixels/tick and eaach tick is 0.25 seconds.

(define (run rate speed)
  (big-bang (make-world speed)
            (on-tick 
             ; World<%> -> World<%>
             ; GIVEN: a world
             ; RETURNS: the world that should follow 
             ; this one after a tick
             (lambda (w) (send w on-tick)) rate)
            (on-draw 
             ; World<%> -> Scene
             ; GIVEN: a world
             ; RETURNS: a scene depicting the given world on it.
             (lambda (w) (send w on-draw)))
            (on-key 
             ; World<%> KeyEvent -> World<%>
             ; GIVEN: a world and a key event
             ; RETURNS: the world that should follow the given 
             ; one after the given key event
             (lambda (w kev) (send w on-key kev)))
            (on-mouse 
             ; World<%> Integer Integer MouseEvent -> World<%>
             ; GIVEN: a world, the x, y coordinates of the mouse 
             ; and the mouse event
             ; RETURNS: the world that should follow given one after
             ; the given mouse event
             (lambda (w x y evt) (send w on-mouse x y evt)))))



;-----------------------------------------------------------------------------;
;                              TEST FUNCTIONS                                 ;
;-----------------------------------------------------------------------------;

;world-equal? : World% World% -> Boolean
;GIVEN: two worlds 
;RETURNS: true iff the target and the list of toys 
;of both of the given worlds match.
;EXAMPLE: (world-equal? (new World% [toy-speed 8]) (new World% [toy-speed 8])
;         => true
;STRATEGY: HOFC

(define (world-equal? w1 w2)
  (and
   (target-equal? w1 w2)
   (andmap
    ; Toy<%> Toy<%> -> Boolean
    ; GIVEN: two toys
    ; RETURNS: true iff the given toys are equal
    (lambda (toy1 toy2) (toy-equal? toy1 toy2))
    (send w1 get-toys)
    (send w2 get-toys))))

; TESTS
(begin-for-test
  (check-true
   (world-equal? (new World% [toy-speed 8]) (new World% [toy-speed 8]))
   "Error in world-equal? function. Should return true as the 2 
    worlds are the same"))

;-------------------------------------------------------------------------;

;target-equal? : World% World% -> Boolean
;GIVEN: two worlds 
;RETURNS: true iff the x,y co-ordinates and selected? 
;of both of the given worlds match.
;EXAMPLE: (target-equal? (new World% [toy-speed 8]) (new World% [toy-speed 8]) 
;         => true
;STRATEGY: Funcion Composition

(define (target-equal? w1 w2)
  (and
   (= (send w1 target-x) (send w2 target-x))
   (= (send w1 target-y) (send w2 target-y))
   (equal? (send w1 target-selected?) (send w2 target-selected?))))

; TESTS
(begin-for-test
  (check-true
   (target-equal? (new World% [toy-speed 8]) (new World% [toy-speed 8]))
   "Error in target-equal? function. Should return true as the 2 
    worlds are the same."))
;-------------------------------------------------------------------------;

;toy-equal? : Toy<%> Toy<%> -> Boolean
;GIVEN: two toys which implement the Toy<%> interface 
;RETURNS: true iff the given toys are equal.
;EXAMPLE: (toy-equal? (make-square-toy 200 250 8) (make-square-toy 200 250 8))
;          => true
;STRATEGY: Funcion Composition

(define (toy-equal? toy1 toy2)
  (and
   (= 
    (send toy1 toy-x)
    (send toy2 toy-x))
   (=
    (send toy1 toy-y)
    (send toy2 toy-y))   
   (string=?      
    (send toy1 toy-color)
    (send toy2 toy-color))))

; TESTS
(begin-for-test
  (check-true
   (toy-equal? (make-square-toy 200 250 8) (make-square-toy 200 250 8))
   "Error in toy-equal? function. Should return true as the 2 
    toys are the same."))
;-------------------------------------------------------------------------;

(begin-for-test
  ; Defining local constants for the tests
  (local
    ((define INIT-SQR (make-square-toy 200 250 8))
     (define INIT-SQR-TICK (send INIT-SQR on-tick))
     (define INIT-CIR (make-circle-toy 200 250))
     (define INIT-CIR-TICK (send INIT-CIR on-tick))
     (define INIT-WORLD (make-world 8))
     (define INIT-WORLD-SQR (send INIT-WORLD on-key KEY-S))
     (define INIT-WORLD-SQR+CIR (send INIT-WORLD-SQR on-key KEY-C))
     (define INIT-WORLD-BTN-DWN (send INIT-WORLD on-mouse 200 250 BUTTON-DOWN))
     (define INIT-WORLD-BTN-UP 
       (send INIT-WORLD-BTN-DWN on-mouse 200 250 BUTTON-UP))
     
     (define SELCTD-WORLD-SQR+CIR 
       (send INIT-WORLD-SQR+CIR on-mouse 200 250 BUTTON-DOWN))
     
     (define DRAGD-WORLD-SQR+CIR 
       (send SELCTD-WORLD-SQR+CIR on-mouse 250 250 BUTTON-DRAG)) 
     
     (define DRAGD-WORLD-SQR+CIR-BTN-UP
       (send DRAGD-WORLD-SQR+CIR on-mouse 250 250 BUTTON-UP))     
     
     (define GRN-CIR-5-TICK 
       (new CircleToy% [x 200] [y 250] [tick-count 5]))
     
     (define RED-CIR (send GRN-CIR-5-TICK on-tick))
     
     (define RED-CIR-5-TICK  
       (new CircleToy% [x 200] [y 250] [tick-count 5] [color RED-COLOR]))
     
     (define WORLD-RED-CIR-5-TICK 
       (new World% [toy-speed 8] [toys (list RED-CIR-5-TICK)]))
     
     (define WORLD-INIT-CIR (send WORLD-RED-CIR-5-TICK on-tick))
     
     (define SQR-LEFT (new SquareToy% [x 200] [y 250] [toy-speed 8][dir LEFT]))
     
     (define SQR-LEFT-AFT-TICK (send SQR-LEFT on-tick))
     
     (define SQR-RGT-BDRY 
       (new SquareToy% [x 372] [y 250] [toy-speed 8][dir RIGHT]))
     
     (define SQR-RGT-BDRY-AFT-TICK (send SQR-RGT-BDRY on-tick))
     
     (define SQR-LEFT-BDRY 
       (new SquareToy% [x 28] [y 250] [toy-speed 8][dir LEFT]))
     
     (define SQR-LEFT-BDRY-AFT-TICK (send SQR-LEFT-BDRY on-tick))
     
     (define NEW-SQR-LEFT-BDRY 
       (new SquareToy% [x 5] [y 250] [toy-speed 8][dir RIGHT]))
     
     (define NEW-SQR-LEFT-BDRY-AFT-TICK (send NEW-SQR-LEFT-BDRY on-tick))
     
     (define IMG-INIT-WORLD-SQR+CIR 
       (place-image
        (circle CIRCLE-RADIUS SOLID GREEN-COLOR) 200 250
        (place-image (square SQUARE-LENGTH OUTLINE GREEN-COLOR) 200 250
                     (place-image (circle TARGET-RADIUS OUTLINE TARGET-COLOR) 
                                  200 250 EMPTY-CANVAS)))))
    ;-------------------------------------------------------------------------;
    ; Testing on-tick method
    (check toy-equal? INIT-SQR-TICK (make-square-toy 208 250 8)
           "Error in on-tick method of square. Should create a square as the 
            given one, but moved to the right by the given speed.")
    (check toy-equal? INIT-CIR-TICK (make-circle-toy 200 250)
           "Error in on-tick method of circle. Should return the given circle")
    (check toy-equal? RED-CIR 
           (new CircleToy% [x 200] [y 250] [color RED-COLOR])
           "Error in on-tick method of circle. Should create a circle as the 
            given one, but with the color toggled.")
    
    (check toy-equal? (send RED-CIR-5-TICK on-tick) INIT-CIR
           "Error in on-tick method of circle. Should create a circle as the 
            given one, but with the color toggled.")
    
    (check toy-equal? SQR-LEFT-AFT-TICK 
           (new SquareToy% [x 192] [y 250] [toy-speed 8] [dir LEFT])
           "Error in on-tick method of square. Should create a square as the 
            given one, but moved to the left by the given speed.")
    
    (check toy-equal? SQR-RGT-BDRY-AFT-TICK 
           (new SquareToy% [x 380] [y 250] [toy-speed 8][dir LEFT])
           "Error in on-tick method of square. Should create a square as the 
            given one, but moved to the right by the given speed and its 
            direction changed to left.")
    
    (check toy-equal? SQR-LEFT-BDRY-AFT-TICK 
           (new SquareToy% [x 20] [y 250] [toy-speed 8][dir RIGHT])
           "Error in on-tick method of square. Should create a square as the 
            given one, but moved to the left by the given speed and its 
            direction changed to right.")
    
    (check toy-equal? NEW-SQR-LEFT-BDRY-AFT-TICK SQR-LEFT-BDRY-AFT-TICK
           "Error in on-tick method of square. Should create a square as the 
            given one, but moved to the left boundary of the canvas")
    ;-------------------------------------------------------------------------;
    ; Testing on-key method
    (check world-equal? WORLD-INIT-CIR
           (new World% [toy-speed 8] [toys (list INIT-CIR)])
           "Error in on-key method of world. Should create a world as the 
            given one, but with a green circle toy added to the list of toys.")
    
    (check world-equal? INIT-WORLD-SQR+CIR  
           (new World% [toy-speed 8] [toys (list INIT-CIR INIT-SQR)])
           "Error in on-key method of world. Should create a world as the 
            given one, but with a square toy added to the list of toys.")
    
    (check world-equal? INIT-WORLD (send INIT-WORLD on-key OTHER-KEY-EVENT)
           "Error in on-key method of world. Should create a world as the
            given one as the given key event is not recognized.")    
    ;-------------------------------------------------------------------------;
    ; Testing on-mouse method
    (check world-equal? INIT-WORLD 
           (send INIT-WORLD on-mouse 200 250 OTHER-MOUSE-EVENT)
           "Error in on-mouse method of world. Should create a world as the
            given one as the given mouse event is not recognized.")
    
    (check world-equal? INIT-WORLD 
           (send INIT-WORLD on-mouse 100 250 BUTTON-DOWN)
           "Error in on-mouse method of world. Should create a world as the
            given one, but with the selected? set to true.")
    
    (check world-equal? INIT-WORLD 
           (send INIT-WORLD on-mouse 200 250 BUTTON-DRAG)
           "Error in on-mouse method of world. Should create a world as the
            given one as selected? is false.")
    
    (check world-equal? INIT-WORLD-BTN-DWN
           (new World% [toy-speed 8] [mouse-x 200] [mouse-y 250]
                [selected? true])
           "Error in on-mouse method of world. Should create a world as the
            given one, but with the selected? set to true.")
    
    (check world-equal? INIT-WORLD-BTN-UP INIT-WORLD
           "Error in on-mouse method of world. Should create a world as the
            given one, but with the selected? set to false.") 
    
    (check world-equal? DRAGD-WORLD-SQR+CIR-BTN-UP
           (new World% [toy-speed 8] [x 250][y 250]
                [toys (list INIT-CIR INIT-SQR)]) 
           "Error in on-mouse method of world. Should create a world as the
            given one, but with the target moved by the mouse and selected?
            set to false.")
    ;-------------------------------------------------------------------------;
    ; Testing on-draw method
    (check-equal?
     (send INIT-WORLD-SQR+CIR on-draw) 
     IMG-INIT-WORLD-SQR+CIR
     "Error in on-draw method of world. Should render the image of the
     given world.")))

;-----------------------------------------------------------------------------;
