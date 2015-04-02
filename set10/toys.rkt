;--------------------------------------------------------------------------;
;                            FILE NAME : toys.rkt                          ;
;--------------------------------------------------------------------------;

#lang racket

; require
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

; run with (run 0.25 8)

; provide
(provide World%)
(provide SquareToy%)
(provide CircleToy%)
(provide make-world)
(provide run) 
(provide make-square-toy)
(provide make-circle-toy)
(provide StatefulWorld<%>)
(provide StatefulToy<%>)

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
(define TARGET-COLOR "black")

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
; A ListOfStatefulToy<%> is one of -
; - empty                                      
;   (interp: empty list of StatefulToy<%>)
; - (cons StatefulToy<%> ListOfStatefulToy<%>) 
;   (interp: non-empty list of StatefulToy<%>) 

; TEMPLATE:
; lost-fn : ListOfStatefulToy<%> -> ??
;(define (lost-fn lot)
;  (cond
;    [(empty? lost) ...]
;    [else (... (first lost)
;               (lost-fn (rest lost)))]))

; EXAMPLE
; empty

;-----------------------------------------------------------------------------;
;                                 INTERFACES                                  ;
;-----------------------------------------------------------------------------;

; A StatefulWorld<%> interface has the folowing methods -
; on-tick, on-mouse, on-key, target-x, target-y, target-selected?, get-toys.
; Any class that implements the StatefulWorld<%> interface should provide 
; definitions of all the above methods.

(define StatefulWorld<%>
  (interface ()
    
    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this StatefulWorld<%> to the state that it should 
    ; be in after a tick.
    on-tick                             
    
    ; Integer Integer MouseEvent -> Void
    ; GIVEN: the x, y coordinates of the mouse and the mouse event
    ; EFFECT: updates this StatefulWorld<%> to the state that it should be in
    ; after the given MouseEvent
    on-mouse
    
    ; KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this StatefulWorld<%> to the state that it
    ; should be in
    ; after the given KeyEvent
    on-key
    
    ; -> Scene
    ; GIVEN: no arguments
    ; RETURNS:  a Scene depicting this StatefulWorld<%> on it
    on-draw 
    
    ; -> Integer
    ; GIVEN: no arguments
    ; RETURN: the x and y coordinates of the target
    target-x
    target-y
    
    ; -> Boolean
    ; GIVEN: no arguments
    ; RETURN: the true is the target of this world is selected else false
    target-selected?
    
    ; -> ListOfStatefulToy<%>
    ; GIVEN: no arguments
    ; RETURNS: the list of StatefulToy<%> of this StatefulWorld<%>
    get-toys
    
    ))

;-----------------------------------------------------------------------------;

; The StatefulToy<%> interface has the following methods -
; on-tick, add-to-scene, toy-x, toy-y and toy-color. 
; Any class that implements the StatefulToy<%> should provide definitions for 
; all the above methods.

(define StatefulToy<%> 
  (interface ()
    
    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this toy StatefulToy<%> to the state it should be in 
    ; after a tick. 
    on-tick                             
    
    ; Scene -> Scene
    ; GIVEN: a Scene
    ; RETURNS: a Scene like the given one, but with this toy StatefulToy<%> 
    ; drawn on it.
    add-to-scene
    
    ; -> Int
    ; GIVEN: no arguments
    ; RETURNS: the current x co-ordinate this StatefulToy<%>
    toy-x
    toy-y
    
    ; -> ColorString
    ; GIVEN: no arguments
    ; RETURNS: the current color of this toy StatefulToy<%>
    toy-color
    
    ))


;-----------------------------------------------------------------------------;
;                                CLASSES                                      ;
;-----------------------------------------------------------------------------;

; World% -- a class that satisfies the StatefulWorld<%> interface.
; A World is a (new World% [toy-speed PosInt] 
;                          [x Integer]
;                          [y Integer]
;                          [mouse-x Integer] 
;                          [moust-y Integer] 
;                          [selected? Boolean] 
;                          [toys ListOfStatefulToy<%>]) 

; INTERPRETATION:
; represents a world that contains the following-
; toy-speed: represents the speed of the world
; x: is the x coordinate of the center of the target present in the world
; y: is the y coordinate of the center of the target present in the world
; mouse-x: represents the x coordinate of the mouse
; mouse-y: represents the y coordinate of the mouse
; selected?: represents whether the target of the world is selected or not.
; toys: represents the list of stateful toys present in the world

(define World%
  (class* object% (StatefulWorld<%>)
    (init-field toy-speed              ; speed of the world
                [x HALF-CANVAS-WIDTH]  ; x coordinate of the target
                [y HALF-CANVAS-HEIGHT] ; y coordinate of the target
                [mouse-x ZERO]         ; x coordinate of the mouse
                [mouse-y ZERO]         ; y coordinate of the mouse
                [selected? false])     ; true iff the target is selected  
    
    
    (init-field [toys empty])          ; list of statefultoys
    
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
         ; StatefulToy<%> Scene -> Scene
         ; GIVEN: a toy and a scene
         ; RETURNS: a image of the given toy on top of the given scene
         (lambda (toy scene) (send toy add-to-scene scene))
         scene-with-target
         toys)))    
    ;-------------------------------------------------------------------------;
    
    ; on-mouse: Integer Integer MouseEvent -> Void
    ; GIVEN: x, y coordinates of the mouse and the mouse event
    ; EFFECT: updates this world that should follow this world after given
    ; mouse event.
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
    
    ; target-after-button-down: Integer Integer -> Void
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; EFFECT: updates this world that should follow this world after a button 
    ; down at the given location of the mouse.
    ; DETAILS: If the event is inside the target, then it returns the world 
    ; just like this world, except that the target is selected. Otherwise 
    ; returns the world unchanged.
    ; EXAMPLE: a world in which the target is not selected will be selected on
    ; the button-down event
    
    (define/public (target-after-button-down mx my)
      (if (in-target? mx my)
          (begin (set! selected? true)
                 (set! mouse-x mx)
                 (set! mouse-y my))
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
    
    ; target-after-button-up: -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this world that should follow this world after the 
    ; button-up
    ; DETAILS: button-up unselects the target
    ; EXAMPLE: a world in which the target is selected will be unselected on 
    ; the button-up event
    
    (define/public (target-after-button-up)
      (begin (set! mouse-x ZERO)
             (set! mouse-y ZERO)
             (set! selected? false)))
    ;-------------------------------------------------------------------------;
    
    ; target-after-drag: Integer Integer -> Void
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; EFFECT: update this world that should follow this world after a drag at 
    ; given location
    ; DETAILS: if the target is selected, move the target to the mouse location
    ; otherwise ignore
    ; EXAMPLE:  a world in which the target moves relatively with the mouse 
    ;           location if the target is selected
    
    (define/public (target-after-drag mx my)
      (if selected?
          (begin (set! x (+ x (- mx mouse-x)))
                 (set! y (+ y (- my mouse-y)))
                 (set! mouse-x mx)
                 (set! mouse-y my))                 
          this))
    ;-------------------------------------------------------------------------;
    
    ; on-tick: -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates the world that should follow this world after a tick
    ; EXAMPLE: 
    ; 1. a world in which the target is at (100,200) and it will remain
    ;    at (100,200) after the tick. 
    ; 2. A world in which a square-toy is at location (70,100) and is moving 
    ;    right will be at location (80,100) and will be moving in right 
    ;    direction considering the world speed to be 10
    
    (define/public (on-tick)
      (for-each
       ; StatefulToy<%> -> Void
       ; GIVEN: a toy
       ; RETURNS: the toy that should follow the given toy after
       ; a tick 
       (lambda (toy) (send toy on-tick))
       toys))
    ;-------------------------------------------------------------------------;
    
    ; on-key: KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this world that shouuld follow this world after the given
    ; key event.
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
    
    ; square-creation: -> Void
    ; GIVEN: no arguments
    ; EFFECT: update this world that should follow this world, but with a 
    ; square-toy added to this world on target location and should start moving 
    ; towards right on tick
    ; EXAMPLE: consider a world consists of target at (70, 100). On call of 
    ; the function, a new square-toy is created at (70,100) i.e. center of the
    ; target location with square length of SQUARE-LENGTH. 
    ; It will start moving towards right on every tick and once 
    ; reaches the boundary it bounces back and so on.
    
    (define/public (square-creation)
      (set! toys (cons (make-square-toy x y toy-speed) toys)))
    ;-------------------------------------------------------------------------;
    
    ; circle-creation: -> Void
    ; GIVEN: no arguments
    ; EFFECT: update this world that should follow this world but with a 
    ; circle-toy added to this world at target location.
    ; EXAMPLE: consider a world consists of target at (70,100). On call of the
    ; method, a new circle-toy is created at (70,100) i.e. center of the 
    ; target location with circle radius of CIRCLE-RADIUS.
    
    (define/public (circle-creation)
      (set! toys (cons (make-circle-toy x y) toys)))
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
    
    ; get-toys: -> ListOfStatefulToy<%>
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

; SquareToy% -- a class that satisfies the StatefulToy<%> interface 

; A SquareToy is a (new SquareToy% [x Integer] [y Integer] [toy-speed PosInt] 
;                                  [dir Direction])
; INTERPRETATION: reperesents a square-toy, containing the x,y co-ordinates,
; speed and the direction of the square-toy.

(define SquareToy%
  (class* object% (StatefulToy<%>)
    (init-field 
     x                  ; The x co-ordinate of the center of this square-toy
     y                  ; The y co-ordinate of the center of this square-toy
     toy-speed          ; The speed in which this square-toy is travelling
     [dir RIGHT])       ; The direction in which this square-toy is travelling
    
    (field [SQUARE-IMG  ; image for displaying a square-toy
            (square SQUARE-LENGTH OUTLINE GREEN-COLOR)])
    
    (super-new)
    ;-------------------------------------------------------------------------;
    
    ; on-tick : -> Void
    ; GIVEN: no arguments
    ; EFFECT: update square-toy as it should be after a tick
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
    
    ; on-tick-right : -> Void
    ; GIVEN: no arguments
    ; EFFECT: update square-toy as it should be after a tick
    ; EXAMPLE: A square-toy on the right boundary of the canvas will bounce 
    ;          back inside the canvas with its direction changed to left
    
    (define/public (on-tick-right)
      (if (>= (+ x toy-speed HALF-SQUARE-LENGTH) CANVAS-WIDTH)
          (begin (set! x (- CANVAS-WIDTH HALF-SQUARE-LENGTH))
                 (set! dir LEFT))
          (on-tick-right-helper)))
    ;-------------------------------------------------------------------------;
    
    ; on-tick-right-helper : -> Void
    ; GIVEN: no arguments
    ; EFFECT: update square-toy as it should be after a tick
    ; EXAMPLE: 1. A square-toy on the left boundary of the canvas will bounce 
    ;             back inside the canvas.
    ;          2. A square-toy inside the canvas moves right by 8 pixels/tick
    
    (define/public (on-tick-right-helper)
      (if (< x HALF-SQUARE-LENGTH)
          (begin (set! x HALF-SQUARE-LENGTH)
                 (set! dir RIGHT))          
          (begin (set! x (+ x toy-speed))
                 (set! dir RIGHT)))) 
    ;-------------------------------------------------------------------------;
    
    ; on-tick-left : -> Void
    ; GIVEN: no arguments
    ; EFFECT: update square-toy as it should be after a tick
    ; EXAMPLE: 1. A square-toy on the left boundary of the canvas will bounce 
    ;             back inside the canvas with its direction changed to right
    ;          2. A square-toy inside the canvas moves left by 8 pixels/tick
    
    (define/public (on-tick-left)
      (if (<= (- x toy-speed HALF-SQUARE-LENGTH) ZERO)
          (begin (set! x HALF-SQUARE-LENGTH)
                 (set! dir RIGHT))
          (begin (set! x (- x toy-speed))
                 (set! dir LEFT))))    
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

; CircleToy% -- a class that satisfies the StatefulToy<%> interface

; A CircleToy% is a (new CircleToy% [x Integer] [y Integer] 
;                                  [tick-count NonNegInt] [color ColorString])
; INTERPRETATION: reperesents a circle-toy, containing the x,y co-ordinates,
; tick-count and the current color of this circle-toy

(define CircleToy%
  (class* object% (StatefulToy<%>)
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
    ; on-tick : -> Void
    ; GIVEN: no arguments
    ; WHERE: the tick-count increases at every tick, and if the tick-count
    ; is greater than or equal to 4 then it is reset to 0 and the color of 
    ; this circle-toy changes from red to green or vice-versa.
    ; EFFECT: update circle-toy as it should be after a tick.
    ; EXAMPLE: a circle-toy which is green and with the tick-count as 4 will
    ;          change to red color with tick-count reset to 0 after on-tick   
    
    (define/public (on-tick)
      (if (>= tick-count FOUR-TICK)
          (begin (set! tick-count ZERO)
                 (set! color (color-toggle)))
          (set! tick-count (add1 tick-count))))
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
      (place-image (circle CIRCLE-RADIUS SOLID color) x y scene))
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

;make-world : PosInt -> StatefulWorld<%>
;GIVEN: a speed
;RETURNS: a world with a target, but no toys, and in which any
;toys created in the future will travel at the given speed (in pixels/tick).
;EXAMPLE: (make-world 8) => (new World% [toy-speed 8])
;STRATEGY : Function Composition

(define (make-world speed)
  (new World% [toy-speed speed]))

; TESTS
; see tests at the end
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

; TESTS
; see tests at the end
;-----------------------------------------------------------------------------;

;make-circle-toy : PosInt PosInt -> CircleToy%
;GIVEN: an x and a y position
;RETURNS: an object represeenting a circle toy at the given position.
;EXAMPLE: (make-circle-toy 200 300) => (new CircleToy% [x 200] [y 300])
;STRATEGY : Function Composition

(define (make-circle-toy new-x new-y)
  (new CircleToy% [x new-x] [y new-y]))

; TESTS
; see tests at the end
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
             ; World -> World
             ; GIVEN: a world
             ; RETURNS: the world that should follow 
             ; this one after a tick
             (lambda (w) (send w on-tick) w) rate)
            (on-draw 
             ; World -> Scene
             ; GIVEN: a world
             ; RETURNS: a scene depicting the given world on it.
             (lambda (w) (send w on-draw)))
            (on-key 
             ; World KeyEvent -> World
             ; GIVEN: a world and a key event
             ; RETURNS: the world that should follow the given 
             ; one after the given key event
             (lambda (w kev) (send w on-key kev) w))
            (on-mouse 
             ; World Integer Integer MouseEvent -> World
             ; GIVEN: a world, the x, y coordinates of the mouse 
             ; and the mouse event
             ; RETURNS: the world that should follow given one after
             ; the given mouse event
             (lambda (w x y evt) (send w on-mouse x y evt) w))))


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
    ; StatefulToy<%> StatefulToy<%> -> Boolean
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

;toy-equal? : StatefulToy<%> StatefulToy<%> -> Boolean
;GIVEN: two toys which implement the StatefulToy<%> interface 
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

; TESTS

; SQUARETOY Constants
(define INIT-SQR (make-square-toy 200 250 8))
(define INIT-SQR-AFTER-TICK
  (new SquareToy% [x 208] [y 250] [toy-speed 8][dir RIGHT]))
(define INIT-SQR-LEFT
  (new SquareToy% [x 200] [y 250] [toy-speed 8][dir LEFT]))
(define INIT-SQR-LEFT-AFTER-TICK
  (new SquareToy% [x 192] [y 250] [toy-speed 8][dir LEFT]))
(define INIT-SQR-X-LESS-THAN-ZERO
  (new SquareToy% [x -10] [y 250] [toy-speed 8][dir LEFT]))
(define INIT-SQR-X-LESS-THAN-ZERO-AFTER-TICK
  (new SquareToy% [x 20] [y 250] [toy-speed 8][dir RIGHT]))
(define INIT-SQR-X-GREATER-THAN-CANVAS
  (new SquareToy% [x 435] [y 250] [toy-speed 8][dir RIGHT]))
(define INIT-SQR-X-GREATER-THAN-CANVAS-AFTER-TICK
  (new SquareToy% [x 380] [y 250] [toy-speed 8][dir LEFT]))
(define INIT-SQR-LEFT-BOUNDARY
  (new SquareToy% [x -10] [y 250] [toy-speed 8][dir RIGHT]))
(define INIT-SQR-LEFT-BOUNDARY-AFTER-TICK
  (new SquareToy% [x 20] [y 250] [toy-speed 8][dir RIGHT]))
(define INIT-SQR-RIGHT-BOUNDARY
  (new SquareToy% [x 7] [y 250] [toy-speed 8][dir LEFT]))
(define INIT-SQR-RIGHT-BOUNDARY-AFTER-TICK
  (new SquareToy% [x 20] [y 250] [toy-speed 8][dir RIGHT]))

; CIRCLETOY Constants
(define INIT-CIR (make-circle-toy 200 250))
(define INIT-CIR-AFTER-TICK 
  (new CircleToy% [x 200] [y 250] [tick-count 0] [color "green"]))
(define INIT-CIR-ON-4-TICK 
  (new CircleToy% [x 200] [y 250] [tick-count 4] [color "green"]))
(define INIT-CIR-ON-5-TICK 
  (new CircleToy% [x 200] [y 250] [tick-count 0] [color "red"]))
(define INIT-CIR-ON-9-TICK 
  (new CircleToy% [x 200] [y 250] [tick-count 4] [color "red"]))
(define INIT-CIR-ON-10-TICK 
  (new CircleToy% [x 200] [y 250] [tick-count 0] [color "green"]))


; WORLD Constants
(define INIT-WORLD (make-world 8))
(define INIT-WORLD-AFTER-TICK (new World% [toy-speed 8]))
(define WORLD-BEFORE-BUTTON-DOWN
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 200] 
       [mouse-y 250] [selected? false] [toys empty]))

(define WORLD-AFTER-BUTTON-DOWN
  (new World%
       [toy-speed 8] [x 200] [y 250] [mouse-x 200]
       [mouse-y 250] [selected? true] [toys empty]))

(define WORLD-BEFORE-BUTTON-UP
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 200] 
       [mouse-y 250] [selected? true] [toys empty]))

(define WORLD-AFTER-BUTTON-UP
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 200] 
       [mouse-y 250] [selected? false] [toys empty]))

(define WORLD-BEFORE-DRAG
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 200] 
       [mouse-y 250] [selected? true] [toys empty]))

(define WORLD-AFTER-DRAG
  (new World% 
       [toy-speed 8] [x 210] [y 250] [mouse-x 210] 
       [mouse-y 250] [selected? true] [toys empty]))

(define WORLD-MOUSE-OUTSIDE-RANGE-BUTTON-DOWN
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 0] 
       [mouse-y 0] [selected? false] [toys empty]))

(define WORLD-DRAG-IF-SELECTED
  (new World% 
       [toy-speed 8] [x 210] [y 250] [mouse-x 210] 
       [mouse-y 250] [selected? false] [toys empty]))

(define INIT-WORLD-CIR
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 0] 
       [mouse-y 0] [selected? false] [toys (list (new CircleToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [tick-count 0] 
                                                      [color "green"]))]))
(define INIT-WORLD-2-CIR
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 0] 
       [mouse-y 0] [selected? false] [toys (list (new CircleToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [tick-count 0] 
                                                      [color "green"])
                                                 (new CircleToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [tick-count 0] 
                                                      [color "green"]))]))

(define INIT-WORLD-2-CIR-1-SQR
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 0] 
       [mouse-y 0] [selected? false] [toys (list (new CircleToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [tick-count 0] 
                                                      [color "green"])
                                                 (new CircleToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [tick-count 0] 
                                                      [color "green"])
                                                 (new SquareToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [toy-speed 8]
                                                      [dir RIGHT]))]))

(define INIT-WORLD-2-CIR-1-SQR-AFTER-TICK
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 0] 
       [mouse-y 0] [selected? false] [toys (list (new CircleToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [tick-count 1] 
                                                      [color "green"])
                                                 (new CircleToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [tick-count 1] 
                                                      [color "green"])
                                                 (new SquareToy% 
                                                      [x 208] 
                                                      [y 250] 
                                                      [toy-speed 8]
                                                      [dir RIGHT]))]))

(define INIT-WORLD-SQR-CIR
  (new World% 
       [toy-speed 8] [x 200] [y 250] [mouse-x 0] 
       [mouse-y 0] [selected? false] [toys (list (new SquareToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [toy-speed 8]
                                                      [dir RIGHT])
                                                 (new CircleToy% 
                                                      [x 200] 
                                                      [y 250] 
                                                      [tick-count 0] 
                                                      [color "green"]))]))

(define IMG-INIT-WORLD-SQR-CIR 
  (place-image
   (circle CIRCLE-RADIUS SOLID GREEN-COLOR) 200 250
   (place-image (square SQUARE-LENGTH OUTLINE GREEN-COLOR) 200 250
                (place-image (circle TARGET-RADIUS OUTLINE TARGET-COLOR) 
                             200 250 EMPTY-CANVAS))))
;------------------------------------------------------------------------------;

(begin-for-test
  
  ; check for square toy moving right after tick 
  (send INIT-SQR on-tick)
  (check-true (toy-equal? INIT-SQR INIT-SQR-AFTER-TICK)
              "check for square toy moving right after tick")  
  ; check for square toy moving left after tick
  (send INIT-SQR-LEFT on-tick)
  (check-true (toy-equal? INIT-SQR-LEFT INIT-SQR-LEFT-AFTER-TICK)
              "check for square toy moving left after tick")
  
  ; check for square toy on left boundary as on next tick should be tangent
  (send INIT-SQR-LEFT-BOUNDARY on-tick)
  (check-true (toy-equal? INIT-SQR-LEFT-BOUNDARY 
                          INIT-SQR-LEFT-BOUNDARY-AFTER-TICK)
              "check for square toy on left boundary as on next tick should 
               be tangent")
  
  ; check for square toy on right boundary as on next tick should be tangent 
  (send INIT-SQR-RIGHT-BOUNDARY on-tick)
  (check-true (toy-equal? INIT-SQR-RIGHT-BOUNDARY 
                          INIT-SQR-RIGHT-BOUNDARY-AFTER-TICK)
              "check for square toy on left boundary as on next tick should 
               be tangent")
  
  ; check for square creation on negative x coordinate
  (send INIT-SQR-X-LESS-THAN-ZERO on-tick)
  (check-true (toy-equal? INIT-SQR-X-LESS-THAN-ZERO
                          INIT-SQR-X-LESS-THAN-ZERO-AFTER-TICK)
              "check for square creation on negative x coordinate")
  
  ; check for square creation on x coordinate greater than canvas width
  (send INIT-SQR-X-GREATER-THAN-CANVAS on-tick)
  (check-true (toy-equal? INIT-SQR-X-GREATER-THAN-CANVAS
                          INIT-SQR-X-GREATER-THAN-CANVAS-AFTER-TICK)
              "check for square creation on x coordinate greater than 
               canvas width")
  
  ; check for circle toy after tick
  (send INIT-CIR on-tick)
  (check-true (toy-equal? INIT-CIR INIT-CIR-AFTER-TICK)
              "check for circle toy after tick")
  
  ; check for circle toy after 4 tick
  (send INIT-CIR-ON-4-TICK on-tick)
  (check-true (toy-equal? INIT-CIR-ON-4-TICK INIT-CIR-ON-5-TICK)
              "check for circle toy after 4 tick")
  
  ; check for circle toy after 9 tick
  (send INIT-CIR-ON-9-TICK on-tick)
  (check-true (toy-equal? INIT-CIR-ON-9-TICK INIT-CIR-ON-10-TICK)
              "check for circle toy after 9 tick")
  
  ; check for world after tick
  (send INIT-WORLD on-tick)
  (check-true (world-equal? INIT-WORLD INIT-WORLD-AFTER-TICK)
              "check for world after tick")
  
  ; check for world before and after button down event
  (send WORLD-BEFORE-BUTTON-DOWN on-mouse 200 250 "button-down")
  (check-true (world-equal? WORLD-BEFORE-BUTTON-DOWN WORLD-AFTER-BUTTON-DOWN)
              "check for world before and after button down event")
  
  ; check for world before and after button up event
  (send WORLD-BEFORE-BUTTON-UP on-mouse 200 250 "button-up")
  (check-true (world-equal? WORLD-BEFORE-BUTTON-UP WORLD-AFTER-BUTTON-UP)
              "check for world before and after button up event")
  
  ; check for other mouse events
  (send WORLD-BEFORE-BUTTON-UP on-mouse 200 250 "enter")
  (check-true (world-equal? WORLD-BEFORE-BUTTON-UP WORLD-BEFORE-BUTTON-UP)
              "check for other mouse events")
  
  ; check for world before and after drag event
  (send WORLD-BEFORE-DRAG on-mouse 210 250 "drag")
  (check-true (world-equal? WORLD-BEFORE-DRAG WORLD-AFTER-DRAG)
              "check for world before and after drag event")
  
  ; check for mouse event outside of range
  (send WORLD-MOUSE-OUTSIDE-RANGE-BUTTON-DOWN on-mouse 100 200 "button-down")
  (check-true (world-equal? WORLD-MOUSE-OUTSIDE-RANGE-BUTTON-DOWN
                            WORLD-MOUSE-OUTSIDE-RANGE-BUTTON-DOWN)
              "check for mouse event outside of range")
  
  ; check for world for drag event
  (send WORLD-DRAG-IF-SELECTED on-mouse 210 250 "drag")
  (check-true (world-equal? WORLD-DRAG-IF-SELECTED WORLD-DRAG-IF-SELECTED)
              "check for world for drag event")
  
  ; check for key event c
  (send INIT-WORLD-CIR on-key KEY-C)
  (check-true (world-equal? INIT-WORLD-CIR INIT-WORLD-2-CIR)
              "check for key event c")
  
  ; check for key event s
  (send INIT-WORLD-2-CIR on-key KEY-S)
  (check-true (world-equal? INIT-WORLD-2-CIR INIT-WORLD-2-CIR-1-SQR)
              "check for key event s")
  
  ; check for other key event
  (send INIT-WORLD-2-CIR on-key "A")
  (check-true (world-equal? INIT-WORLD-2-CIR INIT-WORLD-2-CIR)
              "check for other key event")
  
  ; check for world after tick with 2 circle toy and 1 square toy
  (send INIT-WORLD-2-CIR-1-SQR on-tick)
  (check-true (world-equal? INIT-WORLD-2-CIR-1-SQR 
                            INIT-WORLD-2-CIR-1-SQR-AFTER-TICK)
              "check for world after tick with 2 circle toy and 1 square toy")
  
  ; check for on-draw functionality
  (check-equal? (send INIT-WORLD-SQR-CIR on-draw) IMG-INIT-WORLD-SQR-CIR
                "check for on-draw functionality")
  )
;------------------------------------------------------------------------------;
