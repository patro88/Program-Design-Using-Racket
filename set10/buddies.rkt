;--------------------------------------------------------------------------;
;                            FILE NAME : buddies.rkt                       ;
;--------------------------------------------------------------------------;

#lang racket

; require
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

; run with (run 0.25)

; provide 
(provide World%)
(provide SquareToy%)
(provide make-world)
(provide run)
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
(define BLACK-COLOR "black")
(define ORANGE-COLOR "orange")

; square toy properties
(define SQUARE-LENGTH 30)
(define HALF-SQUARE-LENGTH (/ SQUARE-LENGTH 2))
(define OUTLINE "outline")
(define GREEN-COLOR "green")
(define RED-COLOR "red")

; key properties
(define KEY-S "s")
(define OTHER-KEY-EVENT "q")

; mouse properties
(define BUTTON-UP "button-up")
(define BUTTON-DRAG "drag")
(define BUTTON-DOWN "button-down")
(define OTHER-MOUSE-EVENT "enter")

; constants
(define ZERO 0)

;-----------------------------------------------------------------------------;
;                               DATA DEFINITIONS                              ;
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
;------------------------------------------------------------------------------;

; A ColorString is the color of the toy or the target.


;-----------------------------------------------------------------------------;
;                                 INTERFACES                                  ;
;-----------------------------------------------------------------------------;

; A StatefulWorld<%> interface has the folowing methods -
; on-tick, on-mouse, on-key, target-x, target-y, target-selected?, get-toys,
; on-draw. Any class that implements the StatefulWorld<%> interface should 
; provide definitions of all the above methods.

(define StatefulWorld<%>
  (interface ()
    
    ; -> Void
    ; EFFECT: updates this StatefulWorld<%> to the 
    ;         state that it should be in after a tick.
    on-tick                             
    
    ; Integer Integer MouseEvent -> Void
    ; EFFECT: updates this StatefulWorld<%> to the 
    ;         state that it should be in after the given MouseEvent
    on-mouse
    
    ; KeyEvent -> Void
    ; EFFECT: updates this StatefulWorld<%> to the 
    ;         state that it should be in after the given KeyEvent
    on-key
    
    ; -> Scene
    ; Returns a Scene depicting this StatefulWorld<%> on it.
    on-draw 
    
    ; -> Integer
    ; RETURN: the x and y coordinates of the target
    target-x
    target-y
    
    ; -> Boolean
    ; Is the target selected?
    target-selected?
    
    ; -> ColorString
    ; color of the target
    target-color
    
    ; -> ListOfStatefulToy<%>
    get-toys
    
    ))
;------------------------------------------------------------------------------;

; The StatefulToy<%> interface has the following methods -
; add-to-scene, toy-x, toy-y, on-mouse, toy-color and toy-selected?. 
; Any class that implements the StatefulToy<%> should provide definitions for 
; all the above methods.

(define StatefulToy<%> 
  (interface ()
    
    ; Integer Integer MouseEvent -> Void
    ; GIVEN: the x, y coordinates of the mouse and the mouse event
    ; EFFECT: updates this StatefulToy<%> to the 
    ;         state that it should be in after the given MouseEvent
    on-mouse
    
    ; Scene -> Scene
    ; RETURN: scene like the given one but with this StatefulToy<%> drawn on it
    add-to-scene
    
    ; -> Int
    ; RETURNS: toy-x returns the current x co-ordinate this StatefulToy<%>
    ;          toy-y returns the current y co-ordinate this StatefulToy<%>
    toy-x
    toy-y
    
    ; -> ColorString
    ; RETURN: the current color of this StatefulToy<%>
    toy-color
    
    ; Boolean
    ; RETURN: true if the this StatefulToy<%> is selected else false
    toy-selected?
    
    ))
;------------------------------------------------------------------------------;

(define Subscriber<%>
  (interface ()
    ; String -> Void
    ; EFFECT: updae new color published
    change-color
    
    ; Integer Integer -> Void
    ; EFFECT: update new position published
    position-change
    ))
;------------------------------------------------------------------------------;

(define Publisher<%>
  (interface ()
    ; Subscriber<%> -> Void
    ; EFFECT: adds a new subscriber
    subscribe    
    ))


;-----------------------------------------------------------------------------;
;                                CLASSES                                      ;
;-----------------------------------------------------------------------------;

; World% -- a class that satisfies the StatefulWorld<%> interface.
; A World is a (new World% [x Integer]
;                          [y Integer]
;                          [mouse-x Integer] 
;                          [moust-y Integer] 
;                          [selected? Boolean] 
;                          [color String]
;                          [toys ListOfStatefulToy<%>]) 

; INTERPRETATION:
; represents a world that contains the following-
; toy-speed: represents the speed of the world
; x: is the x coordinate of the center of the target present in the world
; y: is the y coordinate of the center of the target present in the world
; mouse-x: represents the x coordinate of the mouse
; mouse-y: represents the y coordinate of the mouse
; selected?: represents whether the target of the world is selected or not.
; color: represents the color of the target
; toys: represents the list of stateful toys present in the world

(define World%
  (class* object% (StatefulWorld<%>)
    (init-field 
     [x HALF-CANVAS-WIDTH]  ; x coordinate of the target
     [y HALF-CANVAS-HEIGHT] ; y coordinate of the target
     [mouse-x ZERO]         ; x coordinate of the mouse
     [mouse-y ZERO]         ; y coordinate of the mouse
     [selected? false]      ; true iff the target is selected
     [color BLACK-COLOR])  ; color of the target
    (init-field [toys empty]) ; list of statefultoys
    (field [TGT-IMG        ; image for displaying the target
            (circle TARGET-RADIUS OUTLINE color)])
    
    (super-new)
    ;-------------------------------------------------------------------------;
    
    ; on-tick: -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this World to the state that it should be in after a tick
    ; EXAMPLES : (send initial-world on-tick) it update this world after a tick 
    
    (define/public (on-tick)
      this)
    ;-------------------------------------------------------------------------;
    
    ; on-draw: -> Scene
    ; GIVEN: no arguments
    ; RETURNS: a scene as the given one, but with this world painted on it
    ; EXAMPLE: Image of the target on the centre of EMPTY-CANVAS
    ; (send initial-world on-draw) initial-world-with-one-square
    
    (define/public (on-draw)
      (local
        ; add target to the scene
        ((define scene-with-target (place-image 
                                    (circle TARGET-RADIUS OUTLINE color)
                                    x y EMPTY-CANVAS)))
        (foldr
         ; StatefulToy<%> Scene -> Scene
         ; GIVEN: a square toy and a scene
         ; RETURNS: a scene with given square toy painted on it 
         (lambda (toy scene) (send toy add-to-scene scene))
         scene-with-target
         toys)))    
    ;-------------------------------------------------------------------------;
    
    ; on-mouse: Integer Integer MouseEvent -> Void
    ; GIVEN: x, y coordinates of the mouse and the mouse event
    ; EFFECT: updates the world to the state as it should be after the given 
    ; mouse event.
    ; EXAMPLE: (send initial-world on-mouse 200 250 "enter") initial-world
    ; STRATEGY: Cases on evn: MouseEvent
    
    (define/public (on-mouse mx my evn)
      (cond
        [(mouse=? evn BUTTON-DOWN) (begin (target-after-button-down mx my)
                                          (generalise-call-toys mx my evn))]
        [(mouse=? evn BUTTON-DRAG) (begin (target-after-drag mx my)
                                          (generalise-call-toys mx my evn))]
        [(mouse=? evn BUTTON-UP)   (begin (target-after-button-up)
                                          (generalise-call-toys mx my evn))]   
        [else this]))
    ;-------------------------------------------------------------------------;
    
    ; generalise-call-toys: Integer Integer MouseEvent -> Void
    ; GIVEN: x, y coordinates of the mouse and the mouse event
    ; EFFECT: updates the toys of the world after the given mouse event
    ; EXAMPLE: (generalise-call-toys 200 250 "button-down") => updated toys of 
    ; the world after given mouse event 
    
    (define/public (generalise-call-toys mx my evn)
      (for-each
       ; StatefulToy<%> -> Void
       ; GIVEN: a square toy
       ; EFFECT: updated square toy with given mouse event
       (lambda(toy) (send toy on-mouse mx my evn))
       toys))
    ;-------------------------------------------------------------------------;
    
    ; target-after-button-down : Integer Integer MouseEvent -> Void
    ; GIVEN : x,y co-ordinates of the mouse and the mouse event
    ; EFFECT: updates the world like target or square toy or both after the 
    ; given button down mouse event if the mouse coordinates is on the target 
    ; or square toy or both else no effect.
    ; EXAMPLE: a world in which the target is not selected will be selected on
    ; the button-down event
    
    (define/public (target-after-button-down mx my)
      (if (in-target? mx my)
          (begin (set! selected? true)
                 (set! mouse-x mx)
                 (set! mouse-y my)
                 (set! color ORANGE-COLOR))
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
    ; EFFECT: update this world so that the selected target will be unselected 
    ; after the given mouse event
    ; DETAILS: button-up unselects the target
    ; EXAMPLE: a world in which the target is selected will be unselected on 
    ; the button-up event
    
    (define/public (target-after-button-up)
      (begin (set! mouse-x ZERO)
             (set! mouse-y ZERO)
             (set! selected? false)
             (set! color BLACK-COLOR)))
    ;-------------------------------------------------------------------------;
    
    ; target-after-drag: Integer Integer -> Void
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; EFFECT: update the world that should follow this world after a drag at 
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
    
    ; on-key: KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: update the world like this world, but as it should be after the 
    ; given key event.
    ; DETAILS: on pressing "s" a new square-toy should be added to this world.
    ; EXAMPLE: consider a world in which target exists at location (70, 100). 
    ; On pressing the "s" key, a new square-toy is created in the given world 
    ; object at the centre of the target.
    ; STRATEGY: Cases on evn : KeyEvent
    
    (define/public (on-key evn)
      (cond
        [(key=? evn KEY-S) (square-creation)]
        [else this]))
    ;-------------------------------------------------------------------------;
    ; square-creation: -> Void
    ; GIVEN: no arguments
    ; EFFECT: update the world as it should be after the key event. 
    ; A square-toy added to this world on target location
    ; EXAMPLE: consider a world consists of target at (70, 100). On call of 
    ; the function, a new square-toy is created at (70,100) i.e. center of the
    ; target location with square length of SQUARE-LENGTH. 
    
    (define/public (square-creation)
      (set! toys (cons (make-square-toy x y this) toys)))
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
    
    (define/public (target-selected?) 
      selected?)
    ;-------------------------------------------------------------------------;
    
    ; target-color -> ColorString
    ; GIVEN: no arguments
    ; RETURNS: the color of the target, black if target not selected else orange
    ; EXAMPLE: consider a world with target selected, On call of target-color
    ; will return orange
    
    (define/public (target-color) 
      color)
    ;-------------------------------------------------------------------------;
    
    ; get-toys: -> ListOfStatefulToy<%>
    ; GIVEN: no arguments
    ; RETURNS: the list of toys which is present on this world
    ; EXAMPLE: 
    ; 1. Consider a world with target and no toys. On call of this method 
    ;    will return empty.
    ; 2. Coniser a world with target and square-toy. On call of this method
    ;    will return a list with a single square-toy.
    
    (define/public (get-toys) 
      toys)
    ;-------------------------------------------------------------------------;
    
    ; for-test:update-toys: ListOfStatefulToy<%> -> Void
    ; GIVEN: a list of statefultoys
    ; EFFECT: updates the world with toys begin set to the given list of toys
    ; EXAMPLE: on call of this function, world toys will be udpated with the 
    ; given input list of toys
    
    (define/public (for-test:update-toys t)
      (set! toys t))
    
    ))

;------------------------------------------------------------------------------;

; SquareToy% -- a class that satisfies the StatefulToy<%>, Publisher<%> and 
;               Subscriber<%> interface 

; A SquareToy is a (new SquareToy% [x Integer] 
;                                  [y Integer] 
;                                  [world StatefulWorld<%>] 
;                                  [selected? Boolean]
;                                  [color ColorString]
;                                  [mouse-x Integer]
;                                  [mouse-y Integer])

; INTERPRETATION: 
; x represents a square-toy x coordinate in pixels
; y represents a square-toy y coordinates in pixels
; world represents this world
; selected? represents whether the square-toy is selected or not
; color represents the color of the target
; mouse-x represents the x coordinate of the mouse in pixels
; mouse-y represents the y coordinate of the mouse in pixels

(define SquareToy%
  (class* object% (StatefulToy<%> Publisher<%> Subscriber<%>)
    (init-field x                   ; x coordinate in pixels
                y                   ; y x coordinate in pixels
                world               ; represents this world
                [selected? false]   ; represents selected or not
                [color GREEN-COLOR] ; reprsents the color of target
                [mouse-x 0]         ; mouse x coordinate in pixels
                [mouse-y 0]         ; mouse y coordinate in pixels
                )   
    
    (field [SQUARE-IMG              ; image for displaying a square-toy
            (square SQUARE-LENGTH OUTLINE color)])
    
    (init-field [buddy-list empty]) ; represents the ListOfStatefulToy<%>
    
    (super-new)
    ;-------------------------------------------------------------------------;
    
    ; on-mouse: Integer Integer MouseEvent -> Void
    ; GIVEN: x, y coordinates of the mouse and the mouse event
    ; EFFECT: updates the square-toy to the state as it should be after the 
    ; given mouse event.
    ; EXAMPLE: if an enter is given mouse event then the square toy will be have
    ; no effect
    ; STRATEGY: Cases on evn: MouseEvent
    
    (define/public (on-mouse mx my evnt)
      (cond
        [(mouse=? evnt BUTTON-DOWN) (toy-after-button-down mx my)]
        [(mouse=? evnt BUTTON-DRAG) (toy-after-drag mx my)]
        [(mouse=? evnt BUTTON-UP)   (toy-after-button-up)]
        [else this]))
    
    ;--------------------------------------------------------------------------;
    
    ; toy-after-button-down : Integer Integer -> Void
    ; GIVEN : x,y co-ordinates of the mouse
    ; EFFECT: updates the square-toy after the given button down mouse event 
    ; if the mouse coordinates is within the squaretoy else no effect. All the 
    ; buddies of this squaretoy will change color from red to green
    ; EXAMPLE: a square-toy in which the target is not selected will be 
    ; selected on the button-down event. All the buddies of this squaretoy
    ; will change color from red to green
    
    (define/public (toy-after-button-down mx my)
      (if (send this mouse-within-square? mx my)
          (begin (set! mouse-x mx)
                 (set! mouse-y my)
                 (set! selected? true)
                 (set! color RED-COLOR)
                 (publish-color color))
          this))
    ;--------------------------------------------------------------------------;
    
    ; mouse-within-square?: Integer Integer -> Boolean
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; RETURNS: true iff the mouse coordinates is inside the squaretoy
    ; EXAMPLE: true when mouse coordinates are (100, 200) and the squaretoy
    ; is at (99, 203) of length 30. As the mouse is within the area of the 
    ; squaretoy it will return true else returns false
    
    (define/public (mouse-within-square? mx my)
      (and (<= (- x HALF-SQUARE-LENGTH) mx (+ x HALF-SQUARE-LENGTH))
           (<= (- y HALF-SQUARE-LENGTH) my (+ y HALF-SQUARE-LENGTH))))
    ;--------------------------------------------------------------------------;
    
    ; toy-after-drag: Integer Integer -> Void
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; EFFECT: update the squaretoy to the state as it should be on drag. All 
    ; the buddies of this square toy shall move relatively with this squaretoy
    ; DETAILS: if the squaretoy is selected, move the squaretoy with the mouse
    ; otherwise ignore. All the buddies of this squaretoy shall also move 
    ; relatively with this squaretoy
    ; EXAMPLE:  a squaretoy with no buddies will smoothly move with the mouse 
    ; location, if selected
    
    (define/public (toy-after-drag mx my)
      (local
        ((define x1 (- mx mouse-x))
         (define y1 (- my mouse-y)))
        (if selected?
            (begin (check-for-new-buddies this)
                   (update-toy-drag mx my)
                   (publish-coordinates x1 y1))          
            this)))
    ;--------------------------------------------------------------------------;
    
    ; update-toy-drag: Integer Integer -> Void
    ; GIVEN: x, y coordinates of the mouse, i.e. location of the mouse
    ; EFFECT: update the squaretoy to the state as it should be on drag. This 
    ; squaretoy will move with mouse location
    ; EXAMPLE:  a squaretoy with no buddies will smoothly move with the mouse 
    ; locations
    
    (define/public (update-toy-drag mx my)
      (begin (set! x (+ x (- mx mouse-x)))
             (set! y (+ y (- my mouse-y)))
             (set! mouse-x mx)
             (set! mouse-y my)
             (publish-color color)))
    ;--------------------------------------------------------------------------;
    
    ; check-for-new-buddies: StatefulToy<%> -> Void
    ; GIVEN: a squaretoy
    ; EFFECT: update the squaretoy with new buddies if squaretoy edge touches 
    ; other squaretoy edge.
    ; EXAMPLE:  a squaretoy with no buddies and dragged to a new squaretoy in 
    ; the same world becomes buddies if the square edges touches
    
    (define/public (check-for-new-buddies sqr)
      (local
        ((define toys-list (set-diff (send world get-toys) (list sqr))))
        (for-each
         ; StatefulToy<%> -> Void
         ; GIVEN: a square toy
         ; EFFECT: updates buddy-list of the given squaretoy along with the 
         ;         squaretoy with which the given squaretoy overlaps
         (lambda (s) (if(and (not (member s buddy-list))
                             (send sqr squares-overlap? 
                                   (send s toy-x) 
                                   (send s toy-y)))                      
                        (begin 
                          (send s subscribe sqr)
                          (send sqr subscribe s))
                        s))
         toys-list)))    
    ;--------------------------------------------------------------------------;
    
    ; subscribe: StatefulToy<%> -> Void
    ; GIVEN: a square toy
    ; EFFECT: updates the buddy-list of this squaretoy with the squaretoy that
    ; is passed as an argument    
    ; EXAMPLES: If this squaretoy have coordinates as (100, 200) and if it 
    ; overlaps with another squaretoy (112, 202) in the same world then this 
    ; squaretoy will add another squaretoy to its buddy list
    
    (define/public (subscribe sq)
      (set! buddy-list (cons sq buddy-list)))    
    ;--------------------------------------------------------------------------;
    
    ; publish-color: ColorString -> Void
    ; GIVEN: the color for a squartoy
    ; EFFECT: updates the color of the squaretoy buddy list with given color
    ; EXAMPLE: if the given colorsting has value red and is published. After
    ; publish the color of all the buddies of this squartoy will updated to red
    
    (define/public (publish-color col)
      (for-each
       ; StatefulToy<%> -> Void
       ; GIVEN: a squaretoy
       ; EFFECT: updates the given squaretoy with the input color
       (lambda(s) (send s change-color col))
       buddy-list
       ))
    ;--------------------------------------------------------------------------;
    
    ; change-color: ColorString -> Void
    ; GIVEN: is the color for a squaretoy.
    ; EFFECTS: updates color of this squaretoy to the input color
    ; EXAMPlES: on the call of this method shall update the color of squaretoy
    ; with the given color
    
    (define/public (change-color col)
      (set! color col))
    ;--------------------------------------------------------------------------;
    
    ; publish-coordinates: Integer Integer -> Void
    ; GIVEN: the x,y coordiantes of the mouse
    ; EFFECTS: update the position of the buddies of this squaretoy
    ; EXAMPLE: If a squaretoy have a buddy and then the all the buddies will 
    ; relatively move on drag of this squaretoy
    
    (define/public (publish-coordinates mx my)
      (for-each
       ; StatefulToy<%> -> Void
       ; GIVEN: a square toy
       ; EFFECT: updates the position of the squaretoy relatively
       (lambda(s) (send s position-change mx my))
       buddy-list))
    ;--------------------------------------------------------------------------;
    
    ; position-change: Integer Integer -> Void
    ; GIVEN: the relative offset values of the mouse coordinates and squaretoy
    ; EFFECT: update the position of this squaretoy. Also, while moving if some
    ; other squaretoy intersects and which is not in the current buddy list 
    ; becomes buddy with the intersecting squaretoy
    ; EXAMPLE: if a squaretoy has a buddy, then with the movement of this 
    ; squaretoy buddy will also move relatively.
    
    (define/public (position-change change-x change-y)
      (if (not selected?)
          (begin
            (set! x (+ x change-x))
            (set! y (+ y change-y))
            (check-for-new-buddies this))
          this
          ))
    ;--------------------------------------------------------------------------;
    
    ; toy-after-button-up: -> Void
    ; GIVEN: no arguments
    ; EFFECT: update this squaretoy color and selected? to green and false.
    ; Also, publish the new color for buddies.
    ; EXAMPLE: If a button-up is done on a selected squaretoy, then it becomes
    ; unselected and the color changes to green
    
    (define/public (toy-after-button-up)
      (begin (set! selected? false)
             (set! color GREEN-COLOR)
             (publish-color color)))
    ;--------------------------------------------------------------------------;
    
    ; squares-overlap?: Integer Integer -> Boolean
    ; GIVEN: the x,y coordinates of the squaretoy
    ; RETURNS: true if the input coordinates lies on or inside this squaretoy
    ; else false
    ; EXAMPLE: assume a squaretoy at (100, 200). A different squaretoy at 
    ; (101, 200) overlaps with the previous squaretoy, thus true is returned
    
    (define/public (squares-overlap? other-x other-y)
      (and (<= (abs (- x other-x)) SQUARE-LENGTH)
           (<= (abs (- y other-y)) SQUARE-LENGTH)))
    ;--------------------------------------------------------------------------;
    
    ; add-to-scene: Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given scene but with squaretoy painted on it
    ; EXAMPLE: assumne a empty world with only target. On call of this method,
    ; a square-toy with length 30, outline and green color will be pasted on 
    ; the given scene
    
    (define/public (add-to-scene scene)
      (place-image (square SQUARE-LENGTH OUTLINE color) x y scene))
    ;--------------------------------------------------------------------------;
    
    ; get-x: -> Integer
    ; RETURNS: the x coordinate of this squaretoy
    ; EXAMPLES: (send initial-square-toy get-x) -> 200
    
    (define/public (toy-x) 
      x)
    ;--------------------------------------------------------------------------;
    
    ; get-y: -> Integer
    ; RETURNS: the y coordinate of this squaretoy
    ; EXAMPLES: (send initial-square-toy get-y) -> 250
    
    (define/public (toy-y) 
      y)
    ;--------------------------------------------------------------------------;
    
    ; toy-color: -> Colorstring
    ; RETURNS: the current color of the toy
    ; EXAMPLES: (send initial-square-toy toy-color) -> green
    
    (define/public (toy-color) 
      color)
    ;--------------------------------------------------------------------------;
    
    ; toy-selected?: -> Boolean
    ; RETURNS: true if toy is selected else false
    ; EXAMPLES: (send initial-square-toy toy-selected?) -> false
    
    (define/public (toy-selected?) 
      selected?)
    ))
;------------------------------------------------------------------------------;

; make-world : -> World%
; GIVEN: no arguments
; RETURNS: A World% with no squares.
; EXAMPLE: (make-world) => (new World%)
; STRATEGY: Function Composition

(define (make-world)
  (new World%))

; TESTS
; tests at the end
;------------------------------------------------------------------------------;

; make-square-toy : Integer Integer -> SquareToy%
; GIVEN: an x and a y position
; RETURNS: an object representing a square toy at the given position
; EXAMPLE: (make-square-toy 100 200 initial-world) ->
; (new SquareToy% [x 100] [y 200] [world initial-world])
; STRATEGY: Function Composition

(define (make-square-toy new-x new-y w)
  (new SquareToy% [x new-x] [y new-y] [world w]))

; TESTS
; tests at the end
;------------------------------------------------------------------------------;

; run : PosNum -> World%
; GIVEN: a frame rate (in seconds/tick)
; EFFECT: creates and runs a world that runs at the given rate.
; RETURNS: the final world.

(define (run rate)
  (big-bang (make-world)
            (on-tick  
             ; World -> World
             ; GIVEN: a world
             ; RETURNS: the world that should follow this one after a tick
             (lambda (w) (send w on-tick) w) rate)
            (on-draw  
             ; World -> Scene
             ; GIVEN: a world
             ; RETURNS: a scene depicting the given world on it.
             (lambda (w) (send w on-draw)))
            (on-key   
             ; World KeyEvent -> World
             ; GIVEN: a world and a key event
             ; RETURNS: the world that should follow the given one after 
             ; the given key event
             (lambda (w kev) (send w on-key kev) w))
            (on-mouse 
             ; World Integer Integer MouseEvent -> World
             ; GIVEN: a world, the x, y coordinates of the mouse and the 
             ; mouse event
             ; RETURNS: the world that should follow given one after the 
             ; given mouse event
             (lambda (w mx my evnt) (send w on-mouse mx my evnt) w))))
;------------------------------------------------------------------------------;

; TESTS

; testing for world
(define initial-world (make-world))
(define initial-world-with-1-toy (make-world))
(send initial-world-with-1-toy on-key "s")
(define initial-world-with-0-toy (make-world))
(send initial-world-with-0-toy on-key "s")
(define initial-square-toy (new SquareToy%  [x 200] [y 250] [world void]))
(define square-toy-1-1-buddy
  (new SquareToy% [x 200] [y 250] [world void] [buddy-list empty]))
(define initial-world-with-one-square
  (place-image (square SQUARE-LENGTH OUTLINE GREEN-COLOR) 200 250
               (place-image (circle TARGET-RADIUS OUTLINE BLACK-COLOR) 
                            200 250 EMPTY-CANVAS)))

;--------------------------------------------------------------------------; 

(begin-for-test
  ;world tests
  (local
    ((define initial-world (make-world)))
    (check-equal? (send initial-world target-x) HALF-CANVAS-WIDTH
                  "Test for target-x method on initial-world")
    (check-equal? (send initial-world target-y) HALF-CANVAS-HEIGHT
                  "Test for target-y method on initial-world")
    (check-equal? (send initial-world target-selected?) false
                  "Test for target-selected? method on initial-world")
    (check-equal? (send initial-world target-color) "black"
                  "Test for target-color method on initial-world")
    (check-equal? (send initial-world get-toys) empty
                  "Test for get-toys method on initial-world")
    (check-equal? (send initial-world on-tick) initial-world
                  "Test for on-tick method on initial-world")
    (check-equal? (send initial-world on-mouse 200 250 "enter") initial-world
                  "Test for on-mouse event enter on initial-world")
    (send initial-world on-mouse 200 250 "button-down")
    (check-equal? (send initial-world target-selected?) true
                  "Test for on-mouse event button-down on initial-world")    
    (send initial-world on-mouse 220 250 "drag")
    (check-equal? (send initial-world target-x) 220
                  "Test for on-mouse event button-drag on initial-world")    
    (send initial-world on-mouse 220 250 "button-up")
    (check-equal? (send initial-world target-selected?) false
                  "Test for on-mouse event button-drag on initial-world")
    (send initial-world on-mouse 220 250 "enter")
    (check-equal? (send initial-world target-selected?) false
                  "Test for on-mouse event button-drag on initial-world")))
; on draw test
(begin-for-test
  (send initial-world on-key KEY-S)
  (check-equal? (send initial-world on-draw) initial-world-with-one-square
                "Test for the on-draw of world and for key event s"))

; buddies test

(define initial-world-with-0-toy-after-moves
  
  (place-image (square SQUARE-LENGTH OUTLINE GREEN-COLOR)
               118 250 
               (place-image (square SQUARE-LENGTH OUTLINE GREEN-COLOR)
                            88 250
                            (place-image (circle TARGET-RADIUS 
                                                 OUTLINE BLACK-COLOR) 
                                         200 250 EMPTY-CANVAS))))

(begin-for-test
  ; check for world and buddies
  (send initial-world-with-0-toy on-mouse 212 250 BUTTON-DOWN) 
  (send initial-world-with-0-toy on-mouse 100 250 BUTTON-DRAG)
  (send initial-world-with-0-toy on-mouse 100 250 BUTTON-UP)
  (send initial-world-with-0-toy on-key "q")
  (send initial-world-with-0-toy on-key "s")
  (send initial-world-with-0-toy on-mouse 212 250 BUTTON-DOWN)
  (send initial-world-with-0-toy on-mouse 130 250 BUTTON-DRAG)
  (send initial-world-with-0-toy on-mouse 130 250 BUTTON-UP)
  (check-equal? (send initial-world-with-0-toy on-draw)
                initial-world-with-0-toy-after-moves)
  
  ; check for toys
  (check-equal? (send initial-square-toy toy-color) GREEN-COLOR
                "Test for the toy color")
  (check-equal? (send initial-square-toy toy-selected?) false
                "Test for the toy selected")
  (send initial-square-toy change-color "red")
  (check-equal? (send initial-square-toy squares-overlap? 200 250) true
                "Test for square overlap condition")
  (check-equal? (send initial-square-toy on-mouse 100 200 "enter") 
                initial-square-toy "Test for the other mouse events"))


(define world1 (make-world))
(define square-toy-1-buddy
  (new SquareToy%  
       [x 200] [y 250] [world world1] 
       [buddy-list (list (new SquareToy%  
                              [x 200] [y 250] [world world1]))]))
(define square-toy-2 (new SquareToy%  
                          [x 300] [y 250] [world world1]))
(define square-toy-3 (new SquareToy%  
                          [x 195] [y 250] [world world1]
                          [selected? true]))

(begin-for-test
  (send world1 for-test:update-toys (list square-toy-1-buddy
                                          square-toy-2
                                          square-toy-3))
  
  (check-equal? (length (send world1 get-toys)) 3
                "Test for the number of toys in the world")
  (send square-toy-1-buddy on-mouse 210 250 "button-down")
  (send square-toy-1-buddy on-mouse 300 250 "drag")
  (send square-toy-3 position-change 100 200)
  (check-equal? (send square-toy-3 toy-x) 195)
  (check-equal? (send square-toy-3 toy-y) 250))
;------------------------------------------------------------------------------;