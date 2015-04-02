;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;-------------------------------------------------------------------------;
;                       FILE NAME: trees.rkt                              ;
;-------------------------------------------------------------------------;


;-------------------------------------------------------------------------;
;                         PROBLEM STATEMENT                               ;
;-------------------------------------------------------------------------;
;                                                                         ;
; Problem is to design and implement a system for a graphical interface   ;
; for trees. System shall allow to create and manipulate trees on a canvas;
; Any of the nodes shall be selectable and draggable.                     ;
;                                                                         ;
; Key Press functionality:                                                ;
;                                                                         ;
; "t" : Hitting "t" at any time creates a new root node.                  ;
; "n" : Hitting "n" while a node is selected adds a new son.              ;
; "d" : Hitting "d" while a node is selected deletes the node and its     ;
;       whole subtree.                                                    ;
; "u" : Hitting "u" (whether a node is selected or not) deletes every node;
;       whose center is in the upper half of the canvas along with all of ;
;       its children.                                                     ;
;                                                                         ;
; More details can be found on:                                           ;
; http://www.ccs.neu.edu/course/cs5010f14/Problem%20Sets/ps05.html        ;
;-------------------------------------------------------------------------;

; require
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

; start with (run AnyValue)

; providing functions
(provide run)
(provide initial-world)
(provide world-after-mouse-event)
(provide world-after-key-event)
(provide world-to-roots)
(provide node-to-center)
(provide node-to-sons)
(provide node-to-selected?)

;-------------------------------------------------------------------------;
;                             RUN FUNCTION                                ;
;-------------------------------------------------------------------------;

; run:  Any -> World
; GIVEN: any value
; EFFECT: runs a copy of an initial world
; RETURNS: the final state of the world.  The given value is ignored.

(define (run any)
  (big-bang (initial-world any)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;-------------------------------------------------------------------------;
;                             CONSTANTS                                   ;
;-------------------------------------------------------------------------;

; numbers
(define ZERO 0)
(define TWO 2)
(define THREE 3)

; colors
(define RED "red")
(define BLUE "blue")
(define GREEN "green")

; mode
(define OUTLINE "outline")
(define SOLID "solid")

; mouse-events
(define BUTTON-DOWN "button-down")
(define BUTTON-DRAG "drag")
(define BUTTON-UP "button-up")
(define OTHER-MOUSE-EVENT "enter")

; key-events
(define KEY-N "n")
(define KEY-T "t")
(define KEY-U "u")
(define KEY-D "d")
(define OTHER-KEY " ")

; canvas and square attributes
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 400)
(define SQUARE-LENGTH 20)
(define HALF-SQUARE-LENGTH (/ SQUARE-LENGTH 2))
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

; squares and canvas image
(define SQUARE-OUTLINE-GREEN (square SQUARE-LENGTH OUTLINE GREEN))
(define SQUARE-SOLID-GREEN (square SQUARE-LENGTH SOLID GREEN))
(define SQUARE-SOLID-RED (square SQUARE-LENGTH SOLID RED))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

; constants for test
(define NEW-CHILD-X 160)
(define ROOT1-ROOT2-Y 70)
(define DRAG-X 200)
(define DRAG-Y 200)

; scenes for test
(define scene-1
  (place-image 
   SQUARE-OUTLINE-GREEN 100 30
   (scene+line
    (place-image SQUARE-OUTLINE-GREEN 100 90 EMPTY-CANVAS)
    100 90 100 30 BLUE)))
(define scene-2
  (scene+line 
   (place-image SQUARE-SOLID-GREEN 100 30 EMPTY-CANVAS)
   90 0 90 400 RED))
(define scene-3
  (scene+line 
   (place-image SQUARE-SOLID-RED 9 30 EMPTY-CANVAS)
   -1 0 -1 400 RED))
(define scene-4
  (scene+line
   (place-image 
   SQUARE-OUTLINE-GREEN 100 30
   (scene+line
    (place-image SQUARE-OUTLINE-GREEN 100 90 EMPTY-CANVAS)
    100 90 100 30 BLUE)) 100 90 100 30 BLUE))
;-------------------------------------------------------------------------;
;                           DATA DEFINITIONS                              ;
;-------------------------------------------------------------------------;

(define-struct node (x-pos y-pos selected? children))

; A Node is a (make-node Real Real Boolean ListOfNode) 

; INTERPRETATION:
;  x-pos     : x coordinate of the node in pixels
;  y-pos     : y coordinate of the node in pixels
;  selected? : tells whether the node is selected or not
;  children  : sub-nodes of the given node

; TEMPLATE:
; node-fn : Node -> ??
;(define (node-fn n)
;  (... 
;   (node-x-pos n) 
;   (node-y-pos n) 
;   (node-selected? n) 
;   (lon-fn (node-children n))))

; EXAMPLES for tests:
(define node-1 (make-node 100 30 false empty))
(define node-2 (make-node 100 30 false (make-node 100 60 false empty)))
(define node-3 (make-node 100 30 false (list (make-node 100 90 false empty))))
(define node-4 (make-node 100 30 true empty))
(define node-5 (make-node 9 30 true empty))
(define root1-root2 (make-node 160 70 false empty))
(define root1-root3 (make-node 200 70 false empty)) 
(define root1 (make-node 200 10 false (list root1-root2 root1-root3)))
(define root1-selected (make-node HALF-CANVAS-WIDTH
                                  HALF-SQUARE-LENGTH
                                  true (list root1-root2 root1-root3)))
(define root1-root2-drag (make-node 160 260 false empty))
(define root1-root3-drag (make-node 200 260 false empty))
(define root1-drag (make-node 200 200 true (list root1-root2-drag 
                                                root1-root3-drag)))
(define root2-root2 (make-node 220 280 false empty))
(define root2-root1 (make-node 180 280 false empty))
(define root2 (make-node 220 220 false (list root2-root1 root2-root2)))
(define root3 (make-node 220 280 false (list root1-root2)))
(define root3-no-child (make-node 220 280 false empty))
;-------------------------------------------------------------------------;

; A ListOfNode (LON) is one of
; -- empty            (interp: a sequence with no elements)
; -- (cons Node LON)  (interp: (cons Node LON) represents a sequence whose
;                              first element is Node and whose other 
;                              elements are represented by LON)

; TEMPLATE:
; lon-fn : LON -> ??
; (define (lon-fn lon)
;   (cond
;     [(empty? lon) ...]
;     [else (...
;             (node-fn (first lon))
;             (lon-fn (rest lon)))]))
;-------------------------------------------------------------------------;

; A World is a ListOfNode (LON).

; INTERPRETATION: 
; World is the list of nodes.

; EXAMPLES for tests
(define world-1 
  (list (make-node 100 30 false empty)))
(define world-2 
  (list (make-node 100 30 false (list (make-node 100 90 false empty)))))
(define world-3 (list (make-node 100 60 false empty)
                      (make-node 80 60 true empty)))

;-------------------------------------------------------------------------;
;                        END DATA DEFINITIONS                             ;
;-------------------------------------------------------------------------;


;-------------------------------------------------------------------------;
;                         FUNCTION DEFINITIONS                            ;
;-------------------------------------------------------------------------;

; initial-world : Any -> World
; GIVEN: any value
; RETURNS: an initial world.  The given value is ignored.
; EXAMPLES: (initial-world 8) => empty
; STRATEGY: Function Composition

(define (initial-world any)
  empty)

; TESTS
(begin-for-test
  (check-equal? (initial-world 9) empty 
                "Initial World should return an empty world")) 
                                                   
;-------------------------------------------------------------------------;
;                          WORLD TO SCENE                                 ;
;-------------------------------------------------------------------------;

; world-to-scene : World -> Scene
; GIVEN: a world
; RETURNS: a scene that portrays the given world.
; EXAMPLE: (world-to-scene world-1) =>
;           (place-image SQUARE-OUTLINE-GREEN 100 30 EMPTY-CANVAS)
; STRATEGY: HOFC

(define (world-to-scene lon)
  (foldr
   ; Node Scene -> Scene
   ; GIVEN: a node and a scene
   ; RETURNS: Scene with node drawn on that scene
   (lambda (n scene) (world-to-scene-helper n scene))
   EMPTY-CANVAS
   lon))

;TESTS
(begin-for-test
  (check-equal? (world-to-scene world-1) 
                (place-image SQUARE-OUTLINE-GREEN 100 30 EMPTY-CANVAS)
                "Test Failed for world-to-scene"))
;-------------------------------------------------------------------------;

; world-to-scene-helper : Node Scene -> Scene
; GIVEN: a node and a scene
; RETURNS: if node is selected then a node is added to the canvas along with
;          a canvas height size line added to scene at the node's next possible 
;          child location else only a node is added to the canvas         
; EXAMPLE: (world-to-scene-helper node-3 EMPTY-CANVAS) => scene-1
; STRATEGY: Structural Decomposition on n : Node

(define (world-to-scene-helper n scene)
  (if (node-selected? n)
       (scene+line (draw-node n scene)
                   (required-x-cord n HALF-SQUARE-LENGTH) ZERO
                   (required-x-cord n HALF-SQUARE-LENGTH) CANVAS-HEIGHT
                   RED)
       (draw-node n scene)))

; TESTS
(begin-for-test
  (check-equal? (world-to-scene-helper node-4 EMPTY-CANVAS) scene-2
                "Test Failed for world-to-scene-helper for selected node")
  (check-equal? (world-to-scene-helper node-5 EMPTY-CANVAS) scene-3
                "Test Failed for world-to-scene-helper for selected node
                 crossing the left range"))
;-------------------------------------------------------------------------;

; draw-node : Node Scene -> Scene
; GIVEN: a node and a scene
; RETURNS: a scene with node appened to it and a line is drawn between
;          child node and its parent node
; EXAMPLE: (draw-node node-3 EMPTY-CANVAS) => scene-1
; STRATEGY: Structural Decomposition on n : Node

(define (draw-node n scene)
  (place-image 
   (square-type n) (node-x-pos n) (node-y-pos n)
   (draw-line-between-node 
    (node-x-pos n) (node-y-pos n) (node-children n) scene)))

; TESTS
(begin-for-test 
  (check-equal? (draw-node node-3 EMPTY-CANVAS) scene-1
                "Test Failed for draw-node")) 
;-------------------------------------------------------------------------;

; draw-line-between-node : Real Real ListOfNode Scene -> Scene
; GIVEN: x,y coordinates of the parent node, list of child nodes 
;        and the scene
; RETURNS: a scene with node drawn on to it and a line is drawn between
;          child node and its parent node
; EXAMPLE: (draw-line-between-node 100 30 world-2 EMPTY-CANVAS) => scene-1
; STRATEGY: Structural Decomposition on n : Node

(define (draw-line-between-node parent-x parent-y lon scene)
  (foldr
   ; Node Scene -> Scene
   ; GIVEN: a node and a scene
   ; RETURNS: scene with a blue line drawn between parent and child node
   (lambda (n s) (scene+line (world-to-scene-helper n s) (node-x-pos n)
                             (node-y-pos n) parent-x parent-y BLUE)) 
   scene 
   lon)) 

; TESTS
(begin-for-test
  (check-equal? (draw-node node-3 EMPTY-CANVAS) scene-1
                "Line should be drawn from child to parent"))
;-------------------------------------------------------------------------;

; square-type : Node -> Image
; GIVEN: a node
; RETURNS: square image, with type as described below, 
;          - if selected and next probable child node position minus 
;             half square length is less than zero draw square with color
;             red and  mode solid else draw square with color green
;             and mode as solid.
;          - if not selected, draw square with color green and 
;            mode as outline.
; EXAMPLE: (square-type node-5) => SQUARE-SOLID-RED
; STRATEGY: Structural Decomposition on n : Node

(define (square-type n)
  (if (node-selected? n)
      (solid-square-type n)
      SQUARE-OUTLINE-GREEN))

; TESTS 
(begin-for-test
  (check-equal? (square-type node-5) SQUARE-SOLID-RED 
                "Test Failed for square-type, square should be solid red"))
;-------------------------------------------------------------------------;

; solid-square-type : Node -> Image
; GIVEN: a node which is selected
; RETURNS: a square image with the selected node drawn on it
;          - if the next probable child node location
;            is inside the canvas ,then draw square with
;            color green and mode solid else
;            draw square with color red and mode outline.
; EXAMPLE: (solid-square-type node-5) => SQUARE-SOLID-RED
; STRATEGY: Structural Decomposition on n : Node
(define (solid-square-type n)
  (if (< (required-x-cord n HALF-SQUARE-LENGTH) ZERO)
      SQUARE-SOLID-RED
      SQUARE-SOLID-GREEN)) 

; TESTS:
(begin-for-test
  (check-equal? (solid-square-type node-5) SQUARE-SOLID-RED
                "Solid red square should be drawn to the canvas"))

;-------------------------------------------------------------------------;
;                        WORLD AFTER MOUSE EVENT                          ;
;-------------------------------------------------------------------------;

; world-after-mouse-event : World Integer Integer MouseEvent -> World
; GIVEN: a world before the mouse event
; RETURNS: a world that should follow a given mouse event
; EXAMPLE: (world-after-mouse-event (list root1) HALF-CANVAS-WIDTH
;           HALF-SQUARE-LENGTH BUTTON-DOWN) => (list root1-selected)
; STRATEGY: HOFC

(define (world-after-mouse-event lon mouse-x mouse-y mev)
  (map
   ; Node -> Node
   ; GIVEN: a node
   ; RETURNS: a node followed by given mouse event
   (lambda(n) (mouse-event-helper n mouse-x mouse-y mev))
   lon))

; TESTS
(begin-for-test
  ; Test if root node of root1 node is selected or not.
  (check-equal?
   (world-after-mouse-event 
    (list root1) HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH BUTTON-DOWN)
   (list root1-selected) "Root 1 should be selected after button down")) 
;-------------------------------------------------------------------------;

; mouse-event-helper : Node -> Node
; GIVEN: a node
; RETURNS: a node followed by mouse event
; EXAMPLE: (mouse-event-helper root1-selected HALF-CANVAS-WIDTH 
;           HALF-SQUARE-LENGTH BUTTON-UP) => root1
; STRATEGY: Cases on mev : MouseEvent

(define (mouse-event-helper n mouse-x mouse-y mev)
  (cond
    [(mouse=? mev BUTTON-DOWN)(node-after-button-down n mouse-x mouse-y mev)]
    [(mouse=? mev BUTTON-DRAG)(node-after-drag        n mouse-x mouse-y mev)]
    [(mouse=? mev BUTTON-UP)  (node-after-button-up   n mouse-x mouse-y mev)]
    [else n]))

; TESTS
(begin-for-test   
  ; Test if root node of root1 node is unselected after button-up.
  (check-equal? (mouse-event-helper root1-selected HALF-CANVAS-WIDTH  
                                    HALF-SQUARE-LENGTH BUTTON-UP) 
                root1 "Root 1 should be unselected after button-up")  
  ; Test if root node of root1 node is dragged to a location or not.
  (check-equal? (mouse-event-helper root1-selected DRAG-X DRAG-Y BUTTON-DRAG)
                root1-drag "Root 1 should be dragged to location")  
  ; Test for any other mouse event
  (check-equal? (mouse-event-helper root1 DRAG-X DRAG-Y OTHER-MOUSE-EVENT)
                root1 "Unchanged node on any other event"))
;-------------------------------------------------------------------------;

; node-after-button-down : Node Integer Integer MouseEvent -> Node
; GIVEN: a node before button down, mouse event locations and the mouse event
; RETURNS: a node with either itself or  one or several of its children selected
; EXAMPLE: (node-after-button-down root1 ZERO HALF-SQUARE-LENGTH BUTTON-DOWN)
;           => root1
; STRATEGY: Structural Decomposition on n : Node

(define (node-after-button-down n mouse-x mouse-y mev)
  (if (mouse-inside-the-square? n mouse-x mouse-y)
      (make-node (node-x-pos n) (node-y-pos n) true 
                 (world-after-mouse-event 
                  (node-children n) mouse-x mouse-y mev)) 
      (make-node (node-x-pos n) (node-y-pos n) (node-selected? n)
                 (world-after-mouse-event 
                  (node-children n) mouse-x mouse-y mev))))

; TESTS
(begin-for-test  
  ; Test if button-down is outside the root1 node
  (check-equal?
   (node-after-button-down root1 ZERO HALF-SQUARE-LENGTH BUTTON-DOWN)
   root1 "Root1 should remain unchanged after button-down outside of it")) 
;-------------------------------------------------------------------------;

; node-after-button-up : Node Integer Integer MouseEvent -> Node
; GIVEN: a node before button-up, mouse event x and y locations and 
;        the mouse event
; RETURNS: a node with itself or one or several of its children unselected
; EXAMPLE: (node-after-button-up root1 ZERO HALF-SQUARE-LENGTH BUTTON-UP)
;           => root1
; STRATEGY: Structural Decomposition on n : Node

(define (node-after-button-up n mouse-x mouse-y mev)
  (if (node-selected? n)
      (make-node (node-x-pos n) (node-y-pos n) false 
                 (world-after-mouse-event 
                  (node-children n) mouse-x mouse-y mev))
      (make-node (node-x-pos n) (node-y-pos n) (node-selected? n)
                 (world-after-mouse-event 
                  (node-children n) mouse-x mouse-y mev))))

; TESTS
(begin-for-test  
  ; Test if button-up on node not selected. 
  (check-equal?
   (node-after-button-up root1 ZERO HALF-SQUARE-LENGTH BUTTON-UP)
   root1 "Root1 should remain unchanged on button up if not selected")) 
;-------------------------------------------------------------------------;

; node-after-drag : Node Integer Integer MouseEvent  -> Node
; GIVEN: a node before mouse drag event, mouse drag x and y locations 
;        and mouse event
; RETURNS: a node with either itself or one or more of its children
;          dragged to a location
; EXAMPLE: (node-after-drag root1 ZERO HALF-SQUARE-LENGTH BUTTON-DRAG)
;          => root1
; STRATEGY: Structural Decomposition on n : Node

(define (node-after-drag n mouse-x mouse-y mev)
  (if (node-selected? n)
      (make-node mouse-x mouse-y true 
                 (move-node-with-children (node-children n) (node-x-pos n) 
                                          (node-y-pos n) mouse-x mouse-y ))
      (make-node (node-x-pos n) (node-y-pos n) 
                 (node-selected? n) (world-after-mouse-event 
                                     (node-children n) mouse-x mouse-y mev))))

; TESTS
(begin-for-test
  ; Test if button-drag on node not selected.
  (check-equal?
   (node-after-drag root1 ZERO HALF-SQUARE-LENGTH BUTTON-DRAG)
   root1 
   "Root1 should not be dragged to the drag locations"))
;-------------------------------------------------------------------------;

; move-node-with-children : ListOfNode Real Real Integer Integer -> ListOfNode
; GIVEN: children of a node, children's parent x and y coordinates 
;        and the dragged location x and y coordinates.
; RETURNS: children updated with their relative new locations 
;          after being dragged.
; EXAMPLE: (move-node-with-children (list root1-root2 root1-root3) 
;           HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH DRAG-X DRAG-Y)
;           => (list root1-root2-drag root1-root3-drag)
; STRATEGY: HOFC

(define (move-node-with-children lon parent-x parent-y mouse-x mouse-y)
  (map
   ; Node -> Node
   ; GIVEN: a node
   ; RETURNS: a node followed by given mouse event
   (lambda (n) (drag-children n parent-x parent-y mouse-x mouse-y))
   lon))

; TESTS
(begin-for-test  
  ; Test for checking if all children of a node are dragged properly.
  (check-equal?
   (move-node-with-children (list root1-root2 root1-root3) HALF-CANVAS-WIDTH 
                            HALF-SQUARE-LENGTH DRAG-X DRAG-Y)
   (list root1-root2-drag root1-root3-drag)
   "Children should be dragged to their relative new locations"))
;-------------------------------------------------------------------------;

; drag-children : Node Real Real Integer Integer -> Node
; GIVEN: a node that is to be dragged to a location, parent node x and y
;        locations of the node and the location of its parent node being
;        dragged to.
; RETURNS: a node that is dragged to its relative new position along 
;          with all its children.
; EXAMPLE: (drag-children root1-root2 HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH
;          DRAG-X DRAG-Y) => root1-root2-drag
; STRATEGY: Structural Decomposition on n : Node

(define (drag-children n parent-x parent-y mouse-x mouse-y)
  (make-node (+ (node-x-pos n) (- mouse-x parent-x ))
             (+ (node-y-pos n) (- mouse-y parent-y))
             false
             (move-node-with-children 
              (node-children n) parent-x parent-y mouse-x mouse-y)))

; TESTS 
(begin-for-test
  ; Test if node has been dragged to the correct relative location.
  (check-equal?
   (drag-children root1-root2 HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH
                  DRAG-X DRAG-Y) root1-root2-drag
   "Root2 which is the child of root1 should be dragged to its 
    specific location"))
;-------------------------------------------------------------------------;

; mouse-inside-the-square? : Node Integer Integer -> Boolean
; GIVEN: a node and the mouse event location x and y coordinates.
; RETURNS: true iff the mouse event location occurs within the node
; EXAMPLE: (mouse-inside-the-square? root1 HALF-CANVAS-WIDTH 
;           HALF-SQUARE-LENGTH) => true
; STRATEGY: Structural Decomposition on n : Node

(define (mouse-inside-the-square? n mouse-x mouse-y)
  ( and (<= (- (node-x-pos n) HALF-SQUARE-LENGTH) 
            mouse-x 
            (+ (node-x-pos n) HALF-SQUARE-LENGTH))
        (<= (- (node-y-pos n) HALF-SQUARE-LENGTH) 
            mouse-y 
            (+ (node-y-pos n) HALF-SQUARE-LENGTH))))

; TESTS
(begin-for-test  
  ; Tests if mouse event location is inside the node of the node
  (check-equal?
   (mouse-inside-the-square? root1 HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH)
   true 
   "Mouse event location should be inside the node and result in true"))

;-------------------------------------------------------------------------;
;                        WORLD AFTER KEY EVENT                            ;
;-------------------------------------------------------------------------;

; world-after-key-event : World KeyEvent -> World
; GIVEN: a world and a keyevent
; RETURNS: a world followed by keyevent
; EXAMPLE: (world-after-key-event empty KEY-T) =>
;          (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false empty))
; STRATEGY: Cases on ke : KeyEvent

(define (world-after-key-event w ke)
  (cond
    [(key=? ke KEY-T) (create-root-node w)]
    [(key=? ke KEY-N) (create-child-node w)]
    [(key=? ke KEY-D) (selected-node-delete w)]
    [(key=? ke KEY-U) (upper-half-node-remove w)]
    [else w]))

; TESTS
(begin-for-test   
  ; Test for creating a new node
  (check-equal? 
   (world-after-key-event empty KEY-T)
   (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false empty))
   "World after key='t' should create a new node")
  
  ; Test for creating a new child
  (check-equal?
   (world-after-key-event 
    (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH true 
                     (list root1-root3))) KEY-N)
   (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH true 
                    (list root1-root2 root1-root3)))
   "Hitting 'n' should create a new child in root1 node.")
  
  ; Test for deleting a child from the node.
  (check-equal?
   (world-after-key-event (list root1-selected) KEY-D)
   empty
   "Root1 node should be deleted from the world.")
  
  ; Test for removing upper half of the node
  (check-equal?
   (world-after-key-event (list root1) KEY-U)
   empty
   "all nodes in the upper half of the canvas should be deleted.")
  
  ; Test for any other key-event
  (check-equal?
   (world-after-key-event (list root1) OTHER-KEY)
   (list root1)
   "Any other key event should not affect the world"))
;-------------------------------------------------------------------------;

; create-root-node : World -> World
; GIVEN: a world
; RETURNS: updated world with one root node added to the world 
; EXAMPLE: (create-root-node empty) =>
;          (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false empty))
; STRATEGY: Function Composition

(define (create-root-node w)
  (cons (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false empty) w))

; TESTS
(begin-for-test  
  ; Test for creation of a new node
  (check-equal?
   (create-root-node empty)
   (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false empty))
   "A new node should be created"))
;-------------------------------------------------------------------------;

; create-child-node : World -> World
; GIVEN: a world
; RETURNS: the same world with one child node added to the world
; EXAMPLE: (create-child-node (list (make-node HALF-CANVAS-WIDTH 
;                             HALF-SQUARE-LENGTH true (list root1-root3))))
;          => (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH true 
;                   (list root1-root2 root1-root3)))
; STRATEGY: HOFC

(define (create-child-node lon)
  (map
   ; Node -> Node
   ; GIVEN: a node
   ; RETURNS: a node with child node added to its parent node
   (lambda (n) (create-node n))
   lon))

; TESTS
(begin-for-test  
  ; Test for creating a new child
  (check-equal?
   (create-child-node 
    (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH true 
                     (list root1-root3))))
   (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH true 
                    (list root1-root2 root1-root3)))
   "A new child should be created in root1 node"))
;-------------------------------------------------------------------------;

; create-node : Node -> Node
; GIVEN: a node
; RETURNS: a child node added to the given node if it was selected 
; EXAMPLE: (create-node root1) => root1
; STRATEGY: Structural Decomposition on n : Node

(define (create-node n)
  (if (node-selected? n)
      (compliance-check n)
      (make-node (node-x-pos n) (node-y-pos n) (node-selected? n) 
                 (children-check (node-children n)))))

; TESTS
(begin-for-test  
  ; Test if node is not selected.
  (check-equal? (create-node root1) root1
                "If node is not selected, no new child should be created"))
;-------------------------------------------------------------------------;

; children-check : ListOfNode -> ListOfNode
; GIVEN: a node's children
; RETURNS: given children with a new child added or 
;          the given children if a new child was not added.
; EXAMPLE: (children-check (list root1-root2 root1-root3))
;           => (list root1-root2 root1-root3)
; STRATEGY: HOFC

(define (children-check lon)
  (map
   ; Node -> Node 
   ; GIVEN: a node
   ; RETURNS: a node with one more node added to it
   (lambda (child) (create-node child))
   lon))

; TESTS
(begin-for-test  
  ; Test if children are selected and create their children.
  (check-equal?
   (children-check (list root1-root2 root1-root3))
   (list root1-root2 root1-root3)
   "Return the list of children if not selected."))
;-------------------------------------------------------------------------;

; compliance-check : Node -> Node
; GIVEN: node
; RETURNS: a node with one node appended to its child node if the possible 
;          child node will be within the canvas otherwise return the given
;          node
; EXAMPLE: (compliance-check (make-node ZERO ZERO true empty)) =>
;           => (make-node ZERO ZERO true empty)
; STRATEGY: Structural Decomposition on t : Node

(define (compliance-check n)
  (if (child-x-pos-check n)
      (make-node (node-x-pos n) (node-y-pos n) (node-selected? n) 
       (cons (make-node (new-child-x-cord (node-x-pos n) (node-children n))
                        (+ (node-y-pos n) (* THREE SQUARE-LENGTH)) 
                        false empty) (node-children n))) 
      n))

; TESTS
(begin-for-test
  ; Test if node has no children
  (check-equal?
   (compliance-check (make-node ZERO ZERO true empty))
   (make-node ZERO ZERO true empty)
   "The given node should be returned if it has no children."))
;-------------------------------------------------------------------------;

; child-x-pos-check : Node -> Boolean
; GIVEN: a node
; RETURNS: the possible new child's position on canvas 
;          - if given node is empty then HALF-SQUARE-LENGTH 
;          - else difference between the left most 
;            son and HALF-SQUARE-LENGTH
; EXAMPLE: (child-x-pos-check root1-root2) => true
; STRATEGY: Structural Decomposition on n : Node 

(define (child-x-pos-check n)
  (if (empty? (node-children n)) 
      (>= (node-x-pos n) HALF-SQUARE-LENGTH)
      (>= (new-child-x-cord (node-x-pos n) (node-children n))
          HALF-SQUARE-LENGTH)))

; TESTS
(begin-for-test  
  ; Test if parent has no children but it is in range to create a child.
  (check-equal?
   (child-x-pos-check root1-root2) true
   "Root1-root2 node has space to accomodate a child node.")) 

;-------------------------------------------------------------------------;
;                      DELETE SELECTED NODE                               ;
;-------------------------------------------------------------------------;

; selected-node-delete : World -> World
; GIVEN: a world
; RETURNS: the same world after removing the selected node and its subtree
; EXAMPLE: (selected-node-delete (list (make-node 40 40 true empty)))
;           => emtpy
; STRATEGY: HOFC

(define (selected-node-delete lon)
  (map
   ;Node -> Node
   ;GIVEN: a node
   ;RETURN: a node that is not selected
   (lambda (n) (rest-of-node-create n)) 
   (unselected-node-filter lon))) 

; TESTS
(begin-for-test
  ; Test for deletion of nodes from the world.
  (check-equal?
   (selected-node-delete 
    (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false
                      (list root1-root2 (make-node 200 70 true empty)))))
   (list (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false
                    (list root1-root2)))
   "World is empty after deleting root1 node"))                         
;-------------------------------------------------------------------------;

; unselected-node-filter : World -> World
; GIVEN: a World
; RETURNS: the world after removing the selected nodes and its subtrees
; EXAMPLE: (unselected-node-filter     
;      (list root1 (make-node HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT true empty)))
;      => (list root1)
; STRATEGY: Structural Decomposition on n : Node

(define (unselected-node-filter lon)
  (filter 
   ; Node -> Node
   ;GIVEN: a node
   ;RETURN: a node that is not selected
   (lambda(n) (not(node-selected? n))) 
   lon))

; TESTS
(begin-for-test  
  ; Test for filtering all unselected nodes
  (check-equal?
   (unselected-node-filter     
    (list root1 (make-node HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT true empty)))
    (list root1)
    "All unselected nodes should be filtered out."))    
;-------------------------------------------------------------------------;

; rest-of-node-create : Node -> Node
; GIVEN: a node
; RETURNS: a node
; EXAMPLE: (rest-of-node-create (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH 
;                       false (list root1-root2 (make-node 200 70 true empty))))
;           => (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false 
;               (list root1-root2)) 
; STRATEGY: Structural Decomposition on n : Node

(define (rest-of-node-create n)
  (make-node (node-x-pos n) (node-y-pos n) (node-selected? n)
             (selected-node-delete (node-children n))))

; TESTS
(begin-for-test  
  ; Test for checking if the children of the nodes are selected and 
  ; are deleted.
  (check-equal?
   (rest-of-node-create (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false
                        (list root1-root2 (make-node 200 70 true empty))))
   (make-node HALF-CANVAS-WIDTH HALF-SQUARE-LENGTH false (list root1-root2))
   "Node with its selected children deleted."))

;-------------------------------------------------------------------------;
;                      REMOVE UPPER HALF                                  ;
;-------------------------------------------------------------------------;

; upper-half-node-remove : World -> World
; GIVEN: a world
; RETURNS: a world except for the nodes and its subtree which are on upper
;          half of the canvas
; EXAMPLE: (upper-half-node-remove (list root1 root2)) => (list root2)
; STRATEGY: HOFC

(define (upper-half-node-remove lon)
  (map
   ;Node -> Node
   ;GIVEN: a node
   ;RETURN: a node that is in lower half of the canvas
   (lambda(n) (rest-of-node-create n)) 
   (upper-half-nodes-filter lon))) 

; TESTS
(begin-for-test  
  ; Test for removing upper half nodes
  (check-equal?
   (upper-half-node-remove (list root1 root2)) (list root2))
   "root1 should be deleted")
;-------------------------------------------------------------------------;

; upper-half-nodes-filter : World -> World
; GIVEN: a world
; RETURNS: a world after removing the nodes and its subtree which are on upper
;          half of the canvas
; EXAMPLE: (upper-half-nodes-filter (list root1 root2)) => (list root2)
; STRATEGY: Structural Decomposition on n : Node

(define (upper-half-nodes-filter lon)
  (filter
   ;Node -> Node
   ;GIVEN: a ndoe
   ;RETURN: node if its in the upper half of canvas
   (lambda(n) (> (node-y-pos n) HALF-CANVAS-HEIGHT))
   lon))

; TESTS
(begin-for-test  
  ; Test for filtering out upper half nodes
  (check-equal?
   (upper-half-nodes-filter (list root1 root2))
   (list root2)
   "Filter out root1 as it is in upper half of the canvas.")) 


;-------------------------------------------------------------------------;
;                      OTHER REQUIRED FUNCTIONS                           ;
;-------------------------------------------------------------------------;

; required-x-cord : Node Real -> Real
; GIVEN: a node and a value
; RETURNS: the difference between the x-coordinate of the probable 
;          left most child of the node and the input integer
; EXAMPLE: (required-x-cord node-5 0) => 9
; STRATEGY: Structural Decomposition on n : Node

(define (required-x-cord n diff)
  (- (new-child-x-cord (node-x-pos n) (node-children n)) diff))

; TESTS
(begin-for-test
  (check-equal? (required-x-cord node-3 0) 60 
                "Test Failed for required-x-cord"))
;-------------------------------------------------------------------------;

; new-child-x-cord : Real Node -> Real
; GIVEN: a node x coordinate and its child node
; RETURNS: a node x coordinate if its child node is empty else returns the 
;          difference of left most son of the given node and two square length
; EXAMPLE: (new-child-x-cord 80 world-3) => 40
; STRATEGY: Function Composition

(define (new-child-x-cord parent-x-cord n)
  (if (empty? n)
      parent-x-cord
      (- (left-most-son-x-cord n) (* TWO SQUARE-LENGTH))))

; TESTS
(begin-for-test
  (check-equal? (new-child-x-cord 80 world-3) 40 
                "Test Failed for new-child-x-cord"))                
;-------------------------------------------------------------------------;

; left-most-son-x-cord : Node -> Real
; GIVEN: a node
; RETURNS: minimum of the x coordinates of the node's children
; EXAMPLE: (left-most-son-x-cord world-3) => 80
; STRATEGY: Structural Decomposition on t : Node

(define (left-most-son-x-cord n)
  (foldr
   ; Node Real -> Real
   ; GIVEN: a node and the canvas width
   ; RETURNS: minimum of the x coordinates of the node's children
   (lambda (child width) (min (node-x-pos child) width))
   CANVAS-WIDTH
   n))

; TESTS
(begin-for-test
  (check-equal? (left-most-son-x-cord world-3) 80 
                "Test Failed for left-most-son-x-cord"))
;-------------------------------------------------------------------------;

; world-to-roots : World -> ListOfNode
; GIVEN: a World
; RETURNS: a list of all the root nodes in the given world.
; EXAMPLE: (world-to-roots world-1) => world-1
; STRATEGY: Function Composition

(define (world-to-roots w)
  w)

;TESTS
(begin-for-test
  (check-equal? (world-to-roots world-1) world-1 
                "Test Failed for world-to-roots"))
;-------------------------------------------------------------------------;

; node-to-center : Node -> Posn
; GIVEN: a node
; RETURNS: the center of the given node as it is to be displayed on the scene
; EXAMPLE: (node-to-center node-1) => (make-posn 100 30)
; STRATEGY: Structural Decomposition on n : Node

(define (node-to-center n)
  (make-posn (node-x-pos n) (node-y-pos n)))

;TESTS
(begin-for-test
  (check-equal? (node-to-center node-1) (make-posn 100 30) 
                "Test Failed for node-to-center"))
;-------------------------------------------------------------------------;

; node-to-sons : Node -> ListOfNode
; GIVEN: a node
; RETURNS: given node's children
; EXAMPLE: (node-to-sons node-2) => (make-node 100 60 false empty)
; STRATEGY: Structural Decomposition on n : Node

(define (node-to-sons n)
  (node-children n))

;TESTS
(begin-for-test
  (check-equal? (node-to-sons node-2) (make-node 100 60 false empty) 
                "Test Failed for node-to-sons"))
;-------------------------------------------------------------------------;

; node-to-selected? : Node -> Boolean
; GIVEN: a node
; RETURNS: true if node is selected else false
; EXAMPLE: (node-selected? node-1) => false
; STRATEGY: Structural Decomposition on n : Node

(define (node-to-selected? n)
  (node-selected? n))

; TESTS
(begin-for-test
  (check-equal? (node-to-selected? node-1) false
                "Test Failed for node-to-selected?"))
;-------------------------------------------------------------------------;