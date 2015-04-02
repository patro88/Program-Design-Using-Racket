;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; requires
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

; provides
(provide path)

;-------------------------------------------------------------------------;
;                              ALGORITHM
;-------------------------------------------------------------------------;

; 1. Define the minimum boundary of the chessboard in which the robot
;    can reach the target position. The boundary is calculated by searching 
;    for the maximum X or Y positions in the given list of blocks, starting 
;    position and the target position.

; 2. A State consists of the position on the chessboard and the list of 
;    moves required to get there from the starting position.
;    Initial state of the robot will have its starting position and 
;    the list of moves taken to get there which is empty initially.

;    State => ( Position of the robot , List of moves to get to this position
;                                       from the starting position)

; 3. The robot can move in four cardinal directions which are defined as 
;    the successor states of the robot.
;    
;    3.1 Successor states consists of the new positions which the robot has
;        not yet explored and the list of moves taken to get to the new
;        positons.
;    3.1 Successor states consists of new positions which are part of
;        defined boundary of the chessboard and not present in the 
;        list of blocks.

; 4. The robot moves to one of the position's (one of the succesor states),and 
;    continues exploring the chessboard from its new position. 

; 5. If the succesor states generated consists of a state with the target 
;    position , then the algorithm halts and returns the list of moves taken 
;    to get to that target position.
;    Otherwise , if there are no new successor states, there is no possible 
;    path to the target position from the start state.


;-------------------------------------------------------------------------;
;                            GLOBAL CONSTANTS
;-------------------------------------------------------------------------;

(define ZERO 0) 
(define ONE 1)
(define NORTH "north")
(define EAST "east")
(define WEST "west")
(define SOUTH "south")

;-------------------------------------------------------------------------;
;                            DATA DEFINITIONS
;-------------------------------------------------------------------------;

; a PosInt is a positive integer greater than 0

; A Position is a (list PosInt PosInt)
; (x y) represents the position at position x, y on the chessboard
; where x increases on going right and y increases on going down

; TEMPLATE
; pos-fn : Position -> ??
;(define (pos-fn p)
;  (...
;   (first p) (second p)))

;-------------------------------------------------------------------------;

; A ListOf<PosInt> is one of 
; -- (cons PosInt empty) 
; -- (cons PosInt ListOf<PosInt>)
; Interp : a non-empty list of positive integers

; TEMPLATE 
; lst-pos-ints-fn : ListOf<PosInt> -> ??
;(define (lst-pos-ints-fn lst)
;  (cond
;    [(empty? (rest lst)) ...]
;    [else (...
;           (first lst)
;           (lst-pos-ints-fn (rest lst)))]))

;-------------------------------------------------------------------------;

; A ListOf<Position> is either
; -- empty 
; -- (cons Position ListOf<Position>)
; Interp: a list of positions can either be empty or a position
;         and another list of positions that represent a list of 
;         (x,y) coordinates on the chessboard.

; TEMPLATE
; lst-pos-fn : ListOf<Position> -> ??
;(define (lst-pos-fn positions)
;  (cond
;    [(empty? positions) empty]
;    [else (...
;           (pos-fn (first positions))
;           (lst-pos-fn (rest positions)))]))

;-------------------------------------------------------------------------;

; A Move is a (list Direction PosInt)
; Interp: a move of the specified number of steps in the indicated
;         direction.

; TEMPLATE
; move-fn : Move -> ??
;(define (move-fn m)
;  (...(first m) 
;      (second m)))

;-------------------------------------------------------------------------;

; A Direction is one of 
; -- "north"
; -- "east"
; -- "south"
; -- "west"
; Interp: Four cardinal directions in which the robot can move on the 
;         chessboard

; TEMPLATE
; dir-fn : Direction -> ??
;(define (dir-fn dir)
;  (cond
;    [(string=? dir "north") ...]
;    [(string=? dir "south") ...]
;    [(string=? dir "east") ...]
;    [(string=? dir "west") ...]))

;-------------------------------------------------------------------------;

; A ListOf<Move> is one of 
; -- empty
; -- (cons Move ListOf<Move>)
; Interp : a list of moves is either empty or 
;          a move and a list of moves.

; TEMPLATE
; lst-fn : ListOf<Move> -> ??
;(define (lst-fn lst)
;  (cond
;    [(empty? lst) empty]
;    [else (... (first lst) (rest lst))]))

;-------------------------------------------------------------------------;

; A Plan is a ListOf<Move>
; WHERE: the list does not contain two consecutive moves in the same 
;        direction

;-------------------------------------------------------------------------;

; A Maybe<Plan> is one of 
; -- false
; -- Plan
; Interp : a maybe<Plan> can be either false 
;          or a Plan

; TEMPLATE
;(define (maybeplan-fn mbp)
;  (cond
;    [(false? mbp) ...]
;    [(list? mbp) ...]))

;-------------------------------------------------------------------------;

(define-struct state (cur-pos moves))
; A State is a (make-state Position ListOf<Move>)
; Interp:
;   cur-pos = current position on the chessboard
;   moves   = list of blocks covered by the robot to get 
;             to the current position

; TEMPLATE
;(define (state-fn s)
;  (...
;   (pos-fn (state-cur-pos s))
;   (lst-fn (state-moves s))))

;-------------------------------------------------------------------------;

; A ListOf<State> is one of 
; -- empty
; -- (cons State ListOf<State>)
; Interp: a list of states is either empty or 
;         a state with other list of states.

; TEMPLATE
; lst-of-states-fn : ListOf<State> -> ??
;(define (lst-of-states-fn lst)
;  (cond
;    [(empty? lst) ...]
;    [else (...
;           (state-fn (first lst))
;           (lst-of-states-fn (rest lst)))]))

;-------------------------------------------------------------------------;
;                          FUNCTION DEFINITIONS
;-------------------------------------------------------------------------;

; path : Position Position ListOf<Position> -> Maybe<Plan>
; GIVEN: 
;       1. the starting position of the robot
;       2. the target position that robot is supposed to reach
;       3. A list of the blocks which are occupied on the board
; RETURNS: a plan that, when executed, will take the robot from
;          the starting position to the target position without passing 
;          over any of the blocks, or false if no such sequence of move
;          exists.
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (path start goal blocks)
  (plan-or-false (search-graph (list (make-state start empty))
                               (list (make-state start empty))
        goal (build-graph start goal blocks) blocks)))

; Constants for tests
(define path-start (list 1 1))
(define path-goal (list 2 2))
(define path-blocks empty)  
(define path-result (list (list "east" 1) (list "south" 1)))

; TESTS: 
(begin-for-test
   
  ; Test if the correct path has been returned.
  (check-equal? 
   (path path-start path-goal path-blocks) path-result
   "Incorrect path returned"))

;-------------------------------------------------------------------------;

; plan-or-false : Maybe<Plan> -> Maybe<Plan>
; GIVEN: a plan if the robot can reach the target position or false if robot
;        cannot reach its target position.
; RETURNS: false if the robot cannot reach its target position
;          else plan.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on mbp : Maybe<Plan>

(define (plan-or-false mbp)
  (cond
    [(false? mbp) mbp] 
    [(list? mbp) (reverse mbp)]))

; Constants for tests
(define plan-or-false-input (list (list "east" 1) (list "south" 1)))
(define plan-or-false-output (list (list "south" 1) (list "east" 1)))

; TESTS:
(begin-for-test
  
  ; Test if false is returned
  (check-equal? (plan-or-false false) false
                "False should be returned as no plan exits.")
  
  ; Test if plan is returned
  (check-equal? (plan-or-false plan-or-false-input)
                plan-or-false-output
                "A Plan should be returned."))

;-------------------------------------------------------------------------;

; build-graph : Position Position ListOf<Position> -> ListOf<Position>
; GIVEN: 
;       1. starting position of the robot
;       2. target position that robot is supposed to reach
;       3. list of the blocks on the board
; RETURNS: a list of positions which represents the chessboard in which the 
;          robot can move in order to reach its target position
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (build-graph start goal blocks)
  (make-graph (board-list start goal blocks) (board-list start goal blocks)))
 
; TESTS: 
(begin-for-test
  
  ; Test for the correct graph created with the given start , goal
  ; and list of blocks .
  (check-equal? (build-graph start1 goal1 blocks1) get-list1
                "Incorrect graph created."))

;-------------------------------------------------------------------------;

; board-list : Position Position ListOf<Position> -> ListOf<PosInt>
; GIVEN: 
;       1. Starting position of the robot on the chessboard
;       2. Target position of the robot on the chessboard
;       3. A List of positions which represents occupied blocks on the 
;          chessboard.
; RETURNS: a list of positive integers where each element of the list 
;          represents the row and coloumn number of the chessboard
;          in which the robot can move in order to reach its target position.
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (board-list start goal blocks)
  (build-list 
   (+ ONE (max (max-pos first (append (list start) (list goal) blocks))
               (max-pos second (append (list start) (list goal) blocks))))
   add1))

; Constants for tests
(define start1 (list 1 1))
(define goal1 (list 3 3))
(define blocks1 (list (list 2 2) (list 1 1) (list 2 1)))
(define get-list2 (list 1 2 3 4)) 

; TESTS:
(begin-for-test
  
  ; Test for getting the correct list 
  (check-equal? (board-list start1 goal1 blocks1) get-list2
                "Incorrect list created.")) 

;-------------------------------------------------------------------------;

; make-graph : ListOf<PosInt> ListOf<PosInt> -> ListOf<Position>
; GIVEN: a list of positive integers where each element of the list 
;        represents the row and coloumn number of the chessboard.
; RETURNS: the cartesian product of lists l1 and l2 which represents 
;          each block on the chessboard.
; EXAMPLE: see tests below
; STRATEGY: HOFC

(define (make-graph l1 l2)
  (foldr
   append
   empty
   (map
    ; PosInt -> ListOf<Position>
    ; GIVEN: a positive integer
    ; RETURNS: a list of positions 
    ;          on the chessboard
    (lambda (e1) (map
                  ; PosInt -> Position
                  ; GIVEN: a postive integer
                  ; RETURNS: a list of positions on
                  ;          the chessboard
                  (lambda (e2) (list e1 e2))
                  l2))
    l1)))

; Constants for tests
(define get-list1 (list (list 1 1) (list 1 2) (list 1 3) (list 1 4) 
                        (list 2 1) (list 2 2) (list 2 3) (list 2 4)
                        (list 3 1) (list 3 2) (list 3 3) (list 3 4)
                        (list 4 1) (list 4 2) (list 4 3) (list 4 4)))

; TESTS:
(begin-for-test
  
  ; Test for gettin the correct list of positions
  (check-equal? (make-graph get-list2 get-list2) get-list1
                "Incorrect list of positions created.")) 

;-------------------------------------------------------------------------;

; max-pos : (Position -> PosInt) ListOf<Position> -> PosInt
; GIVEN:
;       1. a function which is applied to every element of the given list 
;          of positions
;       2. a list of positions which consists of the blocks occupied 
;          on the chessboard , the starting position of the robot 
;          and the target position of the robot.  
; RETURNS: a positive integer which represents the row and coloumn 
;          number for the chessboard in which the robot can reach 
;          the target
; EXAMPLE: see tests below
; STRATEGY: HOFC

(define (max-pos fn positions)
  (fn (foldr
       ; Position Position -> Position
       ; GIVEN: two positions on the chessboard
       ; RETURNS: the position with the maximum x
       ;          or y coordinate in the given
       ;          positions
       (lambda (f l) (list (max (fn f) (fn l))
                           (max (fn f) (fn l)))) 
       (list ZERO ZERO)
       positions)))

; Constants for tests
(define positions1 (list (list 1 3) (list 100 200)))
(define max1 100)
(define max2 200)

; TESTS:
(begin-for-test
  
  ; Test for returning the maximum X coordinate
  (check-equal? (max-pos first positions1) max1)
  "Incorrect maximum x coordinate in the given list of positions."
  
  ; Test for returning the maximum Y coordinate
  (check-equal? (max-pos second positions1) max2
  "Incorrect maximum y coordinate in the given list of positions."))

;-------------------------------------------------------------------------;

; search-graph : ListOf<State> ListOf<State> Position ListOf<Position>  
;                                                  ListOf<Position> -> Plan
; GIVEN:
;      1. list of states (lst-new) 
;      2. list of states (lst-exp)
;      3. target position where the robot has to reach.
;      4. minimum list of positions which defines the boundary of the 
;         chessboard ,which the robot requires to reach its target position.
;      5. list of positions which are occupied on the chessboard
; WHERE: 
;      1. lst-new is the list of states which consists of the 
;         1.1 present state of the robot
;         1.2 And the list of states which the robot generated 
;             and is yet to visit in all its previous positions 
;             on the chessboard.
;      2. lst-exp consists of all the states that the robot has visited. 
; RETURNS: 
; EXAMPLE: see tests below
; STRATEGY: General Recursion
; HALTING MEASURE: difference between list of positions defining the chessboard
;                  (excluding the list of blocks occupied)
;                  AND the positions present in the list of explored states.
;                  The above difference will be empty if the goal position
;                  cannot be reached.

(define (search-graph new-states exp-states goal graph blocks)
  (cond 
    [(found? goal new-states) (return-path-found goal new-states)]
    [(empty? new-states) false] 
    [else (local
            ((define candidates (remove-visited-states 
                                 (successors (first new-states) blocks graph)
                                 exp-states)))
            (cond
              [(empty? candidates) (search-graph (rest new-states)  
                                                 exp-states goal graph blocks)]
              [else (search-graph
                     (append candidates (rest new-states)) 
                     (cons (first new-states) exp-states) 
                     goal graph blocks)]))]))

; TERMINATION ARGUMENT: 
; At the recursive call where 'candidates' is non-empty , 'exp-states' 
; consists of one element greater than the old one as the robot enters new 
; positions of the chess board at each step.

; Otherwise, at the recursive call where 'candidates' is empty,
; 'new-states' decreases by one which ensures that 'exp-states'
; increases in subsequent recursive calls if there are positions 
; on the chessboard which the robot has not yet explored.

; Hence, 'exp-states' keeps on increasing by one which takes it nearer to 
; the goal position and it satisfies the halting measure.

; Constants for tests
(define search-graph-new-states1 (list (make-state (list 1 1) empty)))
(define search-graph-exp-states1 empty) 
(define search-graph-goal1 (list 4 4))
(define search-graph-blocks1 empty)
(define search-graph-blocks2 (list (list 2 1) (list 1 2) (list 2 2)))
(define search-graph1 get-list1)
(define search-graph-result (list
                             (list "south" 1)
                             (list "east" 3)
                             (list "south" 1)
                             (list "west" 3)
                             (list "south" 1)
                             (list "east" 3)))
    
; TESTS
(begin-for-test
  
  ; Test for the path returned to the goal
  (check-equal? (search-graph search-graph-new-states1 search-graph-exp-states1
                              search-graph-goal1 search-graph1 
                              search-graph-blocks1)
                search-graph-result
                "Incorrect path returned to the goal")
  
  ; Test for false if goal is not reachable
  (check-equal? (search-graph search-graph-new-states1 
                              search-graph-exp-states1 search-graph-goal1
                              search-graph1 search-graph-blocks2)
                false
                "False should be returned as target cannot be reached.")) 
 
;-------------------------------------------------------------------------;

; found? : Position ListOf<State> -> Boolean
; GIVEN: the target position and a list of states in which 
;        the target position might be present.
; RETURNS: true iff the given target position is present 
;          in the given list of states Else false
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on f : State

(define (found? goal states)
  (ormap
   ; State -> Boolean
   ; GIVEN: a state representing a position on the chessboard
   ; RETURNS: true iff the given state has the target position
   (lambda (f) (equal? (state-cur-pos f) goal))
   states)) 
   
; Constants for tests
(define found-goal (list 4 4))
(define found-states (list (make-state (list 4 4) empty)))

; TESTS: 
(begin-for-test
  
  ; Test if target position is present in the list of states
  (check-equal?
   (found? found-goal found-states) true
   "True should be returned as given target position is 
    present in the list of states."))  
 
;-------------------------------------------------------------------------;
; return-path-found : Position ListOf<State> -> ListOf<Move>
; GIVEN: a target position and a list of states in which the target 
;        position is present.
; RETURNS: a list of moves taken by the robot to reach the 
;          target position
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on f : State

(define (return-path-found goal new-states)
  (foldr
   ; State ListOf<Move> -> ListOf<Move>
   ; GIVEN: a state and list of moves required 
   ;        to get to the position in the given state.
   ; RETURNS: list of moves present in the given state 
   ;          if that state's position is equal to
   ;          that of the goal position
   (lambda (f l) (if (equal? goal (state-cur-pos f))
                     (state-moves f)
                     l))
   empty
   new-states))

; Constants for tests
(define goal10 (list 2 2))
(define new-states10 (list (make-state (list 2 2)
                                       (list (list "east" 1)
                                             (list "south" 1)))
                           (make-state (list 1 1)
                                       empty)))
(define path10 (list (list "east" 1) (list "south" 1))) 

; TESTS:
(begin-for-test
  
  ; Test if the correct path is being returned
  (check-equal?
   (return-path-found goal10 new-states10) path10
   "Incorrect path returned"))

;-------------------------------------------------------------------------;

; successors : State ListOf<Position> ListOf<Position> -> ListOf<State>
; GIVEN: 
;       1. a state which represents the position in which the robot
;          is currently in 
;       2. list of positions which represents the list of blocks 
;          which are occupied on the chessboard
;       3. list of positions which represents the chessboard blocks 
;          which the robot requires to find the target position
; RETURNS: list of states which represents the positions to 
;          which the robot can traverse to.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on s : State
 
(define (successors s blocks graph)
  (return-next-states 
   (state-cur-pos s)  
   (state-moves s) blocks graph)) 

; Constants for tests
(define state1 (make-state (list 1 1) empty))
(define successor1 (list (make-state (list 2 1) (list (list "east" 1)))
                         (make-state (list 1 2) (list (list "south" 1)))))
  
; TESTS:              
(begin-for-test
  
   ; Test to verify the correct successors for a state returned.
  (check-equal?
   (successors state1 empty get-list1) successor1
   "Incorrect successors returned."))
   
;-------------------------------------------------------------------------;

; return-next-states : Position ListOf<Move> ListOf<Position> ListOf<Position>
;                                              -> ListOf<State>
; GIVEN:
;       1. current position of the robot
;       2. the list of moves which the robot has taken to get to the current
;           position
;       3. list of positions which represents the list of blocks 
;          which are occupied on the chessboard
;       4. list of positions which represents the chessboard blocks in
;          which the robot requires to find the target position 
; RETURNS: list of states which represents the positions to 
;          which the robot can traverse to.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pos : Position

(define (return-next-states pos moves blocks graph)
  (append
   (in-graph blocks (list (+ ONE (first pos)) (second pos)) 
             (update-helper moves EAST) graph)
   (in-graph blocks (list (- (first pos) ONE) (second pos))
             (update-helper moves WEST) graph)
   (in-graph blocks (list (first pos) (+ ONE (second pos)))
             (update-helper moves SOUTH) graph)
   (in-graph blocks (list (first pos) (- (second pos) ONE)) 
             (update-helper moves NORTH) graph)))

; TESTS:      
(begin-for-test
  
  ; Test to verify the next states of the current state.
  (check-equal? (return-next-states (list 1 1) empty empty get-list1)
  successor1
  "Incorrect next states of the current state returned.")) 

;-------------------------------------------------------------------------;

; update-helper : ListOf<Move> Direction -> ListOf<Move>
; GIVEN: a list of moves and a direction
; RETURNS: updated list of moves for the next possible locations 
;          that the robot can move to.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on moves : ListOf<Move>

(define (update-helper moves dir)
  (cond
    [(empty? moves) (list (list dir ONE))]
    [else (update (first moves) dir (rest moves))]))

; Constants for tests
(define moves1 (list (list "east" 1)))
(define dir1 "east")
(define moves1-output (list (list "east" 2)))

; TESTS:
(begin-for-test
  
  ; Test for checking if update returns the correct updated moves of the robot
  (check-equal?
   (update-helper moves1 dir1) moves1-output
   "Incorrect move sequence updated"))  
 
;-------------------------------------------------------------------------;

; update : Move Direction ListOf<Move> -> ListOf<Move>
; GIVEN:   previous move to get to the current position of the robot
;          on the chessboard, previous direction and the list of moves
;          excluding the input move
; RETURNS: a list of updated moves after the robot moves a step 
;          in the given direction
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on move : Move

(define (update move dir rm)
  (if (string=? (first move) dir)
      (append (list (list dir (+ ONE (second move)))) rm)
      (append (list (list dir ONE)) (cons move rm))))            

; Constants for tests
(define move1 (list "east" 1))
(define dir2 "east")
(define rm1 empty)
(define moves1-output1 (list (list "east" 2)))

; TESTS:
(begin-for-test
  
  ; Test for checking if update returns the correct updated moves of the robot
  (check-equal?
   (update move1 dir2 rm1) moves1-output1
   "Incorrect move sequence updated"))

;-------------------------------------------------------------------------;

; in-graph : ListOf<Position> Position ListOf<Move> ListOf<Position>
;                                                -> ListOf<State>
; GIVEN:
;       1. list of positions which represents the list of blocks 
;          which are occupied on the chessboard
;       2. next position which the robot can go to.
;       3. list of moves which represents the moves required
;          for the robot to get to the given position
;       4. list of positions which represents the chessboard blocks in
;          which the robot requires to find the target position
; RETURNS: list of state which represents the position to which
;          the robot has moved if its is present in the given
;          list of positions reprenting the chessboard
;          and it is not present in the given list of positions 
;          representing the blocks occupied on the chessboard
;          Else empty
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (in-graph blocks pos newmoves graph)
  (if (and (pos-in-graph? pos graph) (not (my-member? pos blocks))) 
       (list (make-state pos newmoves))
       empty))

; Constants for tests
(define in-graph-pos (list 1 2))
(define in-graph-moves (list (list "east" 1)))
(define in-graph-output (list (make-state (list 1 2) in-graph-moves)))

; TESTS:
(begin-for-test
  
  ; Test for correct next state generation for the robot
  (check-equal?
   (in-graph empty in-graph-pos in-graph-moves get-list1)
   in-graph-output
   "Incorrect state representation the next move"))

;-------------------------------------------------------------------------;

; pos-in-graph? : Position ListOf<Position> -> Boolean 
; GIVEN: a position to which the robot can go to.
;        and a list of positions which represents the chessboard blocks in
;        which the robot requires to find the target position
; RETURNS: true iff the given position is present in the given list 
;          of positions. 
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pos : Position

(define (pos-in-graph? pos graph)
  (and (and (<= (first pos) (sqrt (length graph)))
            (> (first pos) ZERO))
       (and (<= (second pos) (sqrt (length graph)))
            (> (second pos) ZERO))))
  
; Constants for tests
(define pos11 (list 2 2))

; TESTS:
(begin-for-test
  
  ; Test for true when given position is inside the graph
  (check-equal?
   (pos-in-graph? pos11 get-list1) true
   "given position is not present in the given list of positions."))

;-------------------------------------------------------------------------;
; remove-visited-states : ListOf<State> ListOf<State> -> ListOf<State>
; GIVEN: a list of new-states explored by the robot which includes 
;        the explored states and the list of explored states by the robot.
; RETURNS: a list of states unexplored by the robot 
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on x : State

(define (remove-visited-states new-states exp-states)
  (filter
   ; State -> Boolean
   ; GIVEN: a state in the list of new states which 
   ;        represents positions yet to explore and 
   ;        the explored states
   ; RETURNS: false if the position in the given state 
   ;          is present in the given list of explored states
   (lambda (x) (not (present-in? (state-cur-pos x) exp-states)))
   new-states))
 
; Constants for tests
(define new-states2 (list (make-state (list 1 1) empty)
                          (make-state (list 2 2) (list (list "east" 1)))))
(define exp-states2 (list (make-state (list 2 2) empty))) 

; TESTS: 
(begin-for-test 
  
  ; Test for correct unexplored state returned
  (check-equal? (remove-visited-states new-states2 exp-states2)
                (list (make-state (list 1 1) empty)) 
                "Incorrect set returned")) 
 
;-------------------------------------------------------------------------;

; present-in? : Position ListOf<State> -> Boolean
; GIVEN: a position of the robot
; RETURNS:  true iff the position in the given state is present in the
;           list of states given Else false
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on x : State

(define (present-in? pos exp-states)
  (ormap
   ; State -> Boolean
   ; GIVEN: a state from the list of given 
   ;        states 
   ; RETURNS: true iff the position in the 
   ;          given state is equal to the given
   ;          position, Else False
   (lambda (x) (equal? pos (state-cur-pos x)))
   exp-states))
  
; Constants for tests
(define pos12 (list 2 2))
(define states12 (list (make-state (list 2 2) empty)
                       (make-state (list 1 1) empty)))

; TESTS: 
(begin-for-test
  
  ; Test if true is returned for a position present in the list of states
  (check-equal?
   (present-in? pos12 states12)
   true
   "true should be returned as the given position is present in the
    list of states"))
;-------------------------------------------------------------------------;
