;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname obstacles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

; require
(require "extras.rkt")
(require "sets.rkt")
(require rackunit)

; providing functions
(provide position-set-equal?)
(provide obstacle?)
(provide blocks-to-obstacles)

;----------------------------------------------------------------------------;
;                              CONSTANTS                                     ;
;----------------------------------------------------------------------------;

; integer constant
(define ONE 1)

;----------------------------------------------------------------------------;
;                             DATA DEFINITIONS                               ;
;----------------------------------------------------------------------------;

; PosInt means positive integer. i.e. integer greater than zero.

; A Position is a (list PosInt PosInt)

; INTERPRETATION:
; (x y) represents the position x, y 
;
; WHERE: x is the first element of the list which represents column and y is  
;        the second element of the list which represents rows in a chessboard
;        where x increases on going right and y increases on going down.

; TEMPLATE:
; pos-fn: Position -> ???
;(define (pos-fn p)
;  (... (first p) (second p)))

;----------------------------------------------------------------------------;

; A PositionSet (PS) is a list of positions without duplication.
;
; PositionSet (PS) is one of the following
; -- empty                       interp: PositionSet is empty
; -- (cons Position PositionSet) interp: it represents  list with first 
;                                        element as Position and the other
;                                        element as PositionSet (PS) i.e.
;                                        (cons Position PS)
; TEMPLATE:
; pos-set-fn: PositionSet -> ???
;(define (pos-set-fn ps)
;  (cond
;    [(empty? ps) ...]
;    [else (... (pos-fn (first ps))
;                (pos-set-fn (rest ps)))]))

;----------------------------------------------------------------------------;

; A PositionSetSet (PSS) is a list of PositionSets without duplication, that
; is, no two position-sets denote the same set of positions.
;
; PositionSetSet (PSS) is one of the following
; -- empty                             interp: PositionSetSet is empty
; -- (cons PositionSet PositionSetSet) interp: it represents list with first
;                                              element as PS and other as
;                                              PositionSetSet (PSS)
;
; TEMPLATE:
; pos-set-set-fn: PositionSetSet -> ???
;(define (pos-set-set-fn pss)
;  (cond
;    [(empty? pss) ...]
;    [else (...(pos-set-fn (first pss))
;               (pos-set-set-fn (rest pss)))]))

;----------------------------------------------------------------------------;
;                            END DATA DEFINITIONS                            ;
;----------------------------------------------------------------------------;



;----------------------------------------------------------------------------;
;                            FUNCTION DEFINITIONS                            ;
;----------------------------------------------------------------------------;

; position-set-equal? : PositionSet PositionSet -> Boolean
; GIVEN: two position sets
; RETURNS: true iff they denote the same set of positions.
; EXAMPLES: (position-set-equal? position-set1 position-set2) => true
; STRATEGY: Function Composition

(define (position-set-equal? ps1 ps2)
  (if (= (length ps1) (length ps2))
      (set-equal? ps1 ps2)
      false))

; TESTS
; Constants for Tests
(define position-set1 (list (list 1 2) (list 3 4) (list 5 6)))
(define position-set2 (list (list 5 6) (list 3 4) (list 1 2)))
(define position-set3 (list (list 1 2)
                            (list 3 4)))

(begin-for-test  
  ; Test if two position sets are equal
  (check-equal? (position-set-equal? position-set1 position-set2) true
                "true should be returned as both position sets are equal.")
  
  ; Test if given position sets are unequal
  (check-equal? (position-set-equal? position-set3 position-set1) false
                "false should be returned as given position sets are distinct"))
;----------------------------------------------------------------------------;

; obstacle? : PositionSet -> Boolean
; GIVEN: a position set
; RETURNS: true iff the set of positions would be an obstacle if they
;          were all occupied and all other positions were vacant.
; EXAMPLES: (obstacle? pos-set1) => true
; STRATEGY: Function Composition

(define (obstacle? ps)
  (if (empty? ps) 
      false 
      (equal? (length (blocks-to-obstacles ps)) ONE)))

; TESTS
; Constants for tests
(define pos-set1 (list (list 1 1) (list 2 2) (list 3 3)))
(define pos-set2 (list (list 1 1) (list 2 2) (list 4 1))) 

(begin-for-test  
  ; Test if given empty position set 
  (check-equal? 
   (obstacle? empty) false
   "Return false if given empty")
  ; Test if given positionset is an obstacle
  (check-equal? 
   (obstacle? pos-set1) true
   "true should be returned as given position set is an obstacle")
  ; Test if given position set is not an obstacle
  (check-equal? 
   (obstacle? pos-set2) false
   "false should be returned as given position set is not an obstacle"))
;----------------------------------------------------------------------------;

; blocks-to-obstacles : PositionSet -> PositionSetSet
; GIVEN: the set of occupied positions on some chessboard
; WHERE: the occupied positions are yet to be seen.
; RETURNS: the set of obstacles on that chessboard.
; EXAMPLES: (blocks-to-obstacles pos-set11) => pos-set-set11
; STRATEGY: General Recursion
; HALTING MEASURE: is the ps positionset which keeps on decreasing on each 
;                  recursive call
; TERMINATION-ARGUMENT: At the recursive call, for position, all its adjacent
;                       positions are calculated using block-reachable-from
;                       function. Hence the result of set difference of ps and
;                       block-reachable-from is at least one element smaller 
;                       than 'ps'. So, the halting measure decreases.
(define (blocks-to-obstacles ps)
  (cond
    [(empty? ps) empty]
    [else (cons
           (block-reachable-from (list (first ps)) ps)
           (blocks-to-obstacles 
            (set-difference ps (block-reachable-from (list (first ps)) ps))))]))

; TESTS
; Constants for tests
(define pos-set11 (list (list 1 1) (list 3 3)))
(define pos-set-set11 (list (list (list 1 1))
                            (list (list 3 3 ))))

(begin-for-test  
  ; Test for returning the set of all obstacles in the given position set
  (check-equal? 
   (blocks-to-obstacles pos-set11) pos-set-set11
   "Incorrect obstacles for the given position set returned.")  
  ; Test if the given position set is empty
  (check-equal? 
   (blocks-to-obstacles empty) empty
   "Test Failed, An empty list should be returned."))
;----------------------------------------------------------------------------;

; block-reachable-from : PositionSet PositionSet -> PositionSet
; GIVEN: two position sets 
;   1. First position set will contain a list of single occupied position. 
;      Then on each recursive call, the list will contains union of its
;      immediate successors or adjacent nodes.
;   2. Second positionset is the set of occupied positions on some chessboard
; WHERE: first positionset is the subset of second positionset
; RETURNS: all the adjacent positions of all the given positionset
;          (first input) within the given positionset (second input)
; EXAMPLES: (block-reachable-from (list (list 1 1)) pos-set5) =>
;           (list (list 2 2) (list 1 1))
; STRATEGY: General Recursion
; HALTING MEASURE: is the set of overall-ps positionset NOT in ps positionset
; TERMINATION-ARGUMENT: At the recursive call, candidates contains atleast 
;                       one element that is not in 'ps' (otherwise the subset?
;                       test would have returned true). Hence the result of 
;                       set union is at least one element bigger than 'ps'.
;                       So, the halting measure decreases. 
(define (block-reachable-from ps overall-ps)
  (local
    ((define candidates (all-successors ps overall-ps)))
    (cond
      [(subset? candidates ps) ps]
      [else (block-reachable-from (set-union candidates ps) overall-ps)])))

; TESTS
(begin-for-test  
  ; Test if for a given positionset, all the reacheables are correct 
  (check-equal? 
   (block-reachable-from (list (list 1 1)) pos-set5) 
   (list (list 2 2) (list 1 1))
   "Test Failed, Should return the list of position (list 2 2) and (list 1 1)"))
;----------------------------------------------------------------------------;

; all-successors : Positionset PositionSet -> PositionSet
; GIVEN: two positionset. First positionset contains a set of positions whose
;        adjacent positions are to be determined within second given positionset
; RETURNS: all the adjacent positions of the first positionset within the second
;          positionset
; EXAMPLES: (all-successors (list (list 1 1)) pos-set5) => (list (list 2 2))
; STRATEGY: HOFC

(define (all-successors ps overall-ps)
  (foldr
   ; Position PositionSet -> PositionSet
   ; GIVEN: a position and a position set which represents
   ;        set-union of the given position and its successors
   ; RETURNS: position set with all the unique positions in it due to union
   (lambda (p x) (set-union (successors p overall-ps) x))
   empty
   ps))

; TESTS
(begin-for-test  
  ; Test if adjacent positions is correct 
  (check-equal? 
   (all-successors (list (list 1 1)) pos-set5) (list (list 2 2))
   "Test Failed, Should return the list of position (list 2 2)"))
;------------------------------------------------------------------------------;

; successors : Position PositionSet -> PositionSet
; GIVEN: a position whose adjacent positions are to be found in the given
;        positionset
; RETURNS: all the adjacent positions of the given position within the given
;          position set
; EXAMPLES: (successors (list 1 1) pos-set5) => (list (list 2 2))
; STRATEGY: HOFC

(define (successors p overall-ps)
  (filter
   ; Position -> Boolean
   ; GIVEN: a position
   ; RETURNS: true iff the given position's adjacent positions are 
   ;          present in the overall-ps list.
   (lambda (x) (local
                 ; Position Position -> Boolean
                 ; GIVEN: two positions on the chessboard      
                 ; RETURNS: true iff both position are adjacent to each other
                 ;          else false
                 ; EXAMPLE: (adjacent-position? (list 1 2) (list 2 3)) => true
                 ((define (adjacent-position? p1 p2)
                    (and (= (abs (- (first p1) (first p2))) ONE)
                         (= (abs (- (second p1) (second p2))) ONE))))
                 (adjacent-position? p x)))
   overall-ps))

; TESTS
; Constants for tests
(define pos-set5 (list (list 1 1) (list 2 2) (list 3 2)))

(begin-for-test  
  ; Test if adjacent position is correct 
  (check-equal? 
   (successors (list 1 1) pos-set5) (list (list 2 2))
   "Test Failed, Should return the list of position (list 2 2)"))
;------------------------------------------------------------------------------;

; set-difference :  PositionSet PositionSet -> PositionSet
; GIVEN:  position set ps1 and position set ps2
; RETURNS: a position set which contains all the elements of ps1 not present
;          in ps2
; EXAMPLES: (set-difference pos-set1 pos-set2) => (list (list 3 3))
; STRATEGY: HOFC

(define (set-difference ps1 ps2)
  (filter
   ; Position -> Boolean
   ; GIVEN: a position
   ; RETURNS: true iff the given position is not present in the list ps2
   (lambda (p) (not (my-member? p ps2)))
   ps1))

; TESTS
(begin-for-test  
  ; Test if given two positionset difference is correct
  (check-equal? 
   (set-difference pos-set1 pos-set2) (list (list 3 3))
   "Test Failed, Should return the set differnce"))
;------------------------------------------------------------------------------;