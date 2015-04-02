;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pitchers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;----------------------------------------------------------------------------;
;                          FILE NAME: pitchers.rkt                           ;
;----------------------------------------------------------------------------;



;----------------------------------------------------------------------------;
;                          INFORMATION ANALYSIS                              ;
;----------------------------------------------------------------------------;
; Our solution to this problem uses the folowing data definitions-
; Pitcher, PitchersExternalRep, ListOfPitchersExternalRep, Move, ListOf<Move>,
; Maybe<ListOf<Move>>, PitchersInternalRep, ListOfPitchersInternalRep, 
; NEListOf<PosInt>, NonNegInt, PosInt

; Our solution uses the Depth First Search (DFS) algorithm to find a
; Maybe<ListOf<Move>> for the given list of capacities. The trivial cases are
; when the first pitcher has the required goal, when the goal is greater than
; the all of the given capacities and when there is only 1 pitcher but goal is
; less than its capacity. The non-trivial problem is divided 
; into smaller problems by using General Recursion. For the non-trivial case 
; the program will check for all possible configurations of the given list of
; pitchers. We use the PitchersInternalRep data defintion to store the
; configurations of the given list of pitchers and the list of moves from the
; initial configuration. Once this is done, the PitchersInternalRep instance
; which has a pitcher capacity that matches with the goal is searched in the
; 'frontier' set. The ListOf<Move> for the matched pitchersinternalrep is 
; given as solution for the problem. If there is no instance which matches the
; goal in the 'frontier' set then the programs outputs false as the solution.

; NOTE - Initially had implemented this solution using BFS, but later changed
; it to DFS to improve the performance of the solution.
;----------------------------------------------------------------------------;


; require extras.rkt, sets.rkt and rackunit
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

; provide functions
(provide list-to-pitchers)
(provide pitchers-to-list)
(provide pitchers-after-moves)
(provide make-move)
(provide move-src)
(provide move-tgt)
(provide solution)
(provide move?)

;----------------------------------------------------------------------------;
;                            DATA DEFINITIONS                                ;
;----------------------------------------------------------------------------;

; A NonNegInt is an interger greater than or equal to zero.
; A PosInt is an integer greater than zero.
;----------------------------------------------------------------------------;

; A Pitcher is a (list NonNegInt NonNegInt)

; INTERPRETATION:
; (capacity contents) represents the capacity and contents of the Pitcher
; WHERE: capacity >= contents

; TEMPLATE:
; pich-fn : Pitcher -> ??
; (define (pich-fn pitcher)
;   (... (first pitcher) (second pitcher)))

; EXAMPLES:
; (list 10 5) 
; (list 8 7)
;----------------------------------------------------------------------------;

; A PitchersExternalRep is a (cons Pitcher PitchersExternalRep)

; INTERPRETATION: PitchersExternalRep can be a Non-Empty list of 
;                 Pitcher followed by PitchersExternalRep
; WHERE: At the beginning, the first pitcher in the list is filled to 
; capacity and the others are empty 

; TEMPLATE:
; pichs-ext-rep-fn : PitchersExternalRep -> ??
; (define (pichs-ext-rep-fn pichs-ext-rep)
;    (... (pich-fn (first pichs-ext-rep))
;         (pichs-ext-rep-fn (rest pichs-ext-rep))))

; EXAMPLES:
; (list (list 10 10) (list 8 0))
; (list (list 15 15) (list 10 0) (list 5 0))

;----------------------------------------------------------------------------;

; A ListOfPitchersExternalRep is one of -

; -- empty                             INTERP: represents an empty 
;                                      ListOfPitchersExternalRep
; -- (cons PitchersExternalRep         INTERP: represents a non-empty 
;          ListOfPitchersExternalRep)  ListOfPitchersExternalRep

; TEMPLATE:
; lo-pichs-ext-rep-fn : ListOfPitchersExternalRep -> ??
; (define (lo-pichs-ext-rep-fn lo-pichs-ext)
;   (cond
;     [(empty? lo-pichs-ext) ...]
;     [else...(pichs-ext-rep-fn (first lo-pichs-ext))
;             (lo-pichs-ext-rep-fn (rest lo-pichs-ext))]))

; EXAMPLE:
; (list (list (list 8 8) (list 3 2) (list 5 2)))
;----------------------------------------------------------------------------;

(define-struct move (src tgt))

; A Move is a (make-move PosInt PosInt)

; INTERPRETATION: (make-move i j) means pour from pitcher i to pitcher j.
; 'pitcher i' refers to the i-th pitcher in the PitchersExternalRep.
; WHERE: src and tgt are different

; TEMPLATE:
; move-fn : Move -> ??
; (define (move-fn m)
;   (... (move-src m) 
;        (move-tgt m)))

; EXAMPLES:
; (make-move 1 2)
; (make-move 1 4)
;----------------------------------------------------------------------------;

; A ListOf<Move> is one of -

; -- empty                     INTERP: is an empty ListOf<Move>
; -- (cons Move ListOf<Move>)  INTERP: is a Move followed by ListOf<Move>

; TEMPLATE:
; lom-fn : ListOf<Move> -> ??
; (define (lom-fn lom)
;   (cond
;     [(empty? lom) ...]
;     [else...(move-fn (first lom))
;             (lom-fn (rest lom))]))

; EXAMPLES:
; (list (make-move 1 2) (make-move 2 4))
; (list (make-move 1 4) (make-move 4 3))
;----------------------------------------------------------------------------;

; A Maybe<ListOf<Move>> is one of 

; -- false           INTERP: represents there is no moves
; -- ListOf<Move>    INTERP: represents a list of moves

; TEMPLATE:
; maybe-lom-fn : Maybe<ListOf<Move>> -> ??
;(define (maybe-lom-fn m-lom)
;  (cond
;    [(false? m-lom)...]
;    [else ...(lom-fn m-lom)])) 
;----------------------------------------------------------------------------;

; A PitchersInternalRep is a (list PitchersExternalRep ListofMoves)

; INTERPRETATION: A PitchersInternalRep is a Non-Empty list which contains 
; PitchersExternalRep as the first element and ListOfMoves as second element.

; TEMPLATE:
; pichs-int-rep-fn : PitchersInternalRep -> ??
; (define (pichs-int-rep-fn pichs-int-rep)
;   (... (pichs-ext-rep-fn (first pichs-int-rep))
;        (lom-fn (second pichs-int-rep))))

; EXAMPLES:
; (list (list (list 10 5) (list 8 7)) (list (make-move 1 2) (make-move 2 1)))
; (list (list (list 15 15) (list 10 0)) (list (make-move 1 2)))
;----------------------------------------------------------------------------;

; A ListOfPitchersInternalRep is one of -

; -- empty                             INTERP: represents an empty 
;                                      ListOfPitchersInternalRep
; -- (cons PitchersInternalRep         INTERP: represents a non-empty 
;          ListOfPitchersInternalRep)  ListOfPitchersInternalRep

; TEMPLATE:
; lo-pichs-int-rep-fn : ListOfPitchersInternalRep -> ??
; (define (lo-pichs-int-rep-fn lo-pichs)
;   (cond
;     [(empty? lo-pichs) ...]
;     [else...(pichs-int-rep-fn (first lo-pichs))
;             (lo-pichs-int-rep-fn (rest lo-pichs))]))

; EXAMPLE:
;(list
; (list (list (list 15 15) (list 10 0)) (list (make-move 1 2)))
; (list (list (list 10 5) (list 8 7)) (list (make-move 1 2) (make-move 2 1))))
;----------------------------------------------------------------------------;

; A NEListOf<PosInt> is a (cons PosInt NEListOf<PosInt>)

; INTERPRETATION: NEListOf<PosInt> represents a list of non-empty PosInts

; TEMPLATE : 
; nelop-fn : NEListOf<PosInt> -> ??
;(define (nelop-fn nelop)
;  ... (first nelop)
;      (nelop-fn (rest nelop)))

; EXAMPLE:
; (list 8 5 3]

;----------------------------------------------------------------------------;
;                           DATA CONSTANTS                                   ;
;----------------------------------------------------------------------------;

(define ZERO 0)
(define ONE 1)

;----------------------------------------------------------------------------;
;                           TESTS CONSTANTS                                  ;
;----------------------------------------------------------------------------;

(define ext-rep-1 '((8 8) (3 2) (5 2)))
(define int-rep-1 (list (list (list 8 8) (list 3 2) (list 5 2)) empty))

(define moves1 (list (make-move 1 2) (make-move 2 3)))
(define moves2 (list (make-move 1 2)))
(define moves3 (list (make-move 1 2) (make-move 2 3) (make-move 3 1)
                     (make-move 2 3) (make-move 3 1) (make-move 2 3)
                     (make-move 1 2) (make-move 2 3)))
(define moves4 (list (make-move 1 2) (make-move 2 3) (make-move 3 1)
                     (make-move 2 3) (make-move 1 2) (make-move 2 3)))
(define moves5 (list (make-move 1 2) (make-move 1 3) (make-move 2 4)
                     (make-move 3 2) (make-move 4 1) (make-move 1 3)
                     (make-move 1 5) (make-move 2 1) (make-move 3 1)
                     (make-move 1 2) (make-move 1 4) (make-move 2 1)
                     (make-move 1 3) (make-move 1 5)))

;----------------------------------------------------------------------------;
;                           FUNCTION DEFINITIONS                             ;
;----------------------------------------------------------------------------;

; list-to-pitchers: PitchersExternalRep -> PitchersInternalRep
; GIVEN: a pithersexternalrep
; RETURNS: the internal representation of the given input.
; EXAMPLE: (list-to-pitchers ext-rep-1) => int-rep-1
; STRATEGY: Function Composition

(define (list-to-pitchers pichs-ext-rep)
  (list pichs-ext-rep empty))

; TESTS
(begin-for-test
  (check-equal? (list-to-pitchers ext-rep-1) int-rep-1
                "Test Failed for list-to-pitchers"))
;----------------------------------------------------------------------------;

; pitchers-to-list : PitchersInternalRep -> PitchersExternalRep
; GIVEN: an internal representation of a set of pitchers
; RETURNS: a pitchersexternalrep that represents the 
; given internal representation.
; EXAMPLE: (pitchers-to-list (list ext-rep-1 moves2)) => ext-rep-1
; STRATEGY: Structural Decomposition on pichs-int-rep : PitchersInternalRep

(define (pitchers-to-list pichs-int-rep)
  (first pichs-int-rep))

; TESTS
(begin-for-test
  (check-equal? (pitchers-to-list (list ext-rep-1 moves2)) ext-rep-1
                "Test Failed for pitchers-to-list"))
;----------------------------------------------------------------------------;

; pitchers-after-moves: PitchersInternalRep ListOf<Move> -> PitchersInternalRep
; GIVEN: An internal representation of a set of pitchers, and a sequence
; of moves
; WHERE: every move refers only to pitchers that are in the set of pitchers
; RETURNS: the internal representation of the set of pitchers that should
; result after executing the given list of moves, in order, on the given
; set of pitchers.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pichs : PitchersInternalRep

(define (pitchers-after-moves pichs moves)
  (list (pitchers-after-moves-helper (pitchers-to-list pichs) moves)
        (append (second pichs) moves)))

; TESTS
(begin-for-test
  (check-equal? (pitchers-after-moves 
                 (list (list (list 8 8) (list 3 2) (list 5 2)) 
                       (list (make-move 1 2))) (list (make-move 2 3)))
                (list (list (list 8 8) (list 3 0) (list 5 4))
                      (list (make-move 1 2) (make-move 2 3)))))
;----------------------------------------------------------------------------;

; pitchers-after-moves-helper : 
;                      PitchersExternalRep ListOf<Move> -> PitchersExternalRep
; GIVEN: a pitcherexternalrep and a list of moves
; RETURNS: a pitcherexternalrep with moves applied to it.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on move : Move

(define (pitchers-after-moves-helper pichs moves)
  (foldl
   ; Move PitchersExternalRep -> PitchersExternalRep
   ; GIVEN: a move and a pitchersexternalrep
   ; RETURNS: pitchersexternalrep with the given move applied to it.
   (lambda (m pichs) (pour-src-tgt (move-src m) (move-tgt m) pichs))
   pichs
   moves))

; TESTS
(begin-for-test
  (check-equal? (pitchers-after-moves 
                 (list (list (list 8 8) (list 3 2) (list 5 2)) 
                       (list (make-move 1 2))) (list (make-move 2 3)))
                (list (list (list 8 8) (list 3 0) (list 5 4))
                      (list (make-move 1 2) (make-move 2 3)))
                "Test Failed for pitchers-after-moves-helper")
  
  (check-equal? (pitchers-after-moves 
                 (list (list (list 8 8) (list 3 2) (list 5 2)) 
                       empty) (list (make-move 2 3)))
                (list (list (list 8 8) (list 3 0) (list 5 4)) 
                      (list (make-move 2 3)))
                "Test Failed for pitchers-after-moves-helper")
  
  (check-equal? (pitchers-after-moves 
                 (list (list (list 8 8) (list 3 2) (list 5 2)) 
                       (list (make-move 1 2))) empty)
                (list (list (list 8 8) (list 3 2) (list 5 2)) 
                      (list (make-move 1 2)))
                "Test Failed for pitchers-after-moves-helper")
  
  (check-equal? (pitchers-after-moves 
                 (list (list (list 8 8) (list 4 4) (list 5 2)) 
                       empty) (list (make-move 2 3)))
                (list (list (list 8 8) (list 4 1) (list 5 5)) 
                      (list (make-move 2 3)))
                "Test Failed for pitchers-after-moves-helper")
  
  (check-equal? (pitchers-after-moves 
                 (list (list (list 8 8) (list 4 4) (list 5 2)) 
                       empty) (list (make-move 2 2)))
                (list (list (list 8 8) (list 4 4) (list 5 2)) 
                      (list (make-move 2 2)))
                "Test Failed for pitchers-after-moves-helper")
  
  (check-equal? (pitchers-after-moves 
                 (list (list (list 8 8) (list 4 1) (list 5 5)) 
                       empty) (list (make-move 2 3)))
                (list (list (list 8 8) (list 4 1) (list 5 5)) 
                      (list (make-move 2 3)))
                "Test Failed for pitchers-after-moves-helper")
  
  (check-equal? (pitchers-after-moves 
                 (list (list (list 8 8) (list 3 0) (list 5 2)) 
                       empty) (list (make-move 2 3)))
                (list (list (list 8 8) (list 3 0) (list 5 2)) 
                      (list (make-move 2 3)))
                "Test Failed for pitchers-after-moves-helper"))
;----------------------------------------------------------------------------;

; pour-src-tgt : PosInt PosInt PitchersExternalRep -> PitchersExternalRep
; GIVEN: source pitcher index, target pitcher index and pitchersexternalrep
; RETURNS: pitchersexternalrep after pouring from source pitcher
; to target pitcher
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (pour-src-tgt src-idx tgt-idx pichs)
  (pour-helper
   (list-ref pichs (- src-idx ONE)) (- src-idx ONE) 
   (list-ref pichs (- tgt-idx ONE)) (- tgt-idx ONE) pichs))

; TESTS
(begin-for-test
  (check-equal? (pour-src-tgt 2 3 (list (list 9 9)
                                        (list 8 2)
                                        (list 4 0)))
                (list (list 9 9) (list 8 0) (list 4 2))
                "Test Failed for pour-src-tgt"))
;----------------------------------------------------------------------------;

; pour-helper : Pitcher NonNegIint Pitcher NonNegInt 
;                                  PitchersExternalRep -> PitchersExternalRep
; GIVEN: Source pitcher, source index, Target pitcher, target index
; and pitchersexternalrep
; RETURNS: pitchersexternalrep after pouring from source pitcher to target
; pitcher.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on src, tgt : Pitcher

(define (pour-helper src src-idx tgt tgt-idx pichs)
  (cond
    [(equal? src-idx tgt-idx) pichs]
    [(equal? (second tgt) (first tgt)) pichs]
    [(equal? (second src) ZERO) pichs]
    [(> (second src) (- (first tgt) (second tgt)))
     (pichs-prep src-idx tgt-idx (- (first tgt) (second tgt)) pichs ZERO)]
    [else (pichs-prep src-idx tgt-idx (second src) pichs ZERO)]))


; TESTS
(begin-for-test
  (check-equal? (pour-helper (list 9 9) 0 (list 8 3) 1 (list (list 9 9)
                                                             (list 8 3)
                                                             (list 4 0)))
                (list (list 9 4) (list 8 8) (list 4 0))
                "Test Failed for pour-helper"))
;----------------------------------------------------------------------------;

; pichs-prep : NonNegInt NonNegInt NonNegInt PitchersExternalRep NonNegInt 
;                                                      -> PitchersExternalRep
; GIVEN: source and target indices, an integer and pitcherexternalrep
; RETURNS: pitchersexternalrep after pouring from source with given src-idx
; to target with given tgt-idx.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pichs : PitchersExternalRep

(define (pichs-prep src-idx tgt-idx diff pichs pich-idx)
  (cond
    [(empty? pichs) empty]
    [else
     (cons
      (pich-compute src-idx tgt-idx diff (first pichs) pich-idx)
      (pichs-prep src-idx tgt-idx diff (rest pichs) (add1 pich-idx)))]))

; TESTS
(begin-for-test
  (check-equal? (pichs-prep 0 1 4 (list (list 5 5) (list 4 0)) 0)
                (list (list 5 1) (list 4 4))
                "Test Failed for pichs-prep"))
;----------------------------------------------------------------------------;
; pich-compute : NonNegInt NonNegInt NonNegInt Pitcher NonNegInt -> Pitcher
; GIVEN: source pitcher, target pitcher, an integer and Pitcher
; RETURNS: an updated pitcher after the move 
; EXAMPLE: see tests below
; STRATEGY: Strucutral Decomposition on pich : Pitcher

(define (pich-compute src-idx tgt-idx diff pich pich-idx)
  (cond 
    [(equal? pich-idx tgt-idx) (list (first pich) (+ (second pich) diff))]
    [(equal? pich-idx src-idx) (list (first pich) (- (second pich) diff))]
    [else pich]))

; TESTS
(begin-for-test
  (check-equal? (pich-compute 0 1 4 (list 4 0) 1)
                (list 4 4)
                "Test Failed for pour-helper"))
;----------------------------------------------------------------------------;

; solution : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
; GIVEN: a list of the capacities of the pitchers and the goal amount
; RETURNS: a sequence of moves which, when executed from left to right,
; results in one pitcher (not necessarily the first pitcher) containing
; the goal amount.  Returns false if no such sequence exists.
; EXAMPLE: (solution (list 10 7 3) 18) => false
; STRATEGY: Function Composition

(define (solution nelop goal)
  (solution-helper nelop goal (list (convert-to-pichs-int nelop)) empty))

; TESTS
(begin-for-test
  (check-equal? (solution (list 8 5 3) 4) moves4
                "Test failed for solution funtion as one of the contents of
 any of the pitchers should be 4 after the outputted list of moves")
  (check-equal? (solution (list 10 7 3) 5) moves3
                "Test failed for solution funtion as one of the contents of
 any of the pitchers should be 5 after the outputted list of moves")
  (check-equal? (solution (list 8 5 3 16 32 65 128) 4) moves5)
  (check-equal? (solution (list 10 7 3) 18) false
                "Test failed for solution funtion as the given goal cannot be 
 acheived and should return false")
  (check-equal? (solution (list 10) 8) false
                "Test failed for solution funtion as the given goal cannot be 
 acheived and should return false")
  (check-equal? (solution (list 10 5 5) 3) false
                "Test failed for solution funtion as the given goal cannot be 
 acheived and should return false")
  (check-equal? (solution (list 10 7 3) 10) empty
                "Test failed for solution funtion as the given goal is already 
 acheived and should return empty list of moves"))
;----------------------------------------------------------------------------;

; convert-to-pichs-int : NEListOf<PosInt> -> PitchersInternalRep
; GIVEN: a list of the capacities of the pitchers
; RETURNS: pitchersinternalrep with contents full to its capacity for the first
; pitcher and contents empty for the rest of the pitchers
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (convert-to-pichs-int nelop)
  (list-to-pitchers (convert-to-pichs-ext nelop)))

; TESTS
(begin-for-test
  (check-equal? (convert-to-pichs-int (list 8 5 3))
                (list (list (list 8 8) (list 5 0) (list 3 0)) empty)
                "Test Failed for convert-to-pichs-int"))
;----------------------------------------------------------------------------;

; convert-to-pichs-ext : NEListOf<PosInt> -> PitchersExternalRep
; GIVEN: a list of the capacities of the pitchers
; RETURNS: pitcherexternalrep with contents full to its capacity for the first 
; pitcher and contents empty for the rest of the pitchers
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on nelop: NEListOf<PosInt>

(define (convert-to-pichs-ext nelop)
  (map
   ; PosInt -> Pitcher
   ; GIVEN: the capacity of the pitcher
   ; RETURNS: a pitcher, with contents full to its capacity if its the first 
   ; pitcher and contents empty if its not the first pitcher.
   (lambda (pos-int) (if (equal? pos-int (first nelop))
                         (list pos-int pos-int)
                         (list pos-int ZERO)))
   nelop))

; TESTS
(begin-for-test
  (check-equal? (convert-to-pichs-ext (list 8 5 3))
                (list (list 8 8) (list 5 0) (list 3 0))
                "Test Failed for convert-to-pichs-ext"))
;----------------------------------------------------------------------------;

; solution-helper : NEListOf<PosInt> PosInt ListOfPitchersInternalRep 
;                   ListOfPitchersInternalRep -> Maybe<ListOf<Move>>
; GIVEN: a list of the capacities of the pitchers, goal amount and a two 
; lists of pitchersinternalrep
; WHERE: -- the ListOfPitchersInternalRep, frontier, is the succesors of all
;           PitchersInternalRep in some ListOfPitchersInternalRep, frontier0, 
;           which will contain all the succesor PitchersInternalReps along
;           with the moves it took to reach the particular configuration.
;        -- the ListOfPitchersInternalRep, explored, is the explored 
;           PitchersInternalRep so far, which is used to filter out the 
;           PitchersInternalReps which have been already traversed from the 
;           unexplored PitchersInternalReps.          
; RETURNS: a list of moves if the goal is achievable else false
; EXAMPLE: see tests below
; STRATEGY: General recursion
; HALTING MEASURE: Is the pair of 
; (Number of unexplored pitchersinternalreps, length of the frontier)
; TERMINATION ARGUMENT: At every recursive call EITHER the frontier list gets 
; shorter or the explored list gets longer. Hence the set of unexplored nodes 
; gets smaller at every recursion. Under the lexicographic ordering: the 
; length of the frontier cannot decrease indefinitely without decreasing 
; the number of unexplored pitchersinternalreps.

(define (solution-helper nelop goal frontier explored)
  (cond
    [(equal? goal (first nelop)) empty]
    [(> goal (max-capacity nelop)) false]
    [(and (empty? (rest nelop)) (< goal (first nelop))) false]
    [(my-member-pitcher? goal (map-pitchers frontier))
     (moves-for-goal goal frontier)]
    [(empty? frontier) false]
    [else 
     (local            
       ((define candidates (set-minus (successors (first frontier)) explored))
        
        (define new-explored (set-union-pitchers candidates explored))
        
        (define new-frontier (set-union-pitchers candidates (rest frontier))))
       
       (cond
         [(empty? candidates)
          (solution-helper nelop goal (rest frontier) explored)]         
         [else (solution-helper nelop goal new-frontier new-explored)]))]))

; TESTS
; constants for tests
(define pitchers-1 (list 10 5 5))
(define ext-rep-2 (list (list 10 10) (list 5 5) (list 5 5)))

(begin-for-test
  (check-equal?
   (solution-helper pitchers-1  3 (list (list ext-rep-2 empty)) empty) false
   "Test Failed for solution-helper"))
;----------------------------------------------------------------------------;

; moves-for-goal : PosInt ListOfPitchersInternalRep -> ListOf<Move>
; GIVEN: a goal amount and the listofpitchersinternalrep
; RETURNS: a list of moves to achieve the goal amount in the given 
;          listofpitchersinternalrep
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on ListOfPitchersInternalRep and 
;           PitchersInternalRep

(define (moves-for-goal goal lo-pichs-int)
  (second (first (goal-tuplet goal lo-pichs-int))))

; TESTS
(begin-for-test 
  (check-equal? (moves-for-goal 15 
                                (list (list (list (list 15 15) (list 10 0))
                                            (list (make-move 1 2)))))
                (list (make-move 1 2))
                "Test Failed for moves-for-goal"))
;----------------------------------------------------------------------------;

; goal-tuplet : PosInt ListOfPitchersInternalRep -> ListOfPitchersInternalRep
; GIVEN: a goal amount and the listofpitchersinternalrep
; RETURNS: listofpitchersinternalrep where the contents of any pitcher
;          matches with the goal
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pichs-int : PitchersInternalRep

(define (goal-tuplet goal lo-pichs-int)
  (filter
   ; PitchersInternalRep -> Boolean
   ; GIVEN:  a pitchersinternalrep
   ; RETURNS: true iff the given pitchersinternalrep has a
   ; pitcher whose contents matches the goal
   (lambda (pichs-int)
     (equal? (first pichs-int) 
             (first (filter-goal goal (map-pitchers lo-pichs-int)))))
   lo-pichs-int))

; TESTS
(begin-for-test 
  (check-equal? (goal-tuplet 15 (list (list (list (list 15 15) (list 10 0)) 
                                            (list (make-move 1 2)))))
                (list (list (list (list 15 15) (list 10 0))
                            (list (make-move 1 2))))
                "Test Failed for goal-tuplet"))
;----------------------------------------------------------------------------;

; max-capacity : NEListOf<PosInt> -> NonNegInt
; GIVEN: list of capacity of pitchers i.e. non empty list of positive integers
; RETURNS: maximum among the given list of pitcher capacities
; EXAMPLE: (max-capacity (list 1 2 3 4)) => 4
; STRATEGY: HOFC

(define (max-capacity nelop)
  (foldr
   max
   ZERO
   nelop))

; TESTS
(begin-for-test
  (check-equal? (max-capacity (list 1 2 3 4)) 4
                "Test Failed for max-capacity"))               
;----------------------------------------------------------------------------;

; successors : PitchersInternalRep -> ListOfPitchersInternalRep
; GIVEN: a pitchersinternalrep
; RETURNS: a ListOfPitchersInternalRep with all successors of given input
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pichs-int : PitchersInternalRep

(define (successors pichs-int)
  (successors-create (first pichs-int) (second pichs-int) ZERO))

; TESTS
(begin-for-test
  (check-equal? (successors (list (list (list 5 5) (list 4 2)) empty))
                (list 
                 (list (list (list 5 5) (list 4 2)) (list (make-move 1 1)))
                 (list (list (list 5 3) (list 4 4)) (list (make-move 1 2)))
                 (list (list (list 5 5) (list 4 2)) (list (make-move 2 1)))
                 (list (list (list 5 5) (list 4 2)) (list (make-move 2 2))))
                "Test failed for successors-extract"))
;----------------------------------------------------------------------------;

; successors-create : PitchersExternalRep ListOf<Move> NonNegInt 
;                                                -> ListOfPitchersInternalRep
; GIVEN: a pitchersixternalrep, a list of moves and a source index as NonNegInt
; WHERE: source index, src-idx, is the current index of the given source.
;        moves-sofar is the moves traversed so far in the given
;        pitchersixternalrep
; RETURNS: a listofpitchersinternalrep i.e. all the possible successors with 
; the given pitcher index as the source index
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (successors-create pichs-ext moves-sofar src-idx)
  (if (> src-idx (- (length pichs-ext) ONE))
      empty
      (append (succesors-search
               pichs-ext (length pichs-ext) src-idx ZERO moves-sofar)
              (successors-create pichs-ext moves-sofar (add1 src-idx)))))

; TESTS
(begin-for-test (successors-create (list (list 5 5) (list 4 2)) empty 0)
                (list
                 (list (list (list 5 5) (list 4 2)) (list (make-move 1 1)))
                 (list (list (list 5 3) (list 4 4)) (list (make-move 1 2)))
                 (list (list (list 5 5) (list 4 2)) (list (make-move 2 1)))
                 (list (list (list 5 5) (list 4 2)) (list (make-move 2 2)))))
;----------------------------------------------------------------------------;

; succesors-search : PitchersExternalRep PosInt NonNegInt NonNegInt 
;                    ListOf<Move> -> ListOfPitchersInternalRep
; GIVEN: a pitcherexternalrep, length of the pitcherexternalrep, source index,
;        target index position and the list of moves
; WHERE: source index,src-idx, is the current index of the source
;        target index,tgt-idx, is the current index of the target
;        moves-sofar is the moves traversed so far 
; RETURNS: a listofpitchersinternalrep i.e. all the successors with 
; the given pitcher indices as the source and target indices
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (succesors-search pichs-ext pichs-len src-idx tgt-idx moves-sofar)
  (if (> tgt-idx (- pichs-len ONE))
      empty
      (append 
       (list (pichs-creation pichs-ext pichs-len src-idx tgt-idx moves-sofar))
       (succesors-search 
        pichs-ext pichs-len src-idx (add1 tgt-idx) moves-sofar))))

; TESTS
(begin-for-test
  (check-equal? (succesors-search (list (list 5 5) (list 4 2)) 2 0 0 empty)
                (list 
                 (list (list (list 5 5) (list 4 2)) (list (make-move 1 1)))
                 (list (list (list 5 3) (list 4 4)) (list (make-move 1 2))))))
;----------------------------------------------------------------------------;

; pichs-creation : PitchersExternalRep PosInt NonNegInt NonNegInt ListOf<Move>
;                  -> PitchersExternalRep
; GIVEN: a pitchersexternalrep, length of the pitcherexternalrep, source index,
; target index, and the list of moves
; WHERE: source index,src-idx, is the current index of the source
;        target index,tgt-idx, is the current index of the target
;        moves-sofar is the moves traversed so far 
; RETURNS: a pitchersexternalrep which is a possible successor with the 
; given source index and target index
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (pichs-creation pichs-ext pichs-len src-idx tgt-idx moves-sofar)
  (local
    ((define src-tgt (pour (list-ref pichs-ext src-idx) src-idx
                           (list-ref pichs-ext tgt-idx) tgt-idx)))
    (list 
     (pichs-update 
      pichs-ext pichs-len src-idx tgt-idx src-tgt ZERO)
     (append moves-sofar (list (make-move (add1 src-idx) (add1 tgt-idx)))))))

; TESTS
(begin-for-test
  (check-equal? (pichs-creation (list (list 5 5) (list 4 2)) 2 0 1 empty)
                (list
                 (list (list 5 3) (list 4 4))
                 (list (make-move 1 2)))
                "Test Failed for pichs-creation"))
;----------------------------------------------------------------------------;

; pichs-update : PitchersExternalRep PosInt NonNegInt NonNegInt 
;                PitchersExternalRep NonNegInt -> PitchersExternalRep
; GIVEN: a pitchersexternalrep, length of the pitcherexternalrep, source index,
; target index, list of updated source and target, pitcher index
; WHERE: source index,src-idx, is the current index of the source
;        target index,tgt-idx, is the current index of the target
;        pitcher index,pich-idx, is the index of the 
;        current pitcher in the given pitchersexternalrep
; RETURNS: a pitchersexternalrep with source and target pitchers updated
; EXAMPLE:  see tests below
; STRATEGY: Structural Decompostion on src-tgt : PitchersExternalRep

(define (pichs-update pichs-ext pichs-len src-idx tgt-idx src-tgt pich-idx)
  (cond
    [(> pich-idx (- pichs-len ONE)) empty]
    [(equal? pich-idx tgt-idx)
     (append (list (second src-tgt))
             (pichs-update pichs-ext pichs-len src-idx tgt-idx src-tgt 
                           (add1 pich-idx)))]
    [(equal? pich-idx src-idx) 
     (append (list (first src-tgt))
             (pichs-update pichs-ext pichs-len src-idx tgt-idx src-tgt 
                           (add1 pich-idx)))]
    [else 
     (append (list (list-ref pichs-ext pich-idx))
             (pichs-update pichs-ext pichs-len src-idx tgt-idx src-tgt 
                           (add1 pich-idx)))]))

; TESTS
(begin-for-test
  (check-equal? (pichs-update (list (list 5 5) (list 4 2)) 2 0 1 
                              (list (list 5 3) (list 4 4)) 0)
                (list (list 5 3) (list 4 4))
                "Test Failed for pichs-update"))

;----------------------------------------------------------------------------;

; pour : Pitcher NonNegInt Pitcher NonNegInt -> PitchersExternalRep
; GIVEN: A source pitcher and its index and a target pitcher and its index
; RETURNS: A pitchersexternalrep which contains the 
; updated source and target pitchers.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on src,tgt : Pitcher

(define (pour src src-idx tgt tgt-idx)
  (if (= src-idx tgt-idx)
      (list src tgt)
      (pour-contents src src-idx tgt tgt-idx)))

; TESTS
(begin-for-test
  (check-equal? (pour (list 8 5) 1 (list 5 3) 2)
                (list (list 8 3) (list 5 5))))
;----------------------------------------------------------------------------;

; pour-contents : Pitcher NonNegInt Pitcher NonNegInt -> PitchersExternalRep
; GIVEN: a source pitcher, source index position, target pitcher and target 
; index position
; RETURNS: a pitcherexternalrep with updated source and target pitchers
; EXAMPLES: see tests below
; STRATEGY: Structural Decomposition on src, tgt : Pitcher

(define (pour-contents src src-idx tgt tgt-idx)      
  (local
    (; Pitcher -> NonNegInt
     ; GIVEN : A picther
     ; RETURNS : a nonnegint which represents how 
     ; much can be filled to the pitcher. (capacity - contents)
     (define tgt-fill (- (first tgt) (second tgt))))
    (if (> tgt-fill (second src))
        (list (list (first src) ZERO)
              (list (first tgt) (+ (second tgt) (second src))))
        (list (list (first src) (- (second src) tgt-fill))
              (list (first tgt) (+ (second tgt) tgt-fill))))))

; TESTS
(begin-for-test
  (check-equal? (pour-contents (list 8 4) 1 (list 5 3) 2)
                (list (list 8 2) (list 5 5))
                "Test Failed for pour-contents"))

;----------------------------------------------------------------------------;

; set-union-pitchers : ListOfPitchersInternalRep ListOfPitchersInternalRep -> 
;                      ListOfPitchersInternalRep 
; GIVEN: two listofpitchersinternalrep lo-pichs-int1 and lo-pichs-int2
; RETURNS: a new listofpitchersinternalrep which has all elements of the 
; given two listofpitchersinternalrep without duplicates.
; EXAMPLE: see tests below
; STRATEGY: HOFC

(define (set-union-pitchers lo-pichs-int1 lo-pichs-int2)
  (foldr 
   set-cons-pitchers
   lo-pichs-int2 
   lo-pichs-int1))

; TESTS
(begin-for-test
  (check-equal? 
   (set-union-pitchers 
    (list
     (list (list (list 15 15) (list 10 0)) (list (make-move 1 2)))
     (list (list (list 10 5) (list 8 7)) (list (make-move 2 3))))
    (list
     (list (list (list 15 15) (list 10 0)) 
           (list (make-move 2 3)))))
   (list
    (list (list (list 10 5) (list 8 7)) (list (make-move 2 3)))
    (list (list (list 15 15) (list 10 0)) (list (make-move 2 3))))))

;----------------------------------------------------------------------------;

; set-cons-pitchers : PitchersInternalRep ListOfPitchersInternalRep
; GIVEN: A pitchersinternalrep and listofpitchersinternalrep
; RETURNS: An updated listofpitchersinternalrep with the given 
; pitchersinternalrep added to it if it is not already present.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pichs-int : PitchersInternalRep

(define (set-cons-pitchers pichs-int lo-pichs-int2)
  (if (my-member? (first pichs-int) (map-pitchers lo-pichs-int2))
      lo-pichs-int2
      (cons pichs-int lo-pichs-int2)))

; TESTS
(begin-for-test
  (check-equal? (set-cons 
                 (list (list (list 5 0) (list  6 6)) (list (make-move 1 2)))
                 (list
                  (list (list (list 15 15) (list 10 0)) (list (make-move 1 2)))
                  (list (list (list 10 5) (list 8 7)) (list (make-move 2 3)))))
                (list
                 (list (list (list 5 0) (list 6 6)) (list (make-move 1 2)))
                 (list (list (list 15 15) (list 10 0)) (list (make-move 1 2)))
                 (list (list (list 10 5) (list 8 7)) (list (make-move 2 3))))))
;----------------------------------------------------------------------------;

; set-minus : ListOfPitchersInternalRep ListOfPitchersInternalRep -> 
;             ListOfPitchersInternalRep 
; GIVEN: two listofpitchersinternalrep lo-pichs-int1 and lo-pichs-int2
; RETURNS: The unique elements of the lo-pichs-int1 which are not in
;          lo-pichs-int2.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pichs-int : PitchersInternalRep

(define (set-minus lo-pichs-int1 lo-pichs-int2) 
  (filter
   ; PitchersInternalRep -> Boolean
   ; GIVEN : A pitchersinternalrep
   ; RETURNS : true iff the given pitchersinternalrep is 
   ; not present in lo-pichs-int2
   (lambda (pichs-int) 
     (not (my-member? (first pichs-int) (map-pitchers lo-pichs-int2))))
   lo-pichs-int1))

; TESTS
(begin-for-test
  (check-equal? (set-minus 
                 (list
                  (list (list (list 15 15) (list 10 0)) (list (make-move 1 2)))
                  (list (list (list 10 5) (list 8 7)) (list (make-move 2 3))))
                 (list
                  (list (list (list 15 15) (list 10 0)) 
                        (list (make-move 2 3)))))
                (list (list (list (list 10 5) (list 8 7))
                            (list (make-move 2 3))))))
;----------------------------------------------------------------------------;

; map-pitchers : ListOfPitchersInternalRep  -> ListOfPitchersExternalRep 
; GIVEN: A listofpitchersinternalrep
; RETURNS: the corresponding listofpitchersexternalrep
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on PitchersInternalRep

(define (map-pitchers lo-pichs-int) 
  (map
   first
   lo-pichs-int))

; TESTS
(begin-for-test
  (check-equal? (map-pitchers (list int-rep-1))
                (list (list (list 8 8) (list 3 2) (list 5 2)))))
;----------------------------------------------------------------------------;

; my-member-pitcher? : PosInt ListOfPitchersExternalRep  -> Boolean
; GIVEN: A goal and a listofpitchersexternalrep
; RETURNS: true iff the listofpitchersexternalrep has a pitcher whose
; contents matches the goal
; EXAMPLE: see tests below
; STRATEGY: HOFC

(define (my-member-pitcher? goal lo-pichs) 
  (ormap
   ; PitchersExternalRep -> Boolean
   ; GIVEN : A pitchersexternalrep
   ; RETURNS : true iff pitchersexternalrep has a
   ; pitcher whose contents matches the goal
   (lambda (pichs) (my-member-helper? goal pichs))
   lo-pichs))

; TESTS
(begin-for-test
  (check-true (my-member-pitcher?
               2 (list (list (list 8 8) (list 3 2) (list 5 2))))))
;----------------------------------------------------------------------------;

; my-member-helper? : PosInt PitchersExternalRep -> Boolean
; GIVEN: A goal and a pitchersexternalrep
; RETURNS: true iff the pitchersexternalrep has a pitcher whose
; contents matches the goal
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pich : Pitcher 

(define (my-member-helper? goal pichs) 
  (ormap
   ; Pitcher -> Boolean
   ; GIVEN : A pitcher
   ; RETURNS : true iff the pitcher contents matches the goal
   (lambda (pich) (equal? goal (second pich)))
   pichs))

; TESTS
(begin-for-test
  (check-true (my-member-helper? 2 (list (list 8 8) (list 3 2) (list 5 2)))))
;----------------------------------------------------------------------------;

; filter-goal : PosInt ListOfPitchersExternalRep -> ListOfPitchersExternalRep
; GIVEN: A goal and listofpictherexternalrep
; RETURNS: listofpictherexternalrep where any of the pitchers 
; content matches the goal
; EXAMPLE: see tests below
; STRATEGY: HOFC

(define (filter-goal goal lo-pichs) 
  (filter
   ; PitchersExternalRep -> Boolean
   ; GIVEN : a PitchersExternalRep
   ; RETURNS : true iff any of the pitchers 
   ; content matches the goal
   (lambda (pichs) (filter-helper goal pichs)) 
   lo-pichs))

; TESTS
(begin-for-test
  (check-equal? (filter-goal 2 (list (list (list 8 8) (list 3 2) (list 5 2))))
                (list (list (list 8 8) (list 3 2) (list 5 2)))))

;----------------------------------------------------------------------------;

; filter-helper : PosInt PitchersExternalRep -> Boolean
; GIVEN: A goal and a pitchersexternalrep
; RETURNS: true iff the goal is present in the contents of any pitcher 
; in the given pitchersexternalrep
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on pich : Pitcher

(define (filter-helper goal pichs)
  (ormap
   ; Pitcher -> Boolean
   ; GIVEN : A pitcher
   ; RETURNS : true iff the pitcher contents matches the goal
   (lambda (pich) 
     (equal? goal (second pich)))
   pichs))

; TESTS
(begin-for-test
  (check-equal? (filter-helper 4  (list (list 5 5) (list 4 4))) true
                "Test Failed for filter-helper, output should be true"))
;----------------------------------------------------------------------------;
