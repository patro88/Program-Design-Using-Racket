;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; robot.rkt

(require rackunit)
(require "extras.rkt")

(provide
  initial-robot
  robot-left 
  robot-right
  robot-forward
  robot-north? 
  robot-south? 
  robot-east? 
  robot-west?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS
(define robo-radius 15)
(define x-max 200)
(define y-max 400)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct robot (x-cord y-cord radius direction))

;; A robot is a (make-room Real Real NonNegativeReal String)

;; INTERPRETATION:
;;  x-cord and y-cord is the centre of the radius of the robot on the
;;  graphics-style coordinate (in pixels).
;;  radius is the radius of the robot (in pixels).
;;  direction is the direction at which robot is pointing.

;; TEMPLATE:
;; (define (robot-fn rob)
;;   (...
;;    (robot-x-cord rob)
;;    (robot-x-cord rob)
;;    (robot-radius rob)
;;    (robot-direction rob)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-robot: Real Real -> Robot
;; GIVEN: x and y coordinates of the centre of the robot (in pixels)
;; RETURNS: an initial state of robot with centre at x and y (in pixels)
;; coordinates as in graphics-style coordinate with radius as robot-radius 
;; and robot pointing to NORTH direction
;; EXAMPLES:
;;  (initial-robot 10 20)    => (make-robot 10 20 15 "NORTH")
;;  (initial-robot -10 -30)  => (make-robot -10 -30 15 "NORTH")
;; STRATEGY: Function Composition

(define (initial-robot x y)
  (make-robot x y robo-radius "NORTH"))

;; TESTS
(begin-for-test
  (check-equal? (initial-robot 10 20) (make-robot 10 20 15 "NORTH")
       "Test Failed, robot initial position should be (10,20)")
  (check-equal? (initial-robot -40 20) (make-robot -40 20 15 "NORTH")
       "Test Failed, robot initial position should be (-40,20)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-north?: Robot -> Boolean
;; robot-south?: Robot -> Boolean
;; robot-east?:  Robot -> Boolean
;; robot-west?:  Robot -> Boolean
;; GIVEN: a robot
;; RETURNS: whether the robot is facing in the specified direction.
;; EXAMPLES:
;;  (robot-north? (make-robot 10 20 20 "NORTH")) => true
;;  (robot-south? (make-robot 10 20 20 "SOUTH")) => true
;; STRATEGY: Structural Decomposition on robot

(define (robot-north? rob)
  (string=? (robot-direction rob) "NORTH"))

(define (robot-west? rob)
  (string=? (robot-direction rob) "WEST"))

(define (robot-south? rob)
  (string=? (robot-direction rob) "SOUTH"))

(define (robot-east? rob)
  (string=? (robot-direction rob) "EAST"))

;; TESTS

(begin-for-test
  (check-equal? (robot-north? (make-robot 10 20 20 "NORTH")) true
                "Test Failed, Robot is pointing to North")
  (check-equal? (robot-south? (make-robot 10 20 20 "SOUTH")) true
                "Test Failed, Robot is pointing to South")
  (check-equal? (robot-east?  (make-robot 10 20 20 "EAST"))  true
                "Test Failed, Robot is pointing to East")
  (check-equal? (robot-west?  (make-robot 10 20 20 "WEST"))  true
                "Test Failed, Robot is pointing to West"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; direction-change:  Robot direction -> Robot
;; GIVEN: a robot and the direction
;; RETURNS: a robot like the original, but turned 90 degrees either 
;; left or right.
;; EXAMPLES:
;;  (direction-change (make-robot 10 20 20 "NORTH") "WEST")
;;                 => (make-robot 10 20 20 "WEST")
;;  (direction-change (make-robot 20 30 20 "EAST") "SOUTH")
;;                 => (make-robot 20 30 20 "SOUTH"))
;; STRATEGY: Structural Decomposition on Robot (rob)

(define (direction-change rob dir)
  (make-robot (robot-x-cord rob) (robot-y-cord rob) (robot-radius rob) dir))

;; TESTS
(begin-for-test
  (check-equal? (direction-change (make-robot 10 20 20 "NORTH") "WEST")
                (make-robot 10 20 20 "WEST") "Test Failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-left:  Robot -> Robot
;; robot-right: Robot -> Robot
;; GIVEN: a robot
;; RETURNS: a robot like the original, but turned 90 degrees either 
;; left or right.
;; EXAMPLES:
;;  (robot-left  (make-robot 10 20 20 "NORTH"))
;;           =>  (make-robot 10 20 20 "WEST")
;;  (robot-right (make-robot 20 30 20 "EAST"))
;;           =>  (make-robot 20 30 20 "SOUTH"))
;; STRATEGY: Function Composition

(define (robot-left rob)
  (cond
    [(robot-north? rob) (direction-change rob "WEST")]
    [(robot-west?  rob) (direction-change rob "SOUTH")]
    [(robot-south? rob) (direction-change rob "EAST")]
    [(robot-east?  rob) (direction-change rob "NORTH")]))


(define (robot-right rob)
  (cond
    [(robot-north? rob) (direction-change rob "EAST")]
    [(robot-east?  rob) (direction-change rob "SOUTH")]
    [(robot-south? rob) (direction-change rob "WEST")]
    [(robot-west?  rob) (direction-change rob "NORTH")]))

;; TESTS

(begin-for-test
  (check-equal? (robot-right (make-robot 10 20 15 "NORTH"))
                (make-robot 10 20 15 "EAST")
                "Test Failed, Robot should be pointing east")
  (check-equal? (robot-right (make-robot 10 20 15 "EAST"))
                (make-robot 10 20 15 "SOUTH")
                "Test Failed, Robot should be pointing south")
  (check-equal? (robot-right (make-robot 10 20 15 "SOUTH"))
                (make-robot 10 20 15 "WEST")
                "Test Failed, Robot should be pointing west")
  (check-equal? (robot-right (make-robot 10 20 15 "WEST"))
                (make-robot 10 20 15 "NORTH")
                "Test Failed, Robot should be pointing north")
  (check-equal? (robot-left (make-robot 10 20 15 "NORTH"))
                (make-robot 10 20 15 "WEST")
                "Test Failed, Robot should be pointing west")
  (check-equal? (robot-left (make-robot 10 20 15 "WEST"))
                (make-robot 10 20 15 "SOUTH")
                "Test Failed, Robot should be pointing south")
  (check-equal? (robot-left (make-robot 10 20 15 "SOUTH"))
                (make-robot 10 20 15 "EAST")
                "Test Failed, Robot should be pointing east")
  (check-equal? (robot-left (make-robot 10 20 15 "EAST"))
                (make-robot 10 20 15 "NORTH")
                "Test Failed, Robot should be pointing north"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-inside-room?  Robot -> Boolean
;; GIVEN: a robot
;; RETURNS: true if robot is inside the room. (A robot is inside the room
;; only if whole robot is inside the room. This can be checked by imagining
;; another rectangular room with original size mius robo-radius from all the 
;; directions of original room) else false.
;; EXAMPLES:
;;  (robot-inside-room? (make-robot 10 20 15 "NORTH")) => false
;;  (robot-inside-room? (make-robot 100 200 15 "NORTH")) => true
;; STRATEGY: Structural Decomposition on robot (rob)

(define (robot-inside-room? rob)
  (if(and (and (<= (robot-x-cord rob) (- x-max (robot-radius rob)))
               (>= (robot-x-cord rob) (robot-radius rob)))
          (and (<= (robot-y-cord rob) (- y-max (robot-radius rob)))
               (>= (robot-y-cord rob) (robot-radius rob)))) true false))

;; TESTS
(begin-for-test
  (check-equal? (robot-inside-room? (make-robot 100 300 robo-radius "NORTH")) 
           true "Test for robot-inside-room? Failed, output should be true")
  (check-equal? (robot-inside-room? (make-robot 10 30 robo-radius "EAST")) 
           false "Test for robot-inside-room? Failed, output should be false"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-normal-forward : Robot PosInt -> Robot
;; GIVEN: a robot and a distance
;; RETURNS: a robot like the given one, but moved forward by the
;; specified number of pixels distance in that particular direction.
;; EXAMPLES:
;;  (robot-normal-forward (make-robot 1500 2000 robo-radius "NORTH") 800)
;;    => (make-robot 1500 1200 robo-radius "NORTH")
;;  (robot-normal-forward (make-robot -120 -15 robo-radius "EAST") 20)
;;    => (make-robot -100 -15 robo-radius "SOUTH")
;; STRATEGY: Structural Decomposition on robot

(define (robot-normal-forward rob x)
  (cond
    [(robot-north? rob) 
     (make-robot (robot-x-cord rob) (- (robot-y-cord rob) x) 
                 (robot-radius rob) "NORTH")]
    [(robot-south? rob) 
     (make-robot (robot-x-cord rob) (+ (robot-y-cord rob) x) 
                 (robot-radius rob) "SOUTH")]
    [(robot-east?  rob) 
     (make-robot (+ (robot-x-cord rob) x) (robot-y-cord rob) 
                 (robot-radius rob) "EAST")]
    [(robot-west?  rob) 
     (make-robot (- (robot-x-cord rob) x) (robot-y-cord rob) 
                 (robot-radius rob) "WEST")]))

(begin-for-test
  (check-equal? (robot-normal-forward (make-robot -150 200 robo-radius "NORTH") 20)
                (make-robot -150 180 robo-radius "NORTH") 
                "Test Failed, new y coordinate should be 400")
  (check-equal? (robot-normal-forward (make-robot -100 -285 robo-radius "SOUTH") 10) 
                (make-robot -100 -275 robo-radius "SOUTH") 
                "Test Failed, new y coordinate should be -275")
  (check-equal? (robot-normal-forward (make-robot 120 105 robo-radius "EAST") 20) 
                (make-robot 140 105 robo-radius "EAST")
                "Test Failed, new x coordinate should be 140")
  (check-equal? (robot-normal-forward (make-robot 185 385 robo-radius "WEST") 10) 
                (make-robot 175 385 robo-radius "WEST") 
                "Test Failed, new x coordinate should be 175"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-robot-north : Robot PosInt -> Robot
;; new-robot-south : Robot PosInt -> Robot
;; new-robot-east  : Robot PosInt -> Robot
;; new-robot-west  : Robot PosInt -> Robot
;; GIVEN: a robot and a distance
;; RETURNS: a robot like the given one, but moved forward by the
;; specified number of pixels distance.  If moving forward the specified
;; number of pixels distance would cause the robot to move from being 
;; entirely inside the room to being even partially outside the room
;; then the robot should stop at the wall.
;; EXAMPLES:
;;  (robot-new-coordinate (make-robot 1500 2000 robo-radius "NORTH") 800)
;;    => (make-robot 1500 1200 robo-radius "NORTH")
;;  (robot-new-coordinate (make-robot -120 -15 robo-radius "EAST") 20)
;;    => (make-robot -100 -15 robo-radius "SOUTH")
;; STRATEGY: Structural Decomposition on robot (rob)

(define (new-robot-north rob x)
  (if (>= (distance-from-y1-axis rob) x)
      (make-robot (robot-x-cord rob) (- (robot-y-cord rob) x) 
                  (robot-radius rob) "NORTH")
      (make-robot (robot-x-cord rob) (robot-radius rob) 
                  (robot-radius rob) "NORTH")))

(define (new-robot-south rob x)
(if (>= (distance-from-y2-axis rob) x)
         (make-robot (robot-x-cord rob) (+ (robot-y-cord rob) x) 
                     (robot-radius rob) "SOUTH")
         (make-robot (robot-x-cord rob) (- y-max (robot-radius rob)) 
                     (robot-radius rob) "SOUTH")))

(define (new-robot-west rob x)
  (if (>= (distance-from-x1-axis rob) x)
         (make-robot (- (robot-x-cord rob) x) (robot-y-cord rob) 
                     (robot-radius rob) "WEST")
         (make-robot (robot-radius rob) (robot-y-cord rob) 
                     (robot-radius rob) "WEST")))

(define (new-robot-east rob x)
(if (>= (distance-from-x2-axis rob) x)
         (make-robot (+ (robot-x-cord rob) x) (robot-y-cord rob) 
                     (robot-radius rob) "EAST")
         (make-robot (- x-max (robot-radius rob)) (robot-y-cord rob) 
                     (robot-radius rob) "EAST")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; robot-new-coordinate : Robot PosInt -> Robot
;; GIVEN: a robot and a distance
;; RETURNS: a robot like the given one, but moved forward by the
;; specified number of pixels distance.  If moving forward the specified
;; number of pixels distance would cause the robot to move from being 
;; entirely inside the room to being even partially outside the room
;; then the robot should stop at the wall.
;; EXAMPLES:
;;  (robot-new-coordinate (make-robot 1500 2000 robo-radius "NORTH") 800)
;;    => (make-robot 1500 1200 robo-radius "NORTH")
;;  (robot-new-coordinate (make-robot -120 -15 robo-radius "EAST") 20)
;;    => (make-robot -100 -15 robo-radius "SOUTH")
;; STRATEGY: Structural Decomposition on robot (rob)

(define (robot-new-coordinate rob x)
  (cond
    [(robot-north? rob) (new-robot-north rob x)]
    [(robot-south? rob) (new-robot-south rob x)]
    [(robot-west? rob)  (new-robot-west rob x) ]
    [(robot-east? rob)  (new-robot-east rob x) ]))

(begin-for-test
  (check-equal? (robot-new-coordinate (make-robot 50 200 robo-radius "NORTH") 200)
                (make-robot 50 15 robo-radius "NORTH") 
                "Test Failed, new y coordinate should be 15")
  (check-equal? (robot-new-coordinate (make-robot 50 200 robo-radius "NORTH") 20)
                (make-robot 50 180 robo-radius "NORTH") 
                "Test Failed, new y coordinate should be 15")
  (check-equal? (robot-new-coordinate (make-robot 50 200 robo-radius "WEST") 10) 
                (make-robot 40 200 robo-radius "WEST") 
                "Test Failed, new x coordinate should be 40")
  (check-equal? (robot-new-coordinate (make-robot 200 105 robo-radius "WEST") 200) 
                (make-robot 15 105 robo-radius "WEST")
                "Test Failed, new x coordinate should be 15")
  (check-equal? (robot-new-coordinate (make-robot 175 375 robo-radius "SOUTH") 100) 
                (make-robot 175 385 robo-radius "SOUTH") 
                "Test Failed, new y coordinate should be 385")
  (check-equal? (robot-new-coordinate (make-robot 175 350 robo-radius "SOUTH") 10) 
                (make-robot 175 360 robo-radius "SOUTH") 
                "Test Failed, new y coordinate should be 360")
  (check-equal? (robot-new-coordinate (make-robot 175 375 robo-radius "EAST") 100) 
                (make-robot 185 375 robo-radius "EAST") 
                "Test Failed, new x coordinate should be 185")
  (check-equal? (robot-new-coordinate (make-robot 155 375 robo-radius "EAST") 10) 
                (make-robot 165 375 robo-radius "EAST") 
                "Test Failed, new x coordinate should be 165"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; distance-from-x1-axis : Robot -> NonNegativeReal
;; distance-from-x2-axis : Robot -> NonNegativeReal
;; distance-from-y1-axis : Robot -> NonNegativeReal
;; distance-from-y2-axis : Robot -> NonNegativeReal

;; GIVEN: a robot
;; RETURNS: 
;;  x1-axis: is an axis at x = (robot-radius rob) 
;;  x2-axis: is an axis at x = x-max - (robot-radius rob) 
;;  y1-axis: is an axis at y = (robot-radius rob) 
;;  y2-axis: is an axis at y = y-max - (robot-radius rob)
;;  Output is the distance of the centre of the robot from the 
;;  above mentioned axis.
;; EXAMPLES:
;;  (distance-from-x1-axis (make-robot 100 20 robo-radius "NORTH"))  => 85
;;  (distance-from-y2-axis (make-robot -120 -15 robo-radius "EAST")) => 400
;; STRATEGY: Structural Decomposition on robot (rob)

(define (distance-from-x1-axis rob)
  (- (robot-x-cord rob) (robot-radius rob)))

(define (distance-from-x2-axis rob)
  (- (- x-max (robot-radius rob)) (robot-x-cord rob)))

(define (distance-from-y1-axis rob)
  (- (robot-y-cord rob) (robot-radius rob)))

(define (distance-from-y2-axis rob)
  (- (- y-max (robot-radius rob)) (robot-y-cord rob)))    

;; TESTS

(begin-for-test
  (check-equal? (distance-from-x1-axis 
                 (make-robot 100 20 robo-radius "NORTH"))
                85 "Test Failed, distance from x1 axis should be 85")
  (check-equal? (distance-from-x2-axis 
                 (make-robot 500 20 robo-radius "NORTH"))
                -315 "Test Failed, distance from x2 axis should be 115")
  (check-equal? (distance-from-y1-axis 
                 (make-robot 100 20 robo-radius "NORTH"))
                 5 "Test Failed, distance from x1 axis should be 85")
  (check-equal? (distance-from-y2-axis 
                 (make-robot 100 400 robo-radius "NORTH"))
                -15 "Test Failed, distance from x1 axis should be 85"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; crosses-x-range-room? : Robot NonNegativeReal -> Boolean
;; crosses-y-range-room? : Robot NonNegativeReal -> Boolean
;; GIVEN: a robot and a distance
;; RETURNS: If the whole robot went inside the room while on move, then 
;; true else false.
;; EXAMPLES:
;;  (crosses-x-range-room? (make-robot 300 300 robo-radius "WEST") 500) => true
;;  (crosses-y-range-room? (make-robot 150 800 robo-radius "NORTH") 100)=> false
;; STRATEGY: Structural Decomposition on robot (rob)

(define (crosses-x-range-room? rob x)
  (cond
    [(and (and (> (robot-y-cord rob) 0) (robot-north? rob)) 
          (> x (distance-from-y1-axis rob))) true]
    [(and (and (< (robot-y-cord rob) 0) (robot-south? rob)) 
          (> x (distance-from-y2-axis rob))) true]
    [else false]))

(define (crosses-y-range-room? rob x)
  (cond
    [(and (and (> (robot-x-cord rob) 0) (robot-west? rob))  
          (> x (distance-from-x1-axis rob))) true]
    [(and (and (< (robot-x-cord rob) 0) (robot-east? rob))  
          (> x (distance-from-x2-axis rob))) true]
    [else false]))

;; TESTS

(begin-for-test
  (check-equal? (crosses-x-range-room? (make-robot 300 300 robo-radius "NORTH")
      500) true "Test Failed, output should be true")
  (check-equal? (crosses-y-range-room? (make-robot -10 300 robo-radius "WEST")
      500) false "Test Failed, output should be false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-within-x-range : Robot -> Boolean
;; robot-within-y-range : Robot -> Boolean
;; GIVEN: a robot
;; RETURNS: If the whole robot went inside the x-range or y-range
;; then true else false
;; EXAMPLES:
;;  (robot-within-x-range (make-robot 300 300 robo-radius "NORTH") 500) => true
;;  (robot-within-y-range (make-robot 150 800 robo-radius "WEST") 100) => false
;; STRATEGY: Structural Decomposition on robot (rob)

(define (robot-within-x-range rob)
  (and (<= (robot-x-cord rob) (- x-max (robot-radius rob))) 
       (>= (robot-x-cord rob) (robot-radius rob))))

(define (robot-within-y-range rob)
  (and (<= (robot-y-cord rob) (- y-max (robot-radius rob))) 
       (>= (robot-y-cord rob) (robot-radius rob))))

(begin-for-test
  (check-equal? (robot-within-x-range (make-robot 170 300 robo-radius "NORTH"))
               true "Test Failed, output should have been true")
  (check-equal? (robot-within-y-range (make-robot 150 800 robo-radius "WEST")) 
               false "Test Failed, output should have been false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-went-inside-room? : Robot NonNegativeReal -> Boolean
;; GIVEN: a robot and a distance
;; RETURNS: If the whole robot went inside the room while on move, then 
;; true else false. This function will be called by robot-forward
;; EXAMPLES:
;;  (robot-went-inside-room? (make-robot 300 300 robo-radius "WEST") 500)
;;     => true
;;  (robot-went-inside-room? (make-robot 150 800 robo-radius "NORTH") 100)
;;     => false
;; STRATEGY: Functional Composition

(define (robot-went-inside-room? rob x)
  (cond
    [(robot-within-x-range rob) (crosses-x-range-room? rob x)]
    [(robot-within-y-range rob) (crosses-y-range-room? rob x)]
    [else false]))

(begin-for-test
  (check-equal? (robot-went-inside-room? 
                 (make-robot 300 300 robo-radius "WEST") 500)
                 true "Test Failed, Output should be True")
  (check-equal? (robot-went-inside-room? 
                 (make-robot 150 800 robo-radius "NORTH") 100)
                 false "Test Failed, Output should be False"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; robot-forward : Robot PosInt -> Robot
;; GIVEN: a robot and a distance
;; RETURNS: a robot like the given one, but moved forward by the
;; specified number of pixels distance.  If moving forward the specified
;; number of pixels distance would cause the robot to move from being 
;; entirely inside the room to being even partially outside the room
;; then the robot should stop at the wall.
;; EXAMPLES:
;;  (robot-forward (make-robot 150 200 robo-radius "NORTH") 800)
;;    => (make-robot 150 15 robo-radius "NORTH")
;;  (robot-forward (make-robot 120 -15 robo-radius "SOUTH") 800)
;;    => (make-robot 120 385 robo-radius "SOUTH")
;; STRATEGY: Function Composition

(define (robot-forward rob x)
 (if (or (robot-inside-room? rob) (robot-went-inside-room? rob x)) 
     (robot-new-coordinate rob x) (robot-normal-forward rob x)))

;; TESTS

(begin-for-test
  (check-equal? (robot-forward (make-robot 150 200 robo-radius "NORTH") 800)
                (make-robot 150 15 robo-radius "NORTH") 
                "Test Failed, new y coordinate should be 15")
  (check-equal? (robot-forward (make-robot -100 285 robo-radius "EAST") 1000) 
                (make-robot 185 285 robo-radius "EAST") 
                "Test Failed, new x coordinate should be 185")
  (check-equal? (robot-forward (make-robot 120 -15 robo-radius "SOUTH") 800) 
                (make-robot 120 385 robo-radius "SOUTH")
                "Test Failed, new y coordinate should be 120")
  (check-equal? (robot-forward (make-robot 185 385 robo-radius "NORTH") 380) 
                (make-robot 185 15 robo-radius "NORTH") 
                "Test Failed, new x coordinate should be 185")
  (check-equal? (robot-forward (make-robot -40 -40 robo-radius "NORTH") 10) 
                (make-robot -40 -50 robo-radius "NORTH") 
                "Test Failed, new x coordinate should be 185"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;