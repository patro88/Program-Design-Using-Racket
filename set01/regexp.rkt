;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname regexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; regexp.rkt

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide
 next-state
 accepting-state?
 error-state?
 initial-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION

;;  State is the current state of the machine. Machine can have below
;;  mentioned states.
;;  IN : Initial State, expect to see 'a' or 'b'
;;  AB : expect to see: 'a', 'b', 'c' or 'd'
;;  CD : expect to see: 'c','d' or 'e'
;;  E  : encountered a 'e', finished
;;  ER : error, user pressed illegal key

;; TEMPLATE:
;; state-fn : State -> ??
;; (define (state-fn st Keyevent)
;;   (cond
;;    [(string=? st "IN") ...]
;;    [(string=? st "AB") ...]
;;    [(string=? st "CD") ...]
;;    [(string=? st "E")  ...]
;;    [(string=? st "ER") ...]
;;   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;; of your machine.  The given number is ignored.
;; EXAMPLE:
;;  (initial-state 10) => "IN"
;; STRATEGY: Functional Composition

(define (initial-state num)
   "IN")

;; TESTS
(begin-for-test
  (check-equal? (initial-state 10) "IN" "Test Failed, Expected state is IN"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ke-ab?    : Keyevent -> Boolean
;; ke-cd?    : Keyevent -> Boolean
;; ke-e?     : Keyevent -> Boolean

;; GIVEN: a Keyevent.
;; RETURNS: ke-ab? : true, when Keyevent is a or b else false.
;;          ke-cd? : true, when Keyevent is c or d else false.
;;          ke-e?  : true, when Keyevent is e else false.
;; EXAMPLE:
;;   (ke-ab? "a") => true
;;   (ke-cd? "c") => true
;; STRATEGY: Function Composition

(define (ke-ab? ke)
  (or (key=? ke "a") (key=? ke "b")))

(define (ke-cd? ke)
  (or (key=? ke "c") (key=? ke "d")))

(define (ke-e? ke)
  (if (key=? ke "e") true false))

;; TESTS
(begin-for-test
  (check-equal? (ke-ab? "a") true "Test Failed, output should be true")
  (check-equal? (ke-ab? "b") true "Test Failed, output should be true")
  (check-equal? (ke-cd? "c") true "Test Failed, output should be true")
  (check-equal? (ke-cd? "d") true "Test Failed, output should be true")
  (check-equal? (ke-e? "e") true "Test Failed, output should be true")
  (check-equal? (ke-e? "f") false "Test Failed, output should be true")
  (check-equal? (ke-ab? "f") false "Test Failed, output should be true")
  (check-equal? (ke-cd? "f") false "Test Failed, output should be true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state-ab : state KeyEvent -> State
;; state-cd : State KeyEvent -> State
;; state-e  : State KeyEvent -> State
;; state-er : State KeyEvent -> State
;; GIVEN: a state of the machine and a key event.
;; RETURNS: the state that should follow the given key event.
;; EXAMPLE:
;;  (state-ab "IN" "a") => "AB"
;;  (state-ab "AB" "a") => "AB"
;;  (state-ab "AB" "c") => "CD"
;;  (state-cd "CD" "c") => "CD"
;;  (state-cd "CD" "e") => "E"
;; STRATEGY: Function Composition

(define (state-ab st ke)
  (if(= (string-length ke) 1) 
     (if (ke-ab? ke) "AB" "ER")
     st))

(define (state-cd st ke)
  (if(= (string-length ke) 1) 
     (cond
       [(ke-cd? ke) "CD"]
       [(ke-ab? ke) st]
       [else "ER"])
     st))

(define (state-e st ke)
  (if(= (string-length ke) 1)
     (cond
       [(ke-cd? ke) st]
       [(ke-e? ke)  "E"]
       [else "ER"])
     st))

(define (state-er st ke)
  (if(= (string-length ke) 1) "ER" st))

;; TESTS
(begin-for-test
  (check-equal? (state-ab "IN" "a") "AB"
                "Test Failed, Expected state is AB")
  (check-equal? (state-ab "IN" "b") "AB"
                "Test Failed, Expected state is AB")
  (check-equal? (state-ab "IN" "c") "ER"
                "Test Failed, Expected state is ER")
  (check-equal? (state-cd "AB" "a") "AB"
                "Test Failed, Expected state is AB")
  (check-equal? (state-cd "AB" "c") "CD"
                "Test Failed, Expected state is CD")
  (check-equal? (state-e "CD" "c") "CD"
                "Test Failed, Expected state is CD")
  (check-equal? (state-e "CD" "e") "E"
                "Test Failed, Expected state is E")
  (check-equal? (state-er "E" "a") "ER"
                "Test Failed, Expected state is ER")
  (check-equal? (state-e "CD" "a") "ER"
                "Test Failed, Expected state is ER")
  (check-equal? (state-er "E" "aa") "E"
                "Test Failed, Expected state is E")
  (check-equal? (state-ab "IN" "aa") "IN"
                "Test Failed, Expected state is IN")
  (check-equal? (state-cd "AB" "aa") "AB"
                "Test Failed, Expected state is AB")
  (check-equal? (state-e "CD" "aa") "CD"
                "Test Failed, Expected state is CD")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; EXAMPLE: 
;;  (accepting-state? "E")) => true
;; STRATEGY: Function Composition

(define (accepting-state? st)
  (if (string=? st "E") true false))

;; TESTS
(begin-for-test
  (check-equal? (accepting-state? "E") true 
                  "Test Failed, output should be true")
  (check-equal? (accepting-state? "AB") false 
                  "Test Failed, output should be false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the string seen so far does not match the specified
;; regular expression and cannot possibly be extended to do so.
;; EXAMPLE: 
;;  (error-state? "ER") => true
;; STRATEGY: Function Composition

(define (error-state? st)
  (if (string=? st "ER") true false))

;; TESTS
(begin-for-test
  (check-equal? (error-state? "ER") true 
                  "Test Failed, output should be true")
  (check-equal? (error-state? "E") false 
                  "Test Failed, output should be false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-state : State KeyEvent -> State
;; GIVEN: a state of the machine and a key event.
;; RETURNS: the state that should follow the given key event.  A key
;; event that is to be discarded should leave the state unchanged.
;; EXAMPLE:
;;  (next-state "IN" "a") => "AB"
;;  (next-state "IN" "f") => "ER"
;;  (next-state "AB" "c") => "CD"
;;  (next-state "AB" "g") => "ER"
;;  (next-state "CD" "d") => "CD"
;;  (next-state "CD" "e") => "E"
;;  (next-state  "E" "a") => "ER"

;; STRATEGY: Structural Decomposition on state (st)

(define (next-state st ke)
  (cond
    [(string=? st "IN") (state-ab st ke)]
    [(string=? st "AB") (state-cd st ke)]
    [(string=? st "CD") (state-e  st ke)]
    [(string=? st "E")  (state-er st ke)]
    [(string=? st "ER") st]))

;; TESTS
(begin-for-test
  (check-equal? (next-state "IN" "a") "AB"
        "Test Failed, Next state should be AB")
  (check-equal? (next-state "IN" "f") "ER"
        "Test Failed, Next state should be ER")
  (check-equal? (next-state "AB" "c") "CD"
        "Test Failed, Next state should be CD")
  (check-equal? (next-state "AB" "g") "ER"
        "Test Failed, Next state should be ER")
  (check-equal? (next-state "CD" "d") "CD"
        "Test Failed, Next state should be CD")
  (check-equal? (next-state "CD" "e") "E"
         "Test Failed, Next state should be E")
  (check-equal? (next-state "E" "a") "ER"
        "Test Failed, Next state should be ER")
  (check-equal? (next-state "AB" "t") "ER"
        "Test Failed, Next state should be ER")
  (check-equal? (next-state "AB" "f13") "AB"
        "Test Failed, Next state should be ER")
  (check-equal? (next-state "AB" "\t") "ER"
        "Test Failed, Next state should be ER")
  (check-equal? (next-state "ER" "\t") "ER"
        "Test Failed, Next state should be ER"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;