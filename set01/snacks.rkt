;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snacks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; snacks.rkt

(require rackunit)
(require "extras.rkt")

(provide
    initial-machine
    machine-next-state
    machine-chocolates
    machine-carrots
    machine-bank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITION

;; A CustomerInput is one of
;; PosInt      interpretation: insert the specified number of cents
;; "chocolate" interpretation: request a chocolate bar
;; "carrots"   interpretation: request a package of carrot sticks
;; "release"   interpretation: return all the coins that the customer has put in

;; TEMPLATE:
;; customerinput-fn: CustomerInput -> ??
;; (define (customerinput-fn custinp)
;;   (cond
;;     [(integer? custinp) ...]
;;     [(string=? custinp "chocolate") ...]
;;     [(string=? custinp "carrots") ...]
;;     [(string=? custinp "release") ...]
;;    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct machine (chocolates carrots cust-input bank))

;; A machine is a (make-machine NonNegInt NonNegInt PosInt bank)
;; INTERPRETATION:
;;  chocolates is the number of chocolates available in machine
;;  carrorts is the number of carrots available in machine
;;  cust-input is the money input by customer
;;  bank is a bank where machine collects money.
;; TEMPLATE:
;;  (define (machine-fn m)
;;   (...
;;     (machine-chocolates m)
;;     (machine-carrots m)
;;     (machine-bank m)
;;   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-machine : NonNegInt NonNegInt-> Machine
;; GIVEN: the number of chocolate bars and the number of packages of
;; carrot sticks
;; RETURNS: a machine loaded with the given number of chocolate bars and
;; carrot sticks, with an empty bank and cust-input.
;; EXAMPLES: (initial-machine 10 10) => (make-machine 10 10  0 0)
;; STRATEGY: Function Composition

(define (initial-machine ch ca)
  (make-machine ch ca 0 0))

;; TESTS
(begin-for-test
  (check-equal? (initial-machine 5 4) (make-machine 5 4 0 0)
                "Test Failed, Machine should be loaded with 5 chocolates
                 and 4 carrots"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; money-inserted : machine PosInt -> Machine
;; GIVEN: a machine and the money input by customer
;; RETURNS: machine loaded with the given customer money with emtpty bank
;; EXAMPLES: (money-inserted (make-machine 10 10 0 0) 10) 
;;    => (make-machine 10 10 10 0)
;; STRATEGY: Structural Decomposition on machine(mach)

(define (money-inserted mach custinp)
  (make-machine (machine-chocolates mach) (machine-carrots mach) 
                (+ custinp (machine-cust-input mach)) (machine-bank mach)))

;; TESTS
(begin-for-test
  (check-equal? (money-inserted (make-machine 10 10 0 0) 10)
                (make-machine 10 10 10 0) "Test Failed, bank should have 0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; chocolates?: Machine -> Boolean
;; carrots?   : Machine -> Boolean
;; GIVEN: a machine
;; RETURNS: true if machine have availability of chocolates and carrots. Also, 
;; the customer money input should be greater than chocolate or carrots prices. 
;; EXAMPLES: 
;;   (chocolates? (make-machine 10 10 200 0)) => true 
;;   (carrots? (make-machine 10 0 200 0))     => false
;; STRATEGY: Structural Decomposition on machine(mach)

(define (chocolates? mach)
  (and (> (machine-chocolates mach) 0) (>= (machine-cust-input mach) 175)))

(define (carrots? mach)
  (and (> (machine-carrots mach) 0) (>= (machine-cust-input mach) 70)))

;; TESTS
(begin-for-test
  (check-equal? (chocolates? (make-machine 10 10 200 0)) true
                            "Test Failed, output should be true")
  (check-equal? (carrots? (make-machine 10 0 200 0)) false
                           "Test Failed, output should be false"))
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; release-chocolate : Machine -> Machine
;; release-carrots   : Machine -> Machine
;; GIVEN: a machine
;; RETURNS: if custinput money is equal or greater than 175 for chocolate
;; and 70 cents for carrots then output should be a machine with same as 
;; previous but with one less chocolate or carrots as per the customer 
;; requirement and bank total increased by 175 or 70 cents else nothing.
;; EXAMPLES: 
;;  (release-chocolate (make-machine 10 10 0 0) 200) 
;;       => (make-machine 9 10 25 175)
;;  (release-carrots (make-machine 10 10 0 0) 200)
;;       => (make-machine 10 9 130 70)
;; STRATEGY: Structural Decomposition on machine(mach)

(define (release-chocolate mach)
  (if (chocolates? mach)
      (make-machine (- (machine-chocolates mach) 1) (machine-carrots mach) 
                    (- (machine-cust-input mach) 175) 
                    (+ (machine-bank mach) 175))
      mach))

(define (release-carrots mach)
  (if (carrots? mach)
      (make-machine (machine-chocolates mach) (- (machine-carrots mach) 1)
                    (- (machine-cust-input mach) 70) 
                    (+ (machine-bank mach) 70))
      mach))

;; TESTS:
(begin-for-test
  (check-equal? (release-chocolate (make-machine 10 10 200 0))
                (make-machine 9 10 25 175)
                "Test Failed, bank should have 175 cents in total")
  (check-equal? (release-carrots (make-machine 10 10 200 0))
                (make-machine 10 9 130 70)
                "Test Failed, bank should have 130 cents in total")
  (check-equal? (release-chocolate (make-machine 0 0 200 0))
                (make-machine 0 0 200 0)
                "Test Failed, bank should have 0 cents in total")
  (check-equal? (release-carrots (make-machine 0 0 200 0))
                (make-machine 0 0 200 0)
                "Test Failed, bank should have 0 cents in total"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; release-money : Machine -> PosInt
;; GIVEN: a machine state
;; RETURNS: the remaining cents in the machine after purchasing
;; EXAMPLE: 
;;  (release-money (make-machine 0 0 200 0) => 200
;; STRATEGY: Structural Decomposition on machine(mach)

(define (release-money mach)
  (machine-cust-input mach))

;; TESTS
(begin-for-test
  (check-equal? (release-money (make-machine 0 0 200 0)) 200
                "Test Failed, Output should be 200"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-next-state : Machine CustomerInput -> Machine
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's input
;; EXAMPLE: 
;;  (initial-machine 10 10) => (make-machine 10 10 0 0)
;; STRATEGY: Structural Decomposition on CustomerInput (custinp)

(define (machine-next-state mach custinp)
  (cond
    [(integer? custinp) (money-inserted mach custinp)]
    [(string=? custinp "chocolate") (release-chocolate mach)]
    [(string=? custinp "carrots") (release-carrots mach)]
    [(string=? custinp "release") (release-money mach)]
    ))

;; TESTS
(begin-for-test
  (check-equal? (initial-machine 10 10) (make-machine 10 10 0 0)
                "Test Failed, 10 chocolates and 10 carrots be present")
  (check-equal? (machine-next-state
                 (machine-next-state 
                  (machine-next-state
                   (machine-next-state 
                    (machine-next-state 
                     (initial-machine 10 10) 200)
                    "chocolate") 
                   300) 
                  "carrots") 
                 "release") 255
                "Test Failed, 9 chocolates and 9 carrots be present"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 