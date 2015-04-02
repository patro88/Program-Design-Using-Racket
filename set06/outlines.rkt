;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;-------------------------------------------------------------------------;
;                       FILE NAME: outlines.rkt                           ;
;-------------------------------------------------------------------------;


;-------------------------------------------------------------------------;
;                         PROBLEM STATEMENT                               ;
;-------------------------------------------------------------------------;
; Write a program that converts from the nested list representation to the;
; flat list representation. Example shown below:                          ;
;                                                                         ;
; NESTED-LIST:                                                            ;
;(("The first section"                                                    ;
;  ("A subsection with no subsections")                                   ;
;  ("Another subsection"                                                  ;
;    ("This is a subsection of 1.2")                                      ;
;    ("This is another subsection of 1.2"))                               ;
;  ("The last subsection of 1")))                                         ;
;                                                                         ;
; FLAT-LIST:                                                              ;
;(((1) "The first section")                                               ;
; ((1 1) "A subsection with no subsections")                              ;
; ((1 2) "Another subsection")                                            ;
; ((1 2 1) "This is a subsection of 1.2")                                 ;
; ((1 2 2) "This is another subsection of 1.2")                           ;
; ((1 3) "The last subsection of 1")                                      ;
;                                                                         ;
; More details can be found on below link:                                ;
; http://www.ccs.neu.edu/course/cs5010f14/Problem%20Sets/ps06.html        ;
;-------------------------------------------------------------------------;

; require
(require "extras.rkt")
(require rackunit)

; provide functions
(provide nested-rep?)
(provide nested-to-flat)

; constants
(define ONE 1)

;-------------------------------------------------------------------------;
;                           DATA DEFINITIONS                              ;
;-------------------------------------------------------------------------;

; An Sexp is one of the following
; -- a String         interp: it is a string.
; -- a NonNegInt      interp: it is a non negative integer i.e 0, 1, etc
;                             and cannot be less than 0
; -- a ListOfSexp     interp: it is a list of Sexpression.

; TEMPLATE:

; sexp-fn : Sexp -> ??
;(define (sexp-fn s)
;  (cond
;    [(string? s) ...]
;    [(integer? s) ...]
;    [else (listofsexp-fn s)]))
;-------------------------------------------------------------------------;

; A ListOfSexp (LOS) is one of
; -- empty                      interp: list of Sexpression is empty.
; -- (cons Sexp ListOfSexp)     interp : represents a list whose first element
;                                        is Sexp and whose other elements are
;                                        represented by List of Sexpression.

; TEMPLATE:

; listofsexp-fn : ListofSexp -> ??
;(define (listofsexp-fn los)
;  (cond
;    [(empty? los) ...]
;    [else (... (first los))
;               (listofsexp-fn (rest los)))]))
;-------------------------------------------------------------------------;

; A NonNegativeInteger is a natural number starting from 0, 1, 2 etc.
; WHERE: the integer cannot be less than zero.

; A ListofIntegers (LOI) is
; -- (cons NonNegativeInteger emtpy) interp: represents a list whose first 
;                                            element is nonnegative integer
;                                            and second element is empty
; -- (cons NonNegativeInteger LOI)   interp: represents a list whose first 
;                                            element is nonnegative integer
;                                            and second element is LOI

; TEMPLATE:
; loi-fn : LOI -> ??
;(define (loi-fn loi)
;  (cond
;    [(empty? (rest loi)) ...]
;    [ ... (cons (first loi) (loi-fn (rest loi)))]))
;-------------------------------------------------------------------------;

; A NestedRep (NR) is one of the following
; -- a ListOfNestedRep       interp: represents a list of nested rep.

; WHERE: NestedRep is a subset of Sexpression.

; TEMPLATE:
; nr-fn : NestedRep -> ??
;(define (nr-fn nr)
;  (cond
;    [(empty? nr) ...]
;    [else (... (lonr-fn (first nr))
;               (nr-fn (rest nr)))]))
;-------------------------------------------------------------------------;

; A ListOfNestedRep (LONR) is one of
; -- empty                             interp: can be empty
; -- (cons String ListOfNestedRep)     interp: represents a list whose first 
;                                              element is string and the second
;                                              element is ListOfNestedRep 
; -- (cons NestedRep ListOfNestedRep)  interp: represents a nestedrep as first
;                                      element and listofnestedrep as second 
;                                      element

; WHERE: ListOfNestRep is a subset of ListOfSexpression (LOS)

; TEMPLATE:
; lonr-fn : ListOfNestedRep -> ??
;(define (lonr-fn lonr)
;  (cond
;    [(empty? lonr) ...]
;    [else (... (nr-fn (first lonr))
;               (lonr-fn (rest lonr)))]))

; EXAMPLES for tests
(define nested-list-1 '(
                        ("The first section"
                         ("A subsection with no subsections")
                         ("Another subsection"
                          ("This is a subsection of 1.2")
                          ("This is another subsection of 1.2"))
                         ("The last subsection of 1"))
                        ("The second section")
                        ))
;-------------------------------------------------------------------------;

; An FlatRep is
; -- a ListOfFlatRep       interp: represents a list of flat rep.

; WHERE: FlatRep is a subset of Sexpression

; TEMPLATE:

; fr-fn : FlatRep -> ??
;(define (fr-fn fr)
;  (cond
;    [(empty? fr) ...]
;    [(string? (second fr)) ...]
;    [else (lofr-fn fr)]))
;-------------------------------------------------------------------------;

; A ListOfFlatRep (LOFR) is one of the following
; -- empty                                     interp: can be empty
; -- (cons ListOfIntegers (cons String empty))  interp: represents a list whose 
;                                              first element is list of integers
;                                              and second elment is a list 
;                                              consist of string and empty
; -- (cons FlatRep ListOfFlatRep)      interp: represents a list whose first
;                                      element is flatlist second element is 
;                                      listofflatrep

; WHERE: ListOfFlatRep is a subset of LOS

; TEMPLATE:

; lofr-fn : ListOfFlatRep -> ??
;(define (lofr-fn lofr)
;  (cond
;    [(empty? lofr) ...]
;    [else (... (fr-fn (first lonr))
;               (lofr-fn (rest lonr)))]))

; EXAMPLES:
(define flat-list-1 '(((1) "The first section")
                      ((1 1) "A subsection with no subsections")
                      ((1 2) "Another subsection")
                      ((1 2 1) "This is a subsection of 1.2")
                      ((1 2 2) "This is another subsection of 1.2")
                      ((1 3) "The last subsection of 1")
                      ((2) "The second section")))


;-------------------------------------------------------------------------;
;                        END DATA DEFINITIONS                             ;
;-------------------------------------------------------------------------;



;-------------------------------------------------------------------------;
;                         FUNCTION DEFINITIONS                            ;
;-------------------------------------------------------------------------;

; nested-rep? : Sexp -> Boolean
; GIVEN: a sexpression
; RETURNS: true iff it is the nested representation of some outline
; EXAMPLE: (nested-rep? "abhi") => false
; STRATEGY: Structural Decomposition on s : Sexp

(define (nested-rep? s)
  (cond
    [(string? s) false]
    [(integer? s) false]
    [else (nested-rep-helper s)]))

; TESTS
(begin-for-test
  ; testing for only string input
  (check-equal? (nested-rep? "abhi") #f 
                "Test Failed for nested-rep? for string input")
  ; testing for only empty input
  (check-equal? (nested-rep? empty) true 
                "Test Failed for nested-rep? for empty input")
  ; testing for empty list
  (check-equal? (nested-rep? (list empty)) true 
                "Test Failed for nested-rep? for empty list input")
  ; testing for integer
  (check-equal? (nested-rep? 1234) false 
                "Test Failed for nested-rep? for integer input"))
;-------------------------------------------------------------------------;

; nested-rep-helper : ListOfSexp -> Boolean
; GIVEN: a list of sexpression
; RETURNS: true iff it is the nested representation of some section
; EXAMPLE: (nested-rep-helper (list "abhishek" (list empty)) => true
; STRATEGY: HOFC

(define (nested-rep-helper los)
  (andmap check-each-list los))

; TESTS
(begin-for-test
  (check-equal? (nested-rep-helper (list "NestedRep")) true 
                "Test Failed for nested-rep-helper for andmap"))
;-------------------------------------------------------------------------;

; check-each-list : Sexp -> Boolean
; GIVEN: a sexpression
; RETURNS: true iff it is the nested representation of some section
; EXAMPLE: (check-each-list "abhishek") => true
; STRATEGY: Structural Decomposition on s : Sexp

(define (check-each-list s)
  (cond
    [(string? s) true]
    [(integer? s) false]
    [else (nested-rep-helper s)]))

; TESTS
; constant for test
(define correct-los 
  (list "abhishek" 
        (list "abhishek" "kumar")
        (list "rahul" "kulkarni" (list "delhi" "elephant" "fun"))))

(begin-for-test
  ;testing for nested list
  (check-equal? (nested-rep? correct-los) true 
                "Test Failed for check-each-list for nested list")
  (check-equal? (check-each-list 12345) #f
                "Test Failed for check-each-list for integer list"))
;-------------------------------------------------------------------------;

; nested-to-flat : NestedRep -> FlatRep
; GIVEN: the representation of an outline as a nested list
; RETURNS: the flat representation of the outline
; EXAMPLE: (nested-to-flat nested-list-1) => flat-list-1    
; STRATEGY: Function Composition

(define (nested-to-flat nr)
  (ntf-helper nr (list ONE)))

;TESTS
(begin-for-test
  (check-equal? (nested-to-flat nested-list-1) flat-list-1                 
                "Test Failed for nested-to-flat"))
;-------------------------------------------------------------------------;

; ntf-helper : NestedRep ListofIntegers -> FlatRep
; GIVEN: a nestedrep and a list of integer 
; WHERE: input listofnestedrep represents depth of the nestedrep formed till 
;        the given nestedrep
; RETURNS: a flatrep representation
; EXAMPLE: (ntf-helper nested-list-1 (list 1)) => flat-list-1
; STRATEGY: Structural Decomposition on nr : NestedRep

(define (ntf-helper nr var)
  (cond
    [(empty? nr) empty]
    [else (append (process-section (first nr) var)
                  (ntf-helper (rest nr) (list-depth var)))]))

;TESTS
(begin-for-test
  (check-equal? (ntf-helper nested-list-1 (list 1)) flat-list-1              
                "Test Failed for nested-to-flat"))
;-------------------------------------------------------------------------;

; list-depth : ListOfIntegers -> ListOfIntegers
; GIVEN: a list of non negative integer.
; WHERE: the list is not empty
; RETURNS: the same list with one added to the last element of the list
; EXAMPLE: (list-depth (list 1 1)) => (list 1 2)
; STRATEGY: Structural Decomposition on var : ListOfIntegers

(define (list-depth var)
  (cond
    [(empty? (rest var)) (list (+ (first var) ONE))]
    [ else (cons (first var) (list-depth (rest var)))]))

;TESTS
(begin-for-test
  (check-equal? (list-depth (list 1 1)) '(1 2) 
                "Test Failed for list-depth"))
;-------------------------------------------------------------------------;

; process-root-level : ListOfNestedRep ListOfIntegers -> FlatRep
; GIVEN: a listofnestedrep and a list of integer
; WHERE: input ListOfIntegers represents depth of the list or the section 
;        formed till the given listofnestedrep
; RETURNS: a flatrep representation
; EXAMPLE: (process-section '("The second section") (list 1)) =>
;           (((1) "The second section"))
; STRATEGY: Structural Decomposition on sec : ListOfNestedRep

(define (process-section sec var)
  (cond
    [(empty? sec) empty]
    [else (cons (list var (first sec))
                (ntf-helper (rest sec) (append var (list ONE))))]))

;TESTS
(begin-for-test
  (check-equal? (process-section '("The second section") (list 1))
                '(((1) "The second section"))
                "Test failed for process-root-level")
  (check-equal? (process-section '() (list 1)) '()
                "Test failed for process-root-level"))
;-------------------------------------------------------------------------;