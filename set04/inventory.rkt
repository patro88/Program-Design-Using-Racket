;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")))))
;-------------------------------------------------------------------------;
;                       FILE NAME: inventory.rkt                          ;
;-------------------------------------------------------------------------;


;-------------------------------------------------------------------------;
;                       PROBLEM STATEMENT                                 ;
;-------------------------------------------------------------------------;
;                                                                         ;
; Basics of Inventory management system. Inventory is a list of books.    ;
; A book will have various attributes. An order can be placed to          ;
; purchase the books from the Inventory. Inventory can reorder some of    ;
; books to fulfull the order demand. Book selling price may be increased  ;
; for some of the publisher.                                              ;
;-------------------------------------------------------------------------;

; using extras.rkt file and rackunit
(require rackunit)
(require "extras.rkt")

; providing functions
(provide inventory-potential-profit)
(provide inventory-total-volume)
(provide price-for-line-item)
(provide fillable-now?)
(provide days-til-fillable)
(provide price-for-order)
(provide inventory-after-order)
(provide increase-prices)
(provide make-book)
(provide make-line-item)
(provide reorder-present?)
(provide make-empty-reorder)
(provide make-reorder)
(provide inventory-after-deliveries)

;-------------------------------------------------------------------------;
;                             CONSTANTS                                   ;
;-------------------------------------------------------------------------;
(define ZERO 0)
(define ONE 1)
(define HUNDRED 100)

;-------------------------------------------------------------------------;
;                           DATA DEFINITIONS                              ;
;-------------------------------------------------------------------------;

(define-struct book (isbn title author publisher unit-price 
                          unit-cost on-hand reorderstatus cuft))

; A Book is a (make-book Integer String String String NonNegativeInt
;                        NonNegativeInt NonNegativeInt ReOrderStatus Real)
; INTERPRETATION:
;  isbn      : an unique integer to identify a book
;  title     : title of the book represented as string
;  author    : book author as string
;  publisher : publisher of the book as string
;  unit-price: the selling price of a book represented as (in USD*100, 
;              ie $14.99 is represented as 1499)
;  unit-cost : the cost of the book represented as (in USD*100, ie $14.99 
;              is represented as 1499)
;  on-hand   : number of copies of a book on hand
;  reorderstatus: represents re-order status of a book. If there is no
;              reorder then false else it contains the number of days left
;              to receive the book and quantity of book ordered.
;  cuft      : volume taken up by one unit of a book

; TEMPLATE:
;  book-fn : Book -> ??
;  (define (book-fn b)
;    (... (book-isbn b) (book-title b) (book-author b) (book-publisher b) 
;         (book-unit-price b) (book-unit-cost b) (book-on-hand b)
;         (book-reorderstatus b) (book-cuft b)))

; EXAMPLES for tests:
; In the reorder struct example
;-------------------------------------------------------------------------;

(define-struct reorder (days-left quantity))

; A ReOrder is a (make-reorder PosInt PosInt)

; INTERPRETATION:
;  days-left : number of days left untill next shipment
;  quantity  : number of copies of a book ordered

; TEMPLATE:
;  reorder-fn :ReOrder -> ??
;  (define (reorder-fn r)
;    (... (reorder-days-left r) (reorder-quantity r)))

; EXAMPLES for tests:
(define reorder1 (make-reorder 3 4))
(define reorder2 (make-reorder 4 1))
(define reorder3 (make-reorder 1 3))

; EXAMPLES of books for tests:
(define book1 
  (make-book 1 "CHEMISTRY" "Abhishek" "HARVARD" 100 90 20 reorder1 20))
(define book2 
  (make-book 2 "PHYSICS" "Sachin" "MIT" 100 80 20 reorder2 20))
(define book3 
  (make-book 3 "BIOLOGY" "Yuvraj" "PRINCETON" 100 80 20 reorder2 20))
(define book4 
  (make-book 4 "MATHEMATICS" "AGARWAL" "MIT" 110 80 20 reorder2 20))
(define book5 
  (make-book 5 "MATHEMATICS" "AGARWAL" "MIT" 110 80 20 reorder1 20))
(define book6 
  (make-book 4 "MATHEMATICS" "AGARWAL" "MIT" 110 80 10 reorder2 20))
(define book7 
  (make-book 5 "MATHEMATICS" "AGARWAL" "MIT" 110 80 10 reorder1 20))
(define book8 
  (make-book 3 "BIOLOGY" "Yuvraj" "PRINCETON" 110 80 20 reorder2 20))
(define book9
  (make-book 3 "BIOLOGY" "Yuvraj" "PRINCETON" 90 80 20 reorder2 20))
(define book10
  (make-book 10 "BIOLOGY" "Yuvraj" "PRINCETON" 90 80 20 reorder3 20))
(define book11
  (make-book 11 "BIOLOGY" "Yuvraj" "PRINCETON" 90 80 20 false 20))
;-------------------------------------------------------------------------;

; A ListOfBooks (LOB) is one of
; -- empty            (interp: a sequence with no elements)
; -- (cons Book LOB)  (interp: (cons Book LOB) represents a sequence whose
;                              first element is Book and whose other 
;                              elements are represented by LOB)

; TEMPLATE:
; lob-fn : LOB -> ??
; (define (lob-fn lob)
;   (cond
;     [(empty? lob) ...]
;     [else (...
;              book-fn((first lob))
;             (lob-fn (rest lob)))]))
;-------------------------------------------------------------------------;

; An Inventory is a ListOfBooks (LOB).
; WHERE: every book is identified by unique isbn

; INTERPRETATION: 
; Inventory is the list of books carried by bookstore.

; EXAMPLES for tests
(define lob1 (cons book1 (cons book2 empty)))
(define lob2 (cons book4 (cons book5 empty)))
(define lob3 (cons book6 (cons book7 empty)))
;-------------------------------------------------------------------------;

; REORDERSTATUS 

; A ReOrderStatus is one of
; -- false                       
;     interp: no pending reorder
; --(make-reorder PosInt PosInt) 
;     interp: days-left represents the time until the next shipment and 
;             quantity represents number of copies expected to arrive

; TEMPLATE:
; reorderstatus-fn : ReOrderStatus -> ??                             
;(define (reorderstatus-fn r)
;  (cond
;    [(false? r)...]
;    [(reorder? r) (...(reorder-days-left r)
;                      (reorder-quantity r))]))
;-------------------------------------------------------------------------;

(define-struct line-item (isbn order-quantity))

; A Line-Item is a (make-line-item Integer PosInt)

; INTERPRETATION:
;  isbn           : an unique integer to identify a book
;  order-quantity : number of book ordered

; TEMPLATE:
;  line-item-fn : line-item -> ??
;  (define (line-item-fn li)
;    (...  (line-item-isbn li) (line-item-order-quantity li)))

; EXAMPLES for tests:
(define lineitem1 (make-line-item 1 10))
(define lineitem2 (make-line-item 2 10))
(define lineitem3 (make-line-item 3 30))
(define lineitem4 (make-line-item 4 10))
(define lineitem5 (make-line-item 5 10))
;-------------------------------------------------------------------------;

; A List of Line-Item (LOLI) is one of:
; -- empty                 
;      interp: a sequence with no elements
; -- (cons line-item LOLI) 
;      interp: (cons Line-Item LOLI) represents a sequence whose first 
;              element is Line-Item and whose other elements are 
;              represented by LOLI)

; TEMPLATE:
;  loli-fn : LOLI -> ??
;  (define (loli-fn loli)
;    (cond
;      [(empty? loli) ...]
;      [else (...  li-fn((first loli))
;                 (loli-fn (rest loli)))]))
;-------------------------------------------------------------------------;

; An Order is a ListOfLine-Item (LOLI).
; WHERE: every book is identified by unique isbn

; INTERPRETATION: 
; Order is the list of line-item.

; EXAMPLES for tests
(define ord1 (cons lineitem1 (cons lineitem2 empty)))
(define ord2 (cons lineitem1 (cons lineitem2 (cons lineitem3 empty))))
(define ord3 (cons lineitem4 (cons lineitem5 empty)))
;-------------------------------------------------------------------------;

; A MaybeInteger is one of:
;  -- Integer    interp: result is integer
;  -- false      interp: result is false

; maybe-fn : MaybeInteger -> ??
; (define (maybe-fn mi)
;  (cond 
;   [(integer? mi)...]
;	  [(false? mi) ...]))
;-------------------------------------------------------------------------;
;                        END DATA DEFINITIONS                             ;
;-------------------------------------------------------------------------;



;-------------------------------------------------------------------------;
;                        FUNCTION DEFINITION                              ;
;-------------------------------------------------------------------------;

; inventory-potential-profit : Inventory ->  Integer
; GIVEN: an inventory
; RETURNS: the total profit, in USD*100, for all the items in stock
; EXAMPLES: (inventory-potential-profit lob1) => 600
; STRATEGY: Higher Order Function Composition

(define (inventory-potential-profit lob)
  (foldr
   ; Book Integer -> Integer
   ; GIVEN: a book and a default number Zero
   ; RETURNS: the potential profit of book
   (lambda(a b) (+ (potential-profit-helper a) b)) 
   ZERO 
   lob))

; TESTS
(begin-for-test
  (check-equal? (inventory-potential-profit lob1) 600 
                "Test failed for inventory-potential-profit"))
;-------------------------------------------------------------------------;

; potential-profit-helper : Book -> Integer
; GIVEN: a book
; RETURNS: the multiple of the book-on-hand with the difference between 
; the unit-price and unit-cost of the book
; EXAMPLES: (potential-profit-helper book1) => 200
; STRATEGY: Structural Decomposition on b : Book

(define (potential-profit-helper b)
  (* (- (book-unit-price b) (book-unit-cost b)) (book-on-hand b)))

; TESTS
(begin-for-test
  (check-equal? (potential-profit-helper book1) 200 
                "Test failed for potential-profit-helper"))
;-------------------------------------------------------------------------;

; inventory-total-volume : Inventory -> Real
; GIVEN: an inventory
; RETURNS: the total volume needed to store all the books in stock. i.e
; product of number of books to their volume
; EXAMPLES: (inventory-total-volume lob1) => 800
; STRATEGY: Higher Order Function Composition

(define (inventory-total-volume lob)
  (foldr
   ; Book Integer -> Integer
   ; GIVEN: a book and a default number Zero
   ; RETURNS: the volume of book
   (lambda (a b) (+ (total-volume-helper a) b)) 
   ZERO 
   lob))

; TESTS
(begin-for-test
  (check-equal? (inventory-total-volume lob1) 800
                "Test Failed for inventory-total-volume"))
;-------------------------------------------------------------------------;

; total-volume-helper : Book -> Real
; GIVEN: a book
; RETURNS: the total volume of a particular book in Inventroy i.e. the 
; product of the books on hand and the volume of a book
; EXAMPLES: (total-volume-helper book1) => 400
; STRATEGY: Structural Decomposition on b : Book

(define (total-volume-helper b)
  (* (book-cuft b) (book-on-hand b)))

; TESTS
(begin-for-test
  (check-equal? (total-volume-helper book1) 400
                "Test Failed for total-volume-helper"))
;-------------------------------------------------------------------------;

; price-for-line-item : Inventory Line-Item -> MaybeInteger
; GIVEN: an inventory and a line-item
; RETURNS: the price for that line item (the quantity times the unit price
; for that item).  Returns false if that isbn does not exist in the 
; inventory
; EXAMPLES: (price-for-line-item inv1 lineitem3) => false
; STRATEGY: Structural Decomposition on li : Line-Item

(define (price-for-line-item lob li)
  (line-item-helper (line-item-isbn li) (line-item-order-quantity li) lob))

; TESTS
; follow helper function
;-------------------------------------------------------------------------;

; line-item-helper : Integer PosInt Inventory -> MaybeInteger
; GIVEN: isbn, quantity of a line-item and an Inventory
; RETURNS: the price for that line item (the quantity times the unit price
; for that item).  Returns false if that isbn does not exist in the 
; inventory
; EXAMPLES: (price-for-line-item lob1 lineitem1) => 1000
; STRATEGY: Higher Order Function Composition

(define (line-item-helper isbn quantity lob)
  (foldr
   ; Book Boolean -> MaybeInteger
   ; GIVEN: a book and a boolean
   ; RETURNS: false if book isbn do not match else price of the line item
   (lambda(a b) (if(isbn-match? a isbn)(calculate-price a quantity) b)) 
   false 
   lob))

; TESTS
(begin-for-test
  (check-equal? (price-for-line-item lob1 lineitem3)
                false "Test Failed for price-for-line-item")
  (check-equal? (price-for-line-item lob1 lineitem1)
                1000 "Test Failed for price-for-line-item"))
;-------------------------------------------------------------------------;

; isbn-match? : Book Integer -> Boolean
; GIVEN: a book and a line-item isbn
; RETURNS: true iff line-item isbn matches with given book isbn else false
; EXAMPLES: (isbn-match? book1 1) => true
; STRATEGY: Structural Decomposition on b : Book

(define (isbn-match? b isbn)
  (= (book-isbn b) isbn))

; TESTS
(begin-for-test
  (check-equal? (isbn-match? book1 1) true
                "Test Failed for isbn-match?"))
;-------------------------------------------------------------------------;

; calculate-price : Book PosInt -> NonNegativeInt
; GIVEN: a Book and a quantity of book ordered in a line item
; RETURNS: the total price of the line item i.e. product of book unit
; price and the quantity of line item
; EXAMPLES: (calculate-price lob1 3) => 300
; STRATEGY: Structural Decomposition on b : Book

(define (calculate-price b quantity)
  (* (book-unit-price b) quantity))

; TESTS
(begin-for-test
  (check-equal? (calculate-price book1 3) 300
                "Test Failed for calculate-price"))
;-------------------------------------------------------------------------;

; fillable-now? : Order Inventory -> Boolean.
; GIVEN: an order and an inventory
; RETURNS: true iff there are enough copies of each book on hand to fill
; the order. If the order contains a book that is not in the inventory, 
; then the order is not fillable.
; EXAMPLES: (fillable-now? ord1 lob1) => true
;           (fillable-now? ord2 lob1) => false
; STRATEGY: Higher Order Function Composition

(define (fillable-now? loli lob)
  (andmap 
   ; Line-Item -> Boolean
   ; GIVEN: A line-item
   ; RETURNS: true if that line-item has enough copies in inventory 
   ; else false. 
   (lambda (li) (fillable-now-helper li lob))
    loli))

; TESTS
(begin-for-test
  (check-equal? (fillable-now? ord1 lob1) true 
                "Test Failed for fillable-now?")
  (check-equal? (fillable-now? ord2 lob1) false 
                "Test Failed for fillable-now?"))
;-------------------------------------------------------------------------;

; fillable-now-helper : Line-Item Inventory -> Boolean.
; GIVEN: a line-item and an inventory
; RETURNS: true iff there are enough copies of the book on hand to fill
; the order else false
; EXAMPLES: (fillable-now-helper lineitem1 lob1) => true
; STRATEGY: Higher Order Function Composition

(define (fillable-now-helper li lob)
  (ormap
   ; Book -> Boolean
   ; GIVEN: A book
   ; RETURNS: true if that book has enough copies or false.
   (lambda (b) (match-with-enough-quantity li b))
   lob))

; TESTS
(begin-for-test
  (check-equal? (fillable-now-helper lineitem1 lob1) true 
                "Test Failed for fillable-now-helper"))
;-------------------------------------------------------------------------;

; match-with-enough-quantity : Line-Item Book -> Boolean.
; GIVEN: a line-item and a book
; RETURNS: true iff there are enough copies of each book on hand to fill
; the order.  If the line-item isbn not matches to that of book isbn then
; false
; EXAMPLES: (match-with-enough-quantity lineitem1 book1) => true
; STRATEGY: Structural Decomposition on li : Line-Item

(define (match-with-enough-quantity li b)
  (and (isbn-match? b (line-item-isbn li)) 
       (enough-quantity? b (line-item-order-quantity li))))

; TESTS
; tests follow helper function
;-------------------------------------------------------------------------;

; enough-quantity? : Book PosInt -> Boolean.
; GIVEN: a book and the quantity of book order
; RETURNS: true iff there are enough copies of each book on hand to fill
; the order else false
; EXAMPLES: (enough-quantity? book1 3) => true
; STRATEGY: Structural Decomposition on b : Book

(define (enough-quantity? b quantity)
  (>= (book-on-hand b) quantity))

; TESTS
(begin-for-test
  (check-equal? (enough-quantity? book1 10) true
                "Test Failed for enough-quantity?"))
;-------------------------------------------------------------------------;

; days-til-fillable : Order Inventory -> MaybeInteger
; GIVEN: an order and an inventory
; RETURNS: the number of days until the order is fillable, assuming all
; the shipments come in on time.  Returns false if there won't be enough
; copies of some book, even after the next shipment of that book comes in.
; EXAMPLES: (days-til-fillable ord1 lob1) => 0
; STRATEGY: Higer Order Function Composition

(define (days-til-fillable loli lob)
  (foldr
   ; Line-Item Integer -> MaybeInteger
   ; GIVEN: A line-item and a default integer zero
   ; RETURNS: true if that book has enough copies or false.
   (lambda (li b) 
     (if 
      (andmap
           ; Line-Item -> Boolean
           ; GIVEN: A line-item
           ; RETURNS: true if that book has enough copies or false.
          (lambda (li) (integer? (fillable-helper li lob))) loli)
      (max (fillable-helper li lob) b) 
      false))
   ZERO
   loli))

; TESTS
(begin-for-test
  (check-equal? (days-til-fillable ord1 lob1) 0 
                "Test Failed for days-til-fillable")
  (check-equal? (days-til-fillable ord2 lob1) false 
                "Test Failed for days-til-fillable"))
;-------------------------------------------------------------------------;

; fillable-helper : Line-Item Inventory -> MaybeInteger
; GIVEN: a line-item and an inventory
; RETURNS: days left to recieve order if the total books on hand including
; reorder can fulfill the line-item order else false
; EXAMPLES: (fillable-helper lineitem1 lob1) => 0
; STRATEGY: Structural Decomposition on li : Line-Item

(define (fillable-helper li lob)
  (inventory-search (line-item-isbn li) (line-item-order-quantity li) lob))

; TESTS
; follow helper function
;-------------------------------------------------------------------------;

; inventory-search : Integer PosInt Inventory -> MaybeInteger
; GIVEN: isbn, quantity of line-item and Inventory
; RETURNS: days left to recieve order if the total books on hand including
; reorder can fulfill the line-item order else false
; EXAMPLES: (inventory-search 1 10 lob1) => 0
; STRATEGY: Higher Order Function Composition

(define (inventory-search isbn quantity lob)
  (foldr
   ; Book Boolean -> MaybeInteger
   ; GIVEN: A book and the boolean
   ; RETURNS: days-left for that reorder to fulfill else false.
   (lambda (b a) (if (book-availability? isbn quantity b) 
                     (days-left-for-order b quantity) a))
   false
   lob))

; TESTS
(begin-for-test
  (check-equal? (inventory-search 1 10 lob1) 0
                "Test Failed for inventory-search"))
;-------------------------------------------------------------------------;

; book-availability? : Integer PosInt Book -> Boolean
; GIVEN: isbn and quantity of line item and Book
; RETURNS: true iff line-item isbn matches with the book isbn and
; line-item quantity is smaller than overall books on-hand else false
; EXAMPLES: (book-availability? 1 10 book1) => true
; STRATEGY: Structural Decomposition on b : Book

(define (book-availability? isbn quantity b)
  (and (= isbn (book-isbn b))
       (<= quantity (book-after-reorder (book-on-hand b)
                                        (book-reorderstatus b)))))

; TESTS
(begin-for-test
  (check-equal? (book-availability? 1 10 book1) true
                "Test Failed for book-availability?"))
;-------------------------------------------------------------------------;

; book-after-reorder : NonNegativeInt ReOrder -> NonNegativeInt
; GIVEN: books on-hand and the reorder
; RETURNS: sum of the books on-hand and quantity of reorder
; EXAMPLES: (book-after-reorder 5 reorder1) => 9
; STRATEGY: Structural Decomposition on r : ReOrder

(define (book-after-reorder oh r)
  (+ oh (reorder-quantity r)))

; TESTS
(begin-for-test
  (check-equal? (book-after-reorder 5 reorder1) 9
                "Test Failed for book-after-reorder"))
;-------------------------------------------------------------------------;

; days-left-for-order : Book PosInt -> NonNegativeInt
; GIVEN: a book and the quantity of book in line-item
; RETURNS: 0 if the book-on-hand is greater than the quantity of 
; line-item else returns the days left to fulfill the order
; EXAMPLES: (days-left-for-order book1 21) => 3
; STRATEGY: Structural Decomposition on b : Book

(define (days-left-for-order b quantity )
  (if (>= (book-on-hand b) quantity) 
      ZERO (reorder-days (book-reorderstatus b))))

; TESTS
(begin-for-test
  (check-equal? (days-left-for-order book1 8) 0
                "Test Failed for reorder-days")
  (check-equal? (days-left-for-order book1 21) 3
                "Test Failed for reorder-days"))
;-------------------------------------------------------------------------;

; reorder-days : ReOrder -> PosInt
; GIVEN: a reorder
; RETURNS: returns the days left to fulfill the order
; EXAMPLES: (reorder-days reorder1) => 3
; STRATEGY: Structural Decomposition on r : ReOrder

(define (reorder-days r)
  (reorder-days-left r))

; TESTS
(begin-for-test
  (check-equal? (reorder-days reorder1) 3
                "Test Failed for reorder-days"))
;-------------------------------------------------------------------------;

; price-for-order : Inventory Order -> NonNegInteger
; GIVEN: an inventory and an order
; RETURNS: the total price for the given order, in USD*100.  The price 
; does not depend on whether any particular line item is in stock.  
; Line items for an ISBN that is not in the inventory count as 0.
; EXAMPLES: (price-for-order ord1 lob1) => 1700
; STRATEGY: Higher Order Function Composition

(define (price-for-order lob loli)
  (foldr
   ; Line-Item NonNegativeInt -> NonNegativeInt
   ; GIVEN: a line-item and a default number zero
   ; RETURNS: total price for the given order.
   (lambda (li b) (+ (if (integer? (price-for-line-item lob li))
                         (price-for-line-item lob li) b) b))
   ZERO loli))

; TESTS
(begin-for-test
  (check-equal? (price-for-order lob1 ord1) 2000
                "Test Failed for price-for-order")
  (check-equal? (price-for-order lob1 ord2) 2000
                "Test Failed for price-for-order")
  (check-equal? (price-for-order lob1 empty) 0
                "Test Failed for price-for-order"))
;-------------------------------------------------------------------------;

; inventory-after-order : Inventory Order -> Inventory.
; GIVEN: an inventory and an order
; WHERE: the order is fillable now
; RETURNS: the inventory after the order has been filled.
; EXAMPLES: (inventory-after-order lob2 ord3) => lob3
; STRATEGY: Higher Order Function Composition

(define (inventory-after-order lob loli)
  (map
   ;Book - > Book
   ;GIVEN: A book
   ;RETURNS: updated book after the line-item has been filled.
   (lambda (a) (inventory-after-order-helper a loli))
   lob))

; TESTS
; tests follow helper function
;-------------------------------------------------------------------------;

; inventory-after-order-helper : Book Order -> Book
; GIVEN: a book and the order
; RETURNS: a book with new book on-hand,if isbn matche and reorder is true
; EXAMPLES: (inventory-after-order-helper book1 empty) => book1
; STRATEGY: Structural Decomposition on li : Line-Item

(define (inventory-after-order-helper b loli)
  (foldr
   ; Line-Item Book -> Book
   ; GIVEN: A line-item and a book
   ; RETURN: the updated book after the line-item has been filled
   (lambda( li b) (if (match-isbn? (book-isbn b) li)
                      (new-book b (line-item-order-quantity li)) b))
   b
   loli))

; TESTS
(begin-for-test
  (check-equal? (inventory-after-order-helper book1 empty) book1
                "Test Failed for inventory-after-order-helper"))
;-------------------------------------------------------------------------;

; match-isbn? : Integer Line-Item -> Boolean
; GIVEN: book isbn and a line-item
; RETURNS: true if isbn of book and line-item matches else false
; EXAMPLES: (match-isbn? 1 lineitem1) => true
; STRATEGY: Structural Decomposition on li : Line-Item

(define (match-isbn? isbn li)
  (= isbn (line-item-isbn li)))

; TESTS
(begin-for-test
  (check-equal? (match-isbn? 1 lineitem1) true
                "Test Failed for match-isbn?"))
;-------------------------------------------------------------------------;

; new-book : Book PosInt -> Book
; GIVEN: a book and the reorder quantity 
; RETURNS: a book with book on-hand changed to book on-hand minus quantity
; EXAMPLES: (inventory-after-order lob2 ord3) => lob3
; STRATEGY: Structural Decomposition on b : Book

(define (new-book b q)
  (make-book (book-isbn b)(book-title b)(book-author b) (book-publisher b)
             (book-unit-price b)(book-unit-cost b) (- (book-on-hand b) q)
             (book-reorderstatus b) (book-cuft b)))

; TESTS
(begin-for-test
  (check-equal? (inventory-after-order lob2 ord3) lob3
                "Test Failed for inventory-after-order"))
;-------------------------------------------------------------------------;

; increase-prices : Inventory String Real -> Inventory
; GIVEN: an inventory, a publisher, and a percentage,
; RETURNS: an inventory like the original, except that all items by that
; publisher have their unit prices increased by the specified
; percentage. If the increased price is a non-integer, it may be either
; raised to the next integer price, or truncated to the next lowest
; integer price in USD*100.
; EXAMPLE: (increase-prices inventory1 "MIT Press" 10)
; returns an inventory like the original, except that all MIT Press
; books in the inventory have had their prices increased by 10%.
; STRATEGY: Higher Order Function Composition 

(define (increase-prices lob pub perc)
  (map
   ;Book -> Book
   ;GIVEN: A book
   ;RETURNS: the book with the updated unit-price
   (lambda(a) (increase-prices-helper a pub perc))
   lob))

; TESTS
(begin-for-test
  (check-equal? (increase-prices (list book3) "PRINCETON" 10) (list book8) 
                "Test Failed for increase-prices")
  (check-equal? (increase-prices (list book3) "PRINCETON" -10)(list book9) 
                "Test Failed for increase-prices"))
;-------------------------------------------------------------------------;

; increase-prices-helper : Book String Real -> Book
; GIVEN: a book, publisher name and the percent
; RETURNS: if publisher matches in inventory then the book unit-price
; shall be raised or decreased as per perctange input
; EXAMPLES: (new-book-price book3 "HARVARD" 10) => book3
; STRATEGY: Structural Decomposition on b : Book

(define (increase-prices-helper b pub perc)
  (if(string=? (book-publisher b) pub) (renewed-price b perc) b))

; TESTS
(begin-for-test
  (check-equal? (increase-prices-helper book4 "PRINCETON" 10) book4
                "Test Failed for new-book-price")
  (check-equal? (increase-prices-helper book3 "HARVARD" 10) book3
                "Test Failed for new-book-price"))
;-------------------------------------------------------------------------;

; renewed-price : Book Real -> Book
; GIVEN: a book and the percentage
; RETURNS: a book with its unit-price raised or decreased as per positive
; or negative perctange input. Calculation done as below.
; (ceiling (*(book-unit-price b)(+ ONE (/ perc HUNDRED))))
; EXAMPLES: (renewed-price book3 10) => book8
; STRATEGY: Structural Decomposition on b : Book

(define (renewed-price b perc)
  (make-book 
   (book-isbn b) (book-title b) (book-author b) (book-publisher b)
   (ceiling (*(book-unit-price b)(+ ONE (/ perc HUNDRED)))) 
   (book-unit-cost b)(book-on-hand b)(book-reorderstatus b)(book-cuft b)))

; TESTS
(begin-for-test
  (check-equal? (renewed-price book3 10) book8 
                "Test Failed for renewed-price")
  (check-equal? (renewed-price book3 -10) book9
                "Test Failed for renewed-price"))
;-------------------------------------------------------------------------;

; reorder-present? : ReorderStatus -> Boolean
; GIVEN: a reorderstatus
; RETURNS: true iff the given ReorderStatus shows a pending re-order.
; EXAMPLES: (reorder-present? false) => false
; STRATEGY: Structural Decomposition on r : ReOrderStatus

(define (reorder-present? r)
  (cond
    [(false? r)   false]
    [(reorder? r) true]))

; TESTS
(define r1 false)
(define r2 (make-reorder 1 3))

(begin-for-test
  (check-equal? (reorder-present? r1) false 
                "Test Failed for reorder-present?")
  (check-equal? (reorder-present? r2) true 
                "Test Failed for reorder-present?"))
;-------------------------------------------------------------------------;

; make-empty-reorder : Any -> ReorderStatus
; GIVEN: Any argument (to be ignored)
; RETURNS: a ReorderStatus showing no pending re-order. 
; EXAMPLES: (make-empty-reorder "Abhishek") => false
; STRATEGY: Functional Composition

(define (make-empty-reorder any)
  false)

; TESTS
(begin-for-test
  (check-equal? (make-empty-reorder "Abhishek") false
                "Test Failed for make-empty-reorder"))
;-------------------------------------------------------------------------;

; inventory-after-deliveries : Inventory -> Inventory
; GIVEN: today's inventory
; RETURNS: an Inventory representing tomorrow's inventory, in which all
;          reorders that were due in 1 day are now available, and all other
;          reorders have their expected times decreased by 1.
; EXAMPLES: (list book10) => (make-book 10 "BIOLOGY" "Yuvraj" "PRINCETON" 
;                            90 80 23 false 20)
; STRATEGY: Higher Order Function Composition

(define (inventory-after-deliveries lob)
  (map
   ;Book -> Book
   ;GIVEN: a book
   ;RETURNS: a book with updated reorderstatus
   (lambda (b) (reorder-check b (book-reorderstatus b)))
   lob))

; TESTS
; tests follow helper function
;-------------------------------------------------------------------------;

; reorder-check : Book ReorderStatus -> Book
; GIVEN: a book and the reorderstatus of that book
; RETURNS: a book, in which a reorder that is due in 1 day is now 
;          available, and the reorder status of a book with due day 
;          greater than 1 day is decreased by 1.
; EXAMPLES: (update-book book10 reorder3) => (make-book 10 "BIOLOGY" 
;            "Yuvraj" "PRINCETON" 90 80 23 false 20)
; STRATEGY: Structural Decomposition on r : ReorderStatus

(define (reorder-check b r)
  (cond
    [(false? r) b]
    [(reorder? r) (update-book b (reorder-days-left r) 
                               (reorder-quantity r))]))

; TESTS
; tests follow helper functions
;-------------------------------------------------------------------------;

; update-book : Book PosInt PosInt -> Book
; GIVEN: a book, reorder days-left and quantity
; RETURNS: a book with updated reorderstatus
; EXAMPLES: (update-book book10 1 3) => (make-book 10 "BIOLOGY" "Yuvraj" 
;           "PRINCETON" 90 80 23 false 20)
; STRATEGY: Structural Decomposition on b : Book

(define (update-book b days quantity)
  (check-for-days 
   (book-isbn b) (book-title b) (book-author b) (book-publisher b)
   (book-unit-price b) (book-unit-cost b)(book-on-hand b) 
   (book-reorderstatus b) (book-cuft b) days quantity))

; TESTS
; tests follow helper function
;-------------------------------------------------------------------------;

; check-for-days : Integer String String String NonNegativeInt 
;                  NonNegativeInt NonNegativeInt ReOrderStatus Real PosInt 
;                  PosInt
; GIVEN: a book isbn, title, author, publisher, unit-price, unit-cost,
;        on-hand, reorderstatus, cuft, reorder days-left and quantity
; RETURNS: a book with updated reorder
; EXAMPLES: (inventory-after-deliveries (list book10)) =>
;           (list (make-book 10 "BIOLOGY" "Yuvraj" "PRINCETON" 90 80 23
;                            false 20))
; STRATEGY: Function Composition

(define (check-for-days isbn title author publisher unit-price unit-cost 
                        on-hand reorderstatus cuft days quantity)
  (if (= days ONE)
      (make-book isbn title author publisher unit-price unit-cost 
                 (+ on-hand quantity) false cuft)
      (make-book isbn title author publisher unit-price unit-cost on-hand 
                 (make-reorder (- days 1) quantity) cuft)))

; TESTS
(begin-for-test
  (check-equal? (inventory-after-deliveries (list book10))
                (list (make-book 10 "BIOLOGY" "Yuvraj" "PRINCETON" 90 80 23
                                 false 20))
                "Test Failed for inventory-after-deliveries")
  (check-equal? (inventory-after-deliveries lob1)
              (list (make-book 1 "CHEMISTRY" "Abhishek" "HARVARD" 100 90 20
                                 (make-reorder 2 4) 20) 
                      (make-book 2 "PHYSICS" "Sachin" "MIT" 100 80 20 
                                 (make-reorder 3 1) 20))
                "Test Failed for inventory-after-deliveries")
  (check-equal? (inventory-after-deliveries lob3)
                (list (make-book 4 "MATHEMATICS" "AGARWAL" "MIT" 110 80 10 
                                 (make-reorder 3 1) 20) 
                      (make-book 5 "MATHEMATICS" "AGARWAL" "MIT" 110 80 10 
                                 (make-reorder 2 4) 20))
                "Test Failed for inventory-after-deliveries")
  (check-equal? (inventory-after-deliveries (list book11))
                (list (make-book 11 "BIOLOGY" "Yuvraj" "PRINCETON" 90 80 20
                                 false 20))
                "Test Failed for inventory-after-deliveries"))
;-------------------------------------------------------------------------;