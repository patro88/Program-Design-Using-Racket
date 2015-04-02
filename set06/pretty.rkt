;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;-------------------------------------------------------------------------;
;                            FILE NAME : pretty.rkt       
;-------------------------------------------------------------------------;

;-------------------------------------------------------------------------;
;                         PROBLEM STATEMENT                               ;
;-------------------------------------------------------------------------;
; Write a program that contains a pretty-printer for Exprs and follow the ;
; below rules:                                                            ;
;                                                                         ;
; 1. The expression should be rendered on a single line if it fits within ;
;    the specified width.                                                 ;
; 2. Otherwise, render the subexpressions in a stacked fashion, that is,  ;
;    like (+ expr1                                                        ;
;	          expr2                                                   ;
;	          ...                                                     ;
;	          exprN)                                                  ;
;    and similarly for multiplication expressions.                        ;
; 3. All subexpressions must fit within the space allotted minus the space;
;    for surrounding parentheses, if any. Apply the rendering algorithm   ;
;    recursively if needed.                                               ;
; 4. There should be no spaces preceding a right parenthesis.             ;
; 5. The algorithm may determine that the given expression cannot fit     ;
;    within the allotted space. In this case, the algorithm should raise  ;
;    an appropriate error, using the function error.                      ;
;                                                                         ;
; More details can be found on below link:                                ;
; http://www.ccs.neu.edu/course/cs5010f14/Problem%20Sets/ps06.html        ;
;-------------------------------------------------------------------------;

; require
(require "extras.rkt")
(require rackunit)

; provide functions
(provide expr-to-strings
         sum-exp-exprs
         make-sum-exp
         make-mult-exp
         mult-exp-exprs)

;-------------------------------------------------------------------------;
;                            GLOBAL CONSTANTS       
;-------------------------------------------------------------------------;

; constants
(define OPENING-BRACKET-SUM "(+ ")
(define OPENING-BRACKET-MULT "(* ")
(define CLOSING-BRACKET ")")
(define SINGLE-SPACE " ")
(define THREE-SPACES "   ") 
(define ERROR-MESSAGE "not enough room")
(define THREE 3)
(define ONE 1)

;-------------------------------------------------------------------------;
;                            DATA  DEFINITIONS       
;-------------------------------------------------------------------------;

(define-struct sum-exp (exprs))

; A Sum-Exp is a (make-sum-exp NonEmptyListOfExpr)

; INTERPRETATION:
; exprs is a non-empty list of expression in which the sum happens

; TEMPLATE:
; sum-exp-fn : Sum-Exp -> ??
;(define (sum-exp-fn se)
;  ( ... (sum-exp-exprs se)))
;-------------------------------------------------------------------------;

(define-struct mult-exp (exprs))

; A Mult-Exp is a (make-mult-exp NonEmptyListOfExpr)

; INTERPRETATION:
; exprs is a non-empty list of expression in which the multiplication happens

; TEMPLATE:
; mult-exp-fn : Mult-Exp -> ??
;(define (mult-exp-fn me)
;  ( ... (mult-exp-exprs me)))
;-------------------------------------------------------------------------;

; An Expr is one of 
; -- Integer                            (interp :  an integer)
; -- (make-sum-exp NonEmptyListOfExpr)  (interp :  a sum-exp represents 
;                                                  an expression which involves
;                                                  sum operation)
; -- (make-mult-exp NonEmptyListOfExpr) (interp :  a mult-exp represents 
;                                                  an expression which involves
;                                                  multiplication operation)

; TEMPLATE
; expr-fn : Expr -> ??
;(define (expr-fn e)  
;  (cond
;    [(integer? e) ...]
;    [(sum-exp? e) (...
;                     (nelst-fn1 (sum-exp-exprs e)))]
;    [(mult-exp? e) (...
;                      (nelst-fn1 (mult-exp-exprs e)))]))

; EXAMPLES for TESTS
(define expr1 (make-sum-exp
               (list
                (make-mult-exp (list 22 3333 44))
                (make-mult-exp
                 (list
                  (make-sum-exp (list 66 67 68)) 
                  (make-mult-exp (list 42 43))))
                (make-mult-exp (list 77 88)))))

(define expr2 (make-sum-exp (list 22 333 55 (make-sum-exp (list 44 40)))))  
(define sum-expr1 (make-sum-exp (list 10 20)))
(define sum-expr2 (make-sum-exp (list (make-mult-exp (list 20 30))
                                      (make-sum-exp (list 40 50)))))
(define sum-expr3 (make-sum-exp (list (make-sum-exp  
                                       (list 
                                        (make-sum-exp (list 20 30)) 
                                        (make-sum-exp (list 100 1000))))
                                      (make-sum-exp (list 40 50)))))
(define sum-expr4 (make-sum-exp (list 200 (make-sum-exp (list 10 20)))))
(define sum-expr5 (make-sum-exp 
                   (list (make-sum-exp (list 2 3)) (make-sum-exp (list 5 6)))))
(define expr4 (make-sum-exp (list (make-mult-exp (list 10 15 20))
                                  (make-mult-exp (list 100 200)))))
;-------------------------------------------------------------------------;

; A ListOfExpression (LOExpr) is one of 
; -- empty               (interp : an empty list of expressions)
; -- (cons Expr NELOExpr2)  (interp : list with an expression and a list of 
;                                  expressions.)

; TEMPLATE 
; loe-fn : LOExpr -> ??
;(define (loe-fn lst)
;  (cond
;    [(empty? lst) ...]
;    [else (...
;           (expr-fn (first lst))
;           (neloe-fn2 (rest lst)))])) 
;-------------------------------------------------------------------------;

; A NonEmptyListOfExpr (NELOExpr1) is 
; -- (cons Expr LOExpr)  (interp: a list which consists of one or more 
;                                 expressions)

; TEMPLATE
; neloe-fn1 : NELOExpr1 -> ??
;(define (neloe-fn1 nelst)
;  (...
;   (expr-fn (first nelst)) 
;   (loe-fn (rest nelst))))
;-------------------------------------------------------------------------;

; A NonEmptyListOfExpr (NELOExpr2) is either 
; -- (cons Expr empty)           (interp : a list with a single expression)
; -- (cons Expr NELOExpr2)       (interp : a list with a single expression
;                                          and a non empty list of expressions)

; TEMPLATE
; neloe-fn2 : NELOExpr2 -> ??
;(define (neloe-fn2 nelst)
;  (cond
;    [(empty? (rest nelst)) (...
;                            (expr-fn (first nelst)))]
;    [else (...
;           (expr-fn (first nelst)) 
;           (neloe-fn2 (rest nelst)))]))
;-------------------------------------------------------------------------;

; A ListOfString (LOS2) is 
; -- (cons String LOS1)          (interp : a list with a single string
;                                          and list of strings)

; TEMPLATE
; los2-fn : LOS2 -> ??
;(define (los2-fn lst)
;  (...(first lst)
;      (los1-fn (rest lst))))
;-------------------------------------------------------------------------;

; A ListOfString (LOS1) is either
; -- (cons String empty)                   (interp: an empty list)
; -- (cons String LOS1)       (interp: a string and a list of other strings)

; TEMPLATE
; los1-fn : LOS1 -> ??
;(define (los1-fn lst)
;  (cond
;    [(empty? (rest lst)) ...]
;    [else (...
;           (first lst)
;           (los1-fn (rest lst)))]))

;-------------------------------------------------------------------------;
;                        END DATA DEFINITIONS                             ;
;-------------------------------------------------------------------------;



;-------------------------------------------------------------------------;
;                            FUNCTION  DEFINITIONS        
;-------------------------------------------------------------------------;

; expr-to-strings : Expr NonNegInt -> ListOfString
; GIVEN: an expression and a width 
; RETURNS: A representation of the expression as a sequence of lines, with 
;          each line represented as a string of length not greater than the 
;          width.
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (expr-to-strings exp width)
  (expr-to-strings-helper exp width))

; TESTS:
; Constant for test
(define sum-expr1-width 20)
(define sum-expr1-result (list "(+ 10 20)"))

(begin-for-test
  
  ; Test if expression fits in the given width
  (check-equal?
   (expr-to-strings sum-expr1 sum-expr1-width)
   sum-expr1-result
   "Given expression should fit within the width"))
;-------------------------------------------------------------------------;

; expr-to-strings-helper : Expr Integer -> ListOfString
; GIVEN: a subexpression e of some expression e0 and width
; WHERE: width w is the space in which the given sub expression e should fit in
;        a single line.
; RETURNS: a representation of the given sub expression as a sequence of lines,
;          with each line represented as a string of length not greater than 
;          the width.
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (expr-to-strings-helper exp width)
  (if (<= (string-length (construct-string exp))
          width) 
      (list (construct-string exp))
      (expr-to-strings-divide exp width)))

; TESTS:
; Constant for test
(define sum-expr2-width 20)
(define sum-expr2-result (list "(+ (* 20 30)" "   (+ 40 50))"))

(begin-for-test
  
  ; Test if given expression fits in the given width
  (check-equal?
   (expr-to-strings-helper sum-expr2 sum-expr2-width)
   sum-expr2-result
   "Given expression should fit within the width"))
;-------------------------------------------------------------------------;

; construct-string : Expr -> String
; GIVEN: an expression
; RETURNS: a string which constructs the given expression in a single line
;          where the returned string consists of all characters in the given
;          expressionwith a single space between every integer, "(+", "(*" and 
;          no space between any integer and closing bracket.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on e : Expr

(define (construct-string e) 
  (cond
    [(integer? e) (number->string e)]
    [(sum-exp? e) (string-append  OPENING-BRACKET-SUM 
                                  (construct-list (sum-exp-exprs e)))]
    [(mult-exp? e) (string-append  OPENING-BRACKET-MULT 
                                   (construct-list (mult-exp-exprs e)))]))

; TESTS:
; Constants for Tests
(define integer-expr 10)
(define string-expr "10")

(begin-for-test
  
  ; Test if given expression is integer
  (check-equal?
   (construct-string integer-expr)
   string-expr
   "Given integer expression should be converted to a string 
    and returned."))
;-------------------------------------------------------------------------;

; construct-list : NonEmptyListOfExpr -> String
; GIVEN: a non-empty list of sub expressions
; RETURNS: a string which represents the given non empty list of subexpressions
;          in a single line.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on lst : NELOExpr1

(define (construct-list lst)
  (if (empty? (rest lst))
      (string-append (construct-string (first lst)) CLOSING-BRACKET) 
      (string-append
       (construct-string (first lst)) SINGLE-SPACE 
       (construct-rest-of-list (rest lst)))))

; TESTS:
; Constants for tests
(define construct-list-list1 (list 1 (make-sum-exp (list 10 20))))  
(define construct-list-string "1 (+ 10 20))")
(define construct-list-list2 (list 10))
(define construct-list-string2 "10)")

(begin-for-test
  
  ; Test if given list expression returns the correct string
  (check-equal?
   (construct-list construct-list-list1)
   construct-list-string 
   "Given non empty list of expressions should be converted into a string 
    and returned with a closing bracket.")
  
  ; Tests if given list contains a single expression
  (check-equal?
   (construct-list construct-list-list2)
   construct-list-string2
   "Given non empty list of single expression should be convertd to a string
    and returned with a closing bracket"))
;-------------------------------------------------------------------------;

; construct-rest-of-list : ListOfExpr -> String
; GIVEN: a list of sub-expressions
; RETURNS: a string which represents the given list of sub expressions
;          on a single line. 
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on lst : LOExpr

(define (construct-rest-of-list lst)
  (cond
    [(empty? lst) CLOSING-BRACKET] 
    [else (if (empty? (rest lst)) 
              (string-append (construct-string (first lst)) CLOSING-BRACKET)
              (string-append 
               (construct-string (first lst)) SINGLE-SPACE
               (construct-rest-of-list-helper (rest lst))))]))
; TESTS:
; Constants for tests
(define construct-rest-of-list-list1 (list 20 (make-mult-exp (list 20 30))))
(define construct-rest-of-list-string "20 (* 20 30))")

(begin-for-test 
  
  ; Test if given list expression returns the correct string
  (check-equal?
   (construct-rest-of-list construct-rest-of-list-list1)
   construct-rest-of-list-string
   "Given list of expressions should be converted into a string 
    and returned with a closing bracket")
  
  ; Test if given list expression is empty
  (check-equal?
   (construct-rest-of-list empty)
   CLOSING-BRACKET
   "Return a closing bracket."))
;-------------------------------------------------------------------------;

; construct-rest-of-list-helper : NonEmptyListOfExpr -> String
; GIVEN: a non-empty list of sub-expressions
; RETURNS: a string which represents the given list of sub expressions
;          on a single line. 
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on lst : NELOExpr2

(define (construct-rest-of-list-helper lst)
  (cond 
    [(empty? (rest lst)) (string-append 
                          (construct-string (first lst))
                          CLOSING-BRACKET)]
    [else (string-append
           (construct-string (first lst))
           SINGLE-SPACE
           (construct-rest-of-list-helper (rest lst)))])) 
; TESTS:
; Constants for tests
(define construct-rest-of-list-helper-list1 
  (list 20 (make-mult-exp (list 20 30))))
(define construct-rest-of-list-helper-string "20 (* 20 30))") 

(begin-for-test
  
  ; Test if given non empty list of expression is converted to the correct
  ; string.
  (check-equal?
   (construct-rest-of-list-helper construct-rest-of-list-helper-list1)
   construct-rest-of-list-helper-string
   "Given non-empty list of expressions should be converted into a string 
    and returned with a closing bracket."))
;-------------------------------------------------------------------------;

; expr-to-strings-divide : Expr Integer -> ListOfString
; GIVEN: a sub expression e of some expression e0 and width
; WHERE: width is the space needed for subexpression e to fit in a single line. 
; RETURNS: a list of strings which represents the given sub expression as a 
;          sequence of lines, with each line represented as a string of length
;          not greater than the width, otherwise an error is thrown if it is 
;          greater.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on e : Expr

(define (expr-to-strings-divide e width)
  (cond
    [(integer? e) (handle-integer e width)]
    [(sum-exp? e) (handle-exp (sum-exp-exprs e) width OPENING-BRACKET-SUM)]
    [(mult-exp? e)(handle-exp (mult-exp-exprs e) width 
                              OPENING-BRACKET-MULT)]))   

; TESTS:
; Constants for tests
(define e-to-s-d-exp1 (make-mult-exp (list 20 30 40)))
(define e-to-s-d-exp2 6)
(define e-to-s-d-integer 6)
(define e-to-s-d-listofstring1 (list "(* 20" "   30" "   40)"))
(define e-to-s-d-listofstring2 (list "6"))

(begin-for-test
  
  ; test if the given expression returns the correct string
  ; after it is split according to the given width
  (check-equal?
   (expr-to-strings-divide e-to-s-d-exp1 e-to-s-d-integer)
   e-to-s-d-listofstring1
   "Given expression should be split such that it fits 
    within the given width")
  
  ; test if the given expression if just an integer
  (check-equal?
   (expr-to-strings-divide e-to-s-d-exp2 e-to-s-d-integer)
   e-to-s-d-listofstring2
   "Given integer expression should return the list which contains
    the given integer which is converted into a string as its element."))
;-------------------------------------------------------------------------;

; handle-integer : Expr Integer -> ListOfString
; GIVEN: a sub-expression e of some expression e0 and width
; WHERE: width is the space in which given sub expression e should fit in
;        a single line.
; RETURNS: list of one string which represents the given integer subexpression
;          iff it fits within the given width
;          otherwise throws an error.
; EXAMPLE: see tests below
; STRATEGY: Function Composition

(define (handle-integer e width)
  (if (integer-fits-in-width? e width) 
      (list (number->string e))
      (error ERROR-MESSAGE))) 

; TESTS:
; Constants for tests
(define handle-integer-exp1 100)
(define handle-integer-width1 5)
(define handle-integer-width2 2)
(define handle-integer-listofstring1 (list "100"))

(begin-for-test
  
  ; test if the given integer expression fits within the given width
  (check-equal?
   (handle-integer handle-integer-exp1 handle-integer-width1)
   handle-integer-listofstring1
   "Given integer expression should be converted into a string 
    and a list with that single string should be returned.")
  
  ; test if the given integer does not fit into the given width
  (check-error
   (handle-integer handle-integer-exp1 handle-integer-width2)
   ERROR-MESSAGE))
;-------------------------------------------------------------------------;

; integer-fits-in-width? : Expr Integer -> Boolean
; GIVEN: an integer sub expression e of some expression e0 , width
; WHERE: width is the space needed for the given subexpression e to fit in
;        a single line.
; RETURNS: true iff the given integer sub-expression fits in
;           the given width else False
; EXAMPLE: see tests below
; STRATEGY: Function Composition 

(define (integer-fits-in-width? e width)
  (>= width 
      (string-length (number->string e)))) 

; TESTS: 
; Constants for tests
(define integer-fits-in-width-exp1 100)
(define integer-fits-in-width-width1 5)

(begin-for-test
  
  ; test if the given expression fits within the width
  (check-equal? 
   (integer-fits-in-width? integer-fits-in-width-exp1 
                           integer-fits-in-width-width1)
   true
   "true should be returned if the given expression can fit 
   into the given width"))
;-------------------------------------------------------------------------;  

; handle-exp : NonEmptyListOfExpr Integer String -> ListOfString
; GIVEN: a non empty list of sub expressions, width and string 
;        representing if it is a sum expression or a multiplication
;        expression.
; WHERE: width is the amount of space in which each subexpression in
;        the list of subexpressions should fit in a single line.
; RETURNS: a list of strings with opening and closing parenthesis added
;          in the beginning and at the end respectively.
;          the returned list of strings represents the given list of 
;          sub expressions as a sequence of lines, with each line represented 
;          as a string of length not greater than the width, otherwise an
;          error is thrown.  
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on lst-of-subexprs : NELOExp1

(define (handle-exp lstof-subexprs width sum-or-mult-sign) 
  (add-brackets  
   (append     
    (expr-to-strings-helper (first lstof-subexprs) (- width THREE))      
    (handle-exp-rest-of-list (rest lstof-subexprs) (- width THREE)))
   sum-or-mult-sign))

; TESTS:
; Constants for test
(define handle-exp-exp1 (list 20 (make-mult-exp (list 20 30)))) 
(define handle-exp-exp2 (make-mult-exp (list (make-mult-exp (list 20 30))
                                             (make-mult-exp (list 10 20)))))
(define handle-exp-width1 10)
(define handle-exp-width2 9)
(define handle-exp-sign "(* ")
(define handle-exp-listofstring (list "(* 20" "   (* 20" "      30))"))

(begin-for-test
  
  ; test if the given list of sub expressions returns the correct list of 
  ; strings if it fits within the given width.
  (check-equal?
   (handle-exp handle-exp-exp1 handle-exp-width1 handle-exp-sign) 
   handle-exp-listofstring
   "Given list of subexpressions should return the list of strings
    where each string represents each subexpression.")
  
  ; test if the given list of sub expressions throws an error if 
  ; expression does not fit in the given width
  (check-error
   (handle-exp handle-exp-exp2 handle-exp-width2 handle-exp-sign)
   ERROR-MESSAGE))
;-------------------------------------------------------------------------;  

; GIVEN: a list of sub expressions and width
; WHERE: width is the amount of space in which each subexpression in the 
;        given list of subexpressions should fit in a single line.
; RETURNS: a list of strings which represents the given list of subexpressions
;          as a sequence of lines, with each line represented as a string of
;          length not greater than the width, otherwise an error is thrown 
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on lstof-subexprs : LOExpr

(define (handle-exp-rest-of-list lstof-subexprs width)
  (cond
    [(empty? lstof-subexprs) empty]
    [else 
     (if (empty? (rest lstof-subexprs))
         (expr-to-strings-helper (first lstof-subexprs) (- width ONE)) 
         (append 
          (expr-to-strings-helper (first lstof-subexprs) width) 
          (handle-exp-rest-of-list-helper (rest lstof-subexprs) width)))]))

; TESTS:
; Constants for tests
(define handle-exp-rest-of-list-exp1 (list 20 (make-mult-exp (list 20 30))))
(define handle-exp-rest-of-list-width1 10)
(define handle-exp-rest-of-list-listofstring (list "20" "(* 20 30)")) 

(begin-for-test
  
  ; Test if given list of subexpressions returns the correct list of strings
  (check-equal?
   (handle-exp-rest-of-list handle-exp-rest-of-list-exp1 
                            handle-exp-rest-of-list-width1)
   handle-exp-rest-of-list-listofstring
   "Given list of subexpressions should return the list of strings
    where each string represents each subexpression.")
  
  ; Test if given list of subexpressions is empty 
  (check-equal?
   (handle-exp-rest-of-list empty handle-exp-rest-of-list-width1) empty))
;-------------------------------------------------------------------------;

; handle-exp-rest-of-list-helper : NonEmptyListOfExpr Integer -> ListOfString
; GIVEN: a list of subexpressions and width
; WHERE: width is the amount of space in which each subexpression in the 
;        given list of subexpressions should fit in
;        a single line.
; RETURNS: a list of strings which represents the given list of subexpressions
;          as a sequence of lines, with each line represented as a string of
;          length not greater than the width, otherwise an error is thrown
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on lstof-subexprs : NELOExpr2

(define (handle-exp-rest-of-list-helper lstof-subexprs width)
  (cond
    [(empty? (rest lstof-subexprs)) (expr-to-strings-helper 
                                     (first lstof-subexprs) (- width ONE))]
    [else (append
           (expr-to-strings-helper (first lstof-subexprs) width)
           (handle-exp-rest-of-list-helper (rest lstof-subexprs) width))]))

; TESTS:
; Constants for tests
(define handle-exp-restoflist-exp1 (list (make-sum-exp  
                                          (list 
                                           (make-sum-exp (list 20 30)) 
                                           (make-sum-exp (list 100 1000))))
                                         (make-sum-exp (list 40 50))))
(define handle-exp-restoflist-width1 20) 
(define handle-exp-restoflist-listofstring
  (list "(+ (+ 20 30)" "   (+ 100 1000))" "(+ 40 50)"))  

(begin-for-test
  
  ; test if list of given sub expressions fit within the width
  (check-equal?
   (handle-exp-rest-of-list-helper handle-exp-restoflist-exp1
                                   handle-exp-restoflist-width1)
   handle-exp-restoflist-listofstring
   "List of strings with subexpressions that fit within the width should 
    be returned."))
;-------------------------------------------------------------------------;

; add-brackets : ListOfString String-> ListOfString
; GIVEN: list of strings that represent list of subexpressions
;        and the string which represents either a sum or a multiplication
;        expression.
; RETURNS: a list of strings with its first element prefixed with
;          the given string.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on lst : LOS2
(define (add-brackets lst sum-or-mult-sign) 
  (cons (string-append sum-or-mult-sign (first lst)) 
        (add-bracket-rest-of-list (rest lst))))  

; TESTS:
; Constants for tests
(define add-brackets-lstofstrings1 (list "20" " 30)"))
(define add-brackets-sign "(+ ")
(define add-brackets-output (list "(+ 20" "    30))"))

(begin-for-test
  
  ; test if an opening bracket is added to the given list of strings.
  (check-equal?
   (add-brackets add-brackets-lstofstrings1 add-brackets-sign)
   add-brackets-output
   "An Opening bracket and sum sign should be prefixed to the first 
    element of the given list"))
;-------------------------------------------------------------------------;

; add-bracket-rest-of-list : ListOfString -> ListOfString 
; GIVEN: a list of strings which represents the list of subexpressions.
; RETURNS: a list of strings in which last element is suffixed with a 
;          closing parenthesis.
; EXAMPLE: see tests below
; STRATEGY: Structural Decomposition on lst : LOS1

(define (add-bracket-rest-of-list lst)
  (cond
    [(empty? (rest lst)) (list (string-append THREE-SPACES (first lst) 
                                              CLOSING-BRACKET))]  
    [else (cons (string-append THREE-SPACES (first lst))
                (add-bracket-rest-of-list (rest lst)))]))

; TESTS:
; Constants for tests
(define add-bracket-restoflist-strings1 (list "20" "20 30"))
(define add-bracket-restoflist-output (list "   20" "   20 30)"))

(begin-for-test
  
  ; Test if a closing parenthesis is added to the list of strings
  (check-equal?
   (add-bracket-rest-of-list add-bracket-restoflist-strings1)
   add-bracket-restoflist-output
   "a closing parenthesis should be suffixed to the last element in the give
    list.")) 
;-------------------------------------------------------------------------;