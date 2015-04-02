;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; editor.rkt

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide 
  make-editor
  editor-pre
  editor-post
  edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct editor (pre post))

;; Editor is a (make-editor String String)
;; INTERPRETATION : editor means the text in the editor is
;; (string-append s t) with the cursor between s and t
;; TEMPLATE :
;; editor-fn : editor -> ???
;; (define (editor-fn ed)
;;   ...
;;   ( .. (editor-pre ed)
;;   ( .. (editor-post ed)
;;   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cursor-pos : editor -> NonNegativeInt
;; GIVEN: An editor which conatins two parts of strings with the 
;; cursor in between.
;; RETURNS: Position of the cursor in the editor which is the 
;; string-length of editor-pre
;; EXAMPLES: 
;;  (cursor-pos (make-editor "Abhishek" "Kumar")) => 8
;;  (cursor-pos (make-editor "Hello" "Kumar")) => 5
;; STRATEGY: Structural Decomposition on editor (ed)

(define (cursor-pos ed)
  (string-length (editor-pre ed)))

;; TESTS
(begin-for-test
  (check-equal? (cursor-pos (make-editor "Abhishek" "Kumar"))
                 8 "Test Failed, Output should have been 8")
  (check-equal? (cursor-pos (make-editor "Hello" "Kumar"))
                 5 "Test Failed, Output should have been 5"))           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editor-add-key : editor keyevent -> editor
;; GIVEN: An editor and the keyevent
;; RETURNS: An editor with keyevent appended to editor-pre.
;; EXAMPLES: 
;; (editor-add-key (make-editor "Abhishek" "Kumar") a)
;;   => (make-editor "Abhisheka" "Kumar")
;; STRATEGY: Structural Decomposition on editor (ed)

(define (editor-add-key ed ke)
  (make-editor (string-append (editor-pre ed) ke) (editor-post ed)))

;; TESTS
(begin-for-test
  (check-equal? (editor-add-key (make-editor "Abhishek" "Kumar") "a")
                (make-editor "Abhisheka" "Kumar")
                "Test Failed, keyevent should have been appended to
                 editor-pre")
  (check-equal? (editor-add-key (make-editor "Hello" "Kumar") "a")
                (make-editor "Helloa" "Kumar")
                "Test Failed, keyevent should have been appended to
                 editor-pre"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editor-backspace : editor keyevent -> editor
;; GIVEN: An editor and the keyevent
;; RETURNS: an editor with last letter deleted from editor-pre
;; and rest all appended to editor-post
;; EXAMPLES:
;; (editor-backspace (make-editor "Abhishek" "Kumar") "\b")
;;   => (make-editor "Abhishe" "Kumar")
;; STRATEGY: Structural Decomposition on editor (ed)

(define (editor-backspace ed)
  (make-editor (string-left ed) (editor-post ed)))

;; TESTS
(begin-for-test
  (check-equal? (editor-backspace (make-editor "Abhishek" "Kumar"))
     (make-editor "Abhishe" "Kumar") "Test Failed, backspace not working"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string-left : editor -> String
;; GIVEN: An editor
;; RETURNS: a string of editor-pre with last letter deleted from it
;; EXAMPLES:
;; (string-left (make-editor "Abhishek" "Kumar")) => Abhishe
;; STRATEGY: Structural Decomposition on editor (ed)

(define (string-left ed)
  (substring (editor-pre ed) 0 (- (cursor-pos ed) 1)))

;; TESTS
(begin-for-test
  (check-equal? (string-left (make-editor "Abhishek" "Kumar"))
                "Abhishe" "Test Failed, Expected Abhishe"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string-last : editor -> String
;; GIVEN: An editor
;; RETURNS: the last charcter of the editor-pre
;; EXAMPLES:
;; (string-last (make-editor "Abhishek" "Kumar")) => k
;; STRATEGY: Structural Decomposition on editor

(define (string-last ed)
  (substring (editor-pre ed) (- (cursor-pos ed) 1) (cursor-pos ed)))

;; TESTS
(begin-for-test
  (check-equal? (string-last (make-editor "Abhishek" "Kumar"))
                "k" "Test Failed, Expected k"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editor-shift-cursor-left : editor -> editor
;; GIVEN: An editor
;; RETURNS: an editor with last letter of editor-pre appended to 
;; editor-post
;; EXAMPLES:
;; (editor-shift-cursor-left (make-editor "Abhishek" "Kumar") "left")
;;   => (make-editor "Abhishe" "kKumar")
;; STRATEGY: Structural Decomposition on editor (ed)

(define (editor-shift-cursor-left ed)
  (make-editor (string-left ed) (string-append (string-last ed) 
                                               (editor-post ed))))

;; TESTS
(begin-for-test
  (check-equal? (editor-shift-cursor-left (make-editor "Abhishek" "Kumar"))
                (make-editor "Abhishe" "kKumar")
                "Test Failed, Expected make-editor Abhishe kKumar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editor-shift-cursor-right : editor -> editor
;; GIVEN: An editor
;; RETURNS: an editor with first letter of editor-post appended to 
;; and rest all editor-post appended to new editor-pre
;; EXAMPLES:
;; (editor-shift-cursor-right (make-editor "Abhishek" "Kumar") "left")
;;   => (make-editor "AbhishekK" "umar")
;; STRATEGY: Structural Decomposition on editor (ed)

(define (editor-shift-cursor-right ed)
  (make-editor (string-append (editor-pre ed) (substring (editor-post ed) 0 1))
               (substring (editor-post ed) 1)))

(begin-for-test
  (check-equal? (editor-shift-cursor-right (make-editor "Abhishek" "Kumar"))
                (make-editor "AbhishekK" "umar")
                "Test Failed, Expected make-editor AbhishekK umar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; edit : editor keyevent -> editor
;; GIVEN: An editor and the keyevent
;; RETURNS: an editor which satisfies the keyevent requirement.
;; EXAMPLES:
;; (edit (make-editor "Abhishek" "Kumar") "a")
;;   => (make-editor "Abhisheka" "Kumar")
;; STRATEGY: Cases

(define (edit ed ke)
  (cond
    [(key=? ke "\b") (editor-backspace ed)]
    [(key=? ke "left") (editor-shift-cursor-left ed)]
    [(key=? ke "right") (editor-shift-cursor-right ed)]
    [(key=? ke "\n") ed]
    [(key=? ke "\t") ed]
    [(key=? ke "\u007F") ed]
    [(key=? ke "\r") ed]
    [(= (string-length ke) 1) (editor-add-key ed ke)]
    [else ed]))

;; TESTS

(begin-for-test
  (check-equal? (edit (make-editor "Abhishek" "Kumar") "a")
                (make-editor "Abhisheka" "Kumar") "Test Failed,
                 key a should have been appended to editor-pre")
  (check-equal? (edit (make-editor "Abhishek" "Kumar") "\b")
                (make-editor "Abhishe" "Kumar") "Test Failed, last
                letter of editor-pre should have been deleted")
  (check-equal? (edit (make-editor "Abhishek" "Kumar") "left")
                (make-editor "Abhishe" "kKumar") "Test Failed, last
                letter of editor-pre should have been deleted")             
  (check-equal? (edit (make-editor "Abhishek" "Kumar") "right")
                (make-editor "AbhishekK" "umar") "Test Failed, last
                letter of editor-pre should have been appened to
                editor-post")
   (check-equal? (edit (make-editor "Abhishek" "Kumar") "f12")
                (make-editor "Abhishek" "Kumar") "Test Failed,
                output editor should be same as input editor")
   (check-equal? (edit (make-editor "Abhishek" "Kumar") "\n")
                (make-editor "Abhishek" "Kumar") "Test Failed,
                output editor should be same as input editor")
   (check-equal? (edit (make-editor "Abhishek" "Kumar") "\n")
                (make-editor "Abhishek" "Kumar") "Test Failed,
                output editor should be same as input editor")
   (check-equal? (edit (make-editor "Abhishek" "Kumar") "\t")
                (make-editor "Abhishek" "Kumar") "Test Failed,
                output editor should be same as input editor")
   (check-equal? (edit (make-editor "Abhishek" "Kumar") "\r")
                (make-editor "Abhishek" "Kumar") "Test Failed,
                output editor should be same as input editor")
   (check-equal? (edit (make-editor "Abhishek" "Kumar") "\u007F")
                (make-editor "Abhishek" "Kumar") "Test Failed,
                output editor should be same as input editor")
   (check-equal? (edit (make-editor "Abhishek" "Kumar") "numlock")
                (make-editor "Abhishek" "Kumar") "Test Failed,
                output editor should be same as input editor"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;