;;;
;;;
;;; CHAPTER 3  EVAL NOTATION
;;;
;;;



;;;
;;; EXERCISE 3.15
;;;

(defun scrabble (word) (list word 'is 'a 'word))

#|

CL-USER> (scrabble 'word)
(WORD IS A WORD)

CL-USER> (scrabble word)
; Evaluation aborted on #<UNBOUND-VARIABLE WORD {1003D22143}>.

|#


(defun stooge (larry moe curly)
  (list larry moe (list 'moe curly) curly 'larry))

#|

CL-USER> (stooge 'moe 'curly 'larry)
(MOE CURLY (MOE LARRY) LARRY LARRY)

|#



;;;
;;; EXERCISE 3.16
;;;
(defun stooge-2 (larry moe curly)
  (list larry (list 'moe curly) curly 'larry))

#|

CL-USER> (stooge-2 'moe 'curly 'larry)
(MOE (MOE LARRY) LARRY LARRY)

|#



;;;
;;; EXERCISE 3.17
;;;

#|

Answer: 
T and NIL cannot be used as regular variables because T and NIL evaluate to 
themselves, whereas a regular variable needs to evaluate to whatever value it 
holds.

|#



;;;
;;; EXERCISE 3.18
;;;

#|

Answer:
1) Both functions and data can be expressed in EVAL notation.
2) EVAL notation is concise and easy to type on a keyboard.

|#




;;;
;;; EXERCISE 3.19
;;;

#|

CL-USER> (cons 'grapes '(of wrath))
(GRAPES OF WRATH)
CL-USER> (list t 'is 'not NIL)
(T IS NOT NIL)
CL-USER> (first '(list moose goose))
LIST
CL-USER> (first (list 'moose 'goose))
MOOSE
CL-USER> (cons 'home ('sweet 'home))
; in: CONS 'HOME
;     ('SWEET 'HOME)
; 
; caught ERROR:
;   illegal function call
; 
; compilation unit finished
;   caught 1 ERROR condition
; Evaluation aborted on #<SB-INT:COMPILED-PROGRAM-ERROR {1003EA2133}>.

|#




;;;
;;; EXERCISE 3.20
;;;

(defun mystery (x)
  (list (second x) (first x)))

#|

CL-USER> (mystery '(dancing bear))
(BEAR DANCING)
CL-USER> (mystery (list 'first 'second))
(SECOND FIRST)

|#



;;;
;;; EXERCISE 3.21.
;;;

#| What is wrong with each of the following function definitions?

(defun speak (x y) (list ’all ’x ’is ’y))
Answer: The symbols 'x and 'y instead of the variables x and y are used in the
        body of the function.

(defun speak (x) (y) (list ’all x ’is y))
Answer: There is a separate argument list for each argument instead of a common
        one for all arguments.

(defun speak ((x) (y)) (list all ’x is ’y))
Answer: The arguments are presented as functions (x) and (y) instead of as
        variables.
|#



;;;
;;; EXERCISE 3.25
;;;

#|

CL-USER> (list 'cons t nil)
(CONS T NIL)
CL-USER> (eval (list 'cons t nil))
(T)
CL-USER> (apply #'cons '(t nil))
(T)
CL-USER> (eval (eval (list 'cons t nil)))
; in: T
;     (T)
; 
; caught WARNING:
;   The function T is undefined, and its name is reserved by ANSI CL so that even
;   if it were defined later, the code doing so would not be portable.
; 
; compilation unit finished
;   Undefined function:
;     T
;   caught 1 WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION T {1004233393}>.
CL-USER> (eval (list 'eval nil))
NIL

|#
