;;;
;;; EXERCISE 14.1
;;;

#|

CL-USER> (defvar arg-x 2)
ARG-X
CL-USER> (ppmx (pop arg-x))
First step of expansion:
(PROG1 (CAR ARG-X) (SETQ ARG-X (CDR ARG-X)))

Final expansion:
(LET ((#:G590 (CAR ARG-X)))
  (SETQ ARG-X (CDR ARG-X))
  #:G590)

; No value

|#


;;;
;;; EXERCISE 14.2
;;;

#|

CL-USER> (ppmx (defstruct starship-2 (name NIL)(condition 'green)))
Macro expansion:
(PROGN
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
   (SB-KERNEL::%DEFSTRUCT-PACKAGE-LOCKS
    '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>))
 (SB-KERNEL::%DEFSTRUCT
  '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>
  '#(#<SB-KERNEL:LAYOUT for T {50300003}>
     #<SB-KERNEL:LAYOUT for STRUCTURE-OBJECT {50300083}>)
  (SB-C:SOURCE-LOCATION))
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
   (SB-KERNEL::%COMPILER-DEFSTRUCT
    '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>
    '#(#<SB-KERNEL:LAYOUT for T {50300003}>
       #<SB-KERNEL:LAYOUT for STRUCTURE-OBJECT {50300083}>)))
 (SB-C:XDEFUN COPY-STARSHIP-2
     :COPIER
     (SB-KERNEL:INSTANCE)
   (COPY-STRUCTURE (THE STARSHIP-2 SB-KERNEL:INSTANCE)))
 (SB-C:XDEFUN STARSHIP-2-P
     :PREDICATE
     (SB-KERNEL::OBJECT)
   (TYPEP SB-KERNEL::OBJECT 'STARSHIP-2))
 (SB-C:XDEFUN (DEFVAR STARSHIP-2-NAME)
     :ACCESSOR
     (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
   (SB-KERNEL:%INSTANCE-SET (THE STARSHIP-2 SB-KERNEL:INSTANCE) 0
                            SB-KERNEL::VALUE))
 (SB-C:XDEFUN STARSHIP-2-NAME
     :ACCESSOR
     (SB-KERNEL:INSTANCE)
   (SB-KERNEL:%INSTANCE-REF (THE STARSHIP-2 SB-KERNEL:INSTANCE) 0))
 (SB-C:XDEFUN (DEFVAR STARSHIP-2-CONDITION)
     :ACCESSOR
     (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
   (SB-KERNEL:%INSTANCE-SET (THE STARSHIP-2 SB-KERNEL:INSTANCE) 1
                            SB-KERNEL::VALUE))
 (SB-C:XDEFUN STARSHIP-2-CONDITION
     :ACCESSOR
     (SB-KERNEL:INSTANCE)
   (SB-KERNEL:%INSTANCE-REF (THE STARSHIP-2 SB-KERNEL:INSTANCE) 1))
 (SB-C:XDEFUN MAKE-STARSHIP-2
     :CONSTRUCTOR
     (&KEY ((:NAME #:NAME) NIL) ((:CONDITION #:CONDITION) 'GREEN))
   (DECLARE (SB-INT:EXPLICIT-CHECK))
   (SB-KERNEL::%MAKE-STRUCTURE-INSTANCE-MACRO
    #<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>
    '((:SLOT T . 0) (:SLOT T . 1)) #:NAME #:CONDITION))
 (SB-KERNEL::%TARGET-DEFSTRUCT
  '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>))

|#


;;;
;;; EXERCISE 14.3
;;;

(defmacro set-nil (v)
  "Takes a variable (v) as input, and generates Lisp code that sets v to NIL."
  (list 'setq v 'NIL))

#|

CL-USER> (defvar arg-c 2)
ARG-C
CL-USER> arg-c
NIL
CL-USER> (set-nil arg-c)
NIL
CL-USER> arg-c
NIL

|#


;;;
;;; EXERCISE 14.4
;;;

(defmacro simple-rotatef (x y)
  "Takes two variables (x) and (y), and swaps their values."
  `(let ((tmp ,x))
     (setq ,x ,y)
     (setq ,y tmp)))

(defmacro simple-rotatef-2 (x y)
  `(let ((tmp ,x)
	 (tmp-2 ,y))
     (setq ,x tmp-2)
     (setq ,y tmp)))

#|

CL-USER> (defvar arg-x-2 4)
ARG-X-2
CL-USER> arg-x-2
4
CL-USER> (defvar arg-y-2 111)
ARG-Y-2
CL-USER> arg-y-2
111
CL-USER> (simple-rotatef arg-x-2 arg-y-2)
4
CL-USER> (simple-rotatef-2 arg-x-2 arg-y-2)
111
CL-USER> arg-x-2
4
CL-USER> arg-y-2
111

|#


;;; A common use of macros is to avoid having to quote arguments. The macro
;;; expands into an ordinary function call with the quoted versions of the
;;; arguments filled in where needed. Backquoting can generate expressions with
;;; quotes by including the quotes as part of the template.

#|

CL-USER> '(defvar foo 'bar)
(DEFVAR FOO 'BAR)

|#


(defmacro two-from-one (f o)
  "Takes a function (f) and an object (o), and applies the function to two
   copies of the object."
  `(,f ',o ',o))

(defmacro two-from-one-2 (f o)
  `(,f ,o ,o))

(defmacro two-from-one-3 (f o)
  `(,f 'o 'o))

#|

CL-USER> '(defvar foo 'bar)
(DEFVAR FOO 'BAR)
CL-USER> (two-from-one cons aardvaark)
(AARDVAARK . AARDVAARK)
CL-USER> (ppmx (two-from-one cons aardvaark))
Macro expansion:
(CONS 'AARDVAARK 'AARDVAARK)

; No value
CL-USER> (two-from-one-2 cons 'aardvaark)
(AARDVAARK . AARDVAARK)
CL-USER> (ppmx (two-from-one-2 cons 'aardvaark))
Macro expansion:
(CONS 'AARDVAARK 'AARDVAARK)

; No value
CL-USER> (two-from-one-3 cons aardvaark)
(O . O)
CL-USER> (two-from-one-3 cons 'aardvaark)
(O . O)
CL-USER> (ppmx (two-from-one-3 cons aardvaark))
Macro expansion:
(CONS 'O 'O)

; No value

|#


;;;
;;; EXERCISE 14.5
;;;

(defmacro set-mutual (x y)
  "Takes two variables (x) and (y), and sets each variable to the name of the
   other."
  `(let ((name-1 ',x)
	 (name-2 ',y))
     (format t "~&set-mutual:~%")
     (format t "~&Before: x=~S, y=~S~%" ,x ,y)
     ;;(format t "~&Before: x=~S, y=~S~%" ',x ',y)
     ;;`(format t "~&Before: x=~S, y=~S~%" ',,x ',,y)     
     (defvar ,x name-2)
     (defvar ,y name-1)
     (format t "~&After : x=~S, y=~S~%" ,x ,y)
     (format t "~%")))

(defmacro set-mutual-progn (x y)
  `(progn
     (format t "~&set-mutual-progn:~%")
     (format t "~&Before: x=~S, y=~S, ',x=~S, ',y=~S~%" ,x ,y ',x ',y)
     (defvar ,x ',y)
     (defvar ,y ',x)
     (format t "~&After : x=~S, y=~S, ',x=~S, ',y=~S~%" ,x ,y ',x ',y)
     (format t "~%")))

(defun set-mutual-func (x y)
  "Takes two variables (x) and (y), and sets each variable to the name of the
   other."
  (format t "~&set-mutual-func:~%")
  (format t "~&x=~S, y=~S~%" x y)
  (format t "~%"))

#|

CL-USER> (defvar x-3 3)
X-3
CL-USER> (defvar y-3 4)
Y-3
CL-USER> (set-mutual x-3 y-3)
set-mutual:
Before: x=3, y=4
After : x=3, y=4

NIL
CL-USER> (defvar x-3 3)
X-3
CL-USER> (defvar y-3 4)
Y-3
CL-USER> (set-mutual-progn x-3 y-3)
set-mutual-progn:
Before: x=3, y=4, ',x=X-3, ',y=Y-3
After : x=3, y=4, ',x=X-3, ',y=Y-3

NIL
CL-USER> (macroexpand-1 '(set-mutual-progn x-3 y-3))
(PROGN
 (FORMAT T "~&set-mutual-progn:~%")
 (FORMAT T "~&Before: x=~S, y=~S, ',x=~S, ',y=~S~%" X-3 Y-3 'X-3 'Y-3)
 (DEFVAR X-3 'Y-3)
 (DEFVAR Y-3 'X-3)
 (FORMAT T "~&After : x=~S, y=~S, ',x=~S, ',y=~S~%" X-3 Y-3 'X-3 'Y-3)
 (FORMAT T "~%"))
T
CL-USER> (defvar x-3 3)
X-3
CL-USER> (defvar y-3 4)
Y-3
CL-USER> (set-mutual-func x-3 y-3)
set-mutual-func:
x=3, y=4

NIL

|#


;;;
;;; EXERCISE 14.6
;;;

(defmacro variable-chain (&rest vars)
  "Takes a number of variables (vars) as input, and generates code that sets
   each var to the name of the next one."
  `(progn
     ,@(mapcar #'(lambda (var-1 var-2) `(defvar ,var-1 ',var-2))
	       vars
	       (rotate-left vars))))

#|

CL-USER> (show-macexp (variable-chain a b c d))

(PROGN (DEFVAR A 'B) (DEFVAR B 'C) (DEFVAR C 'D) (DEFVAR D 'A))
; No value
CL-USER> (set-zero x-3 y-3 z-3)
(ZEROED X-3 Y-3 Z-3)
CL-USER> (variable-chain x-3 y-3 z-3)
Z-3
CL-USER> x-3
3
CL-USER> y-3
4

|#


;;;
;;; EXERCISE 14.7
;;;

#|

CL-USER> (initialize-fsm)
NIL
CL-USER> (defnode start)
#<Node START>
CL-USER> (defnode have-5)
#<Node HAVE-5>
CL-USER> (defnode have-10)
#<Node HAVE-10>
CL-USER> (defnode have-15)
#<Node HAVE-15>
CL-USER> (defnode have-20)
#<Node HAVE-20>
CL-USER> (defnode HAVE-25)
#<Node HAVE-25>
CL-USER> (defnode end)
#<Node END>

CL-USER> *nodes*
(#<Node START> #<Node HAVE-5> #<Node HAVE-10> #<Node HAVE-15> #<Node HAVE-20>
 #<Node HAVE-25> #<Node END>)

CL-USER> (defarc  start    coin-return  start    "Nothing to returnDDD.~%")
#<ARC START / COIN-RETURN / START>
CL-USER> (defarc  start    nickel       have-5   "Clunk!")
#<ARC START / NICKEL / HAVE-5>
CL-USER> (defarc  start    dime         have-10  "Clink!")
#<ARC START / DIME / HAVE-10>
CL-USER> (defarc  start    quarter      have-25  "Ker-chunk!")
#<ARC START / QUARTER / HAVE-25>

CL-USER> (defarc  have-5   coin-return  start    "Returned five cents.")
#<ARC HAVE-5 / COIN-RETURN / START>
CL-USER> (defarc  have-5   nickel       have-10  "Clunk!")
#<ARC HAVE-5 / NICKEL / HAVE-10>
CL-USER> (defarc  have-5   dime         have-15  "Clink!")
#<ARC HAVE-5 / DIME / HAVE-15>
CL-USER> (defarc  have-5   quarter      have-25  "Nickel returned.")
#<ARC HAVE-5 / QUARTER / HAVE-25>

CL-USER> (defarc  have-10  coin-return  start    "Returned ten cents.")
#<ARC HAVE-10 / COIN-RETURN / START>
CL-USER> (defarc  have-10  nickel       have-15  "Clunk!")
#<ARC HAVE-10 / NICKEL / HAVE-15>
CL-USER> (defarc  have-10  dime         have-20  "Clink!")
#<ARC HAVE-10 / DIME / HAVE-20>
CL-USER> (defarc  have-10  quarter      have-25  "Dime returned.")
#<ARC HAVE-10 / QUARTER / HAVE-25>

CL-USER> (defarc  have-15  coin-return  start    "Returned fifteen cents.")
#<ARC HAVE-15 / COIN-RETURN / START>
CL-USER> (defarc  have-15  nickel       have-20  "Clunk!")
#<ARC HAVE-15 / NICKEL / HAVE-20>
CL-USER> (defarc  have-15  dime         have-20  "Clink!")
#<ARC HAVE-15 / DIME / HAVE-20>
CL-USER> (defarc  have-15  quarter      have-25  "Nickel and dime returned.")
#<ARC HAVE-15 / QUARTER / HAVE-25>
CL-USER> (defarc  have-15  gum-button   end      "Delivered gum.")
#<ARC HAVE-15 / GUM-BUTTON / END>

CL-USER> (defarc  have-20  coin-return  start    "Returned twenty cents.")
#<ARC HAVE-20 / COIN-RETURN / START>
CL-USER> (defarc  have-20  nickel       have-20  "Nickel returned.")
#<ARC HAVE-20 / NICKEL / HAVE-20>
CL-USER> (defarc  have-20  dime         have-20  "Dime returned.")
#<ARC HAVE-20 / DIME / HAVE-20>
CL-USER> (defarc  have-20  quarter      have-25  "2 dimes returned.")
#<ARC HAVE-20 / QUARTER / HAVE-25>
CL-USER> (defarc  have-20  gum-button   end      "Delivered gum and nickel change.")
#<ARC HAVE-20 / GUM-BUTTON / END>
CL-USER> (defarc  have-20  mint-button  end      "Delivered mints")
#<ARC HAVE-20 / MINT-BUTTON / END>

CL-USER> (defarc  have-25  coin-return  start    "Returned twenty-five cents.")
#<ARC HAVE-25 / COIN-RETURN / START>
CL-USER> (defarc  have-25  nickel       have-25  "Nickel returned.")
#<ARC HAVE-25 / NICKEL / HAVE-25>
CL-USER> (defarc  have-25  dime         have-25  "Dime returned.")
#<ARC HAVE-25 / DIME / HAVE-25>
CL-USER> (defarc  have-25  gum-button   end      "Delivered gum and dime change.")
#<ARC HAVE-25 / GUM-BUTTON / END>
CL-USER> (defarc  have-25  mint-button  end      "Delivered mints and nickel change.")
#<ARC HAVE-25 / MINT-BUTTON / END>
CL-USER> (defarc  have-25  choc-button  end      "Delivered chocolate bar.")
#<ARC HAVE-25 / CHOC-BUTTON / END>

CL-USER> *arcs*
(#<ARC START / COIN-RETURN / START> #<ARC START / NICKEL / HAVE-5>
 #<ARC START / DIME / HAVE-10> #<ARC START / QUARTER / HAVE-25>
 #<ARC HAVE-5 / COIN-RETURN / START> #<ARC HAVE-5 / NICKEL / HAVE-10>
 #<ARC HAVE-5 / DIME / HAVE-15> #<ARC HAVE-5 / QUARTER / HAVE-25>
 #<ARC HAVE-10 / COIN-RETURN / START> #<ARC HAVE-10 / NICKEL / HAVE-15>
 #<ARC HAVE-10 / DIME / HAVE-20> #<ARC HAVE-10 / QUARTER / HAVE-25>
 #<ARC HAVE-15 / COIN-RETURN / START> #<ARC HAVE-15 / NICKEL / HAVE-20>
 #<ARC HAVE-15 / DIME / HAVE-25> #<ARC HAVE-15 / QUARTER / HAVE-25>
 #<ARC HAVE-15 / GUM-BUTTON / END> #<ARC HAVE-20 / COIN-RETURN / START>
 #<ARC HAVE-20 / NICKEL / HAVE-25> #<ARC HAVE-20 / DIME / HAVE-25>
 #<ARC HAVE-20 / GUM-BUTTON / END> #<ARC HAVE-20 / MINT-BUTTON / END>
 #<ARC HAVE-25 / COIN-RETURN / START> #<ARC HAVE-25 / NICKEL / HAVE-25>
 #<ARC HAVE-25 / DIME / HAVE-25> #<ARC HAVE-25 / GUM-BUTTON / END>
 #<ARC HAVE-25 / MINT-BUTTON / END> #<ARC HAVE-25 / CHOC-BUTTON / END>)

CL-USER> (fsm)

State START.
Input: coin-return
Nothing to return.

State START.
Input: nickel
Clunk!

State HAVE-5.
Input: nickel
Clunk!

State HAVE-10.
Input: nickel
Clunk!

State HAVE-15.
Input: nickel
Clunk!

State HAVE-20.
Input: nickel
Clunk!

State HAVE-25.
Input: nickel
Nickel returned.

State HAVE-25.
Input: choc-button
Delivered chocolate bar.
NIL
CL-USER> *current-node*
#<Node END>

|#


;;;
;;; EXERCISE 14.11
;;;

(defun from-have-5 (input-syms &aux (this-input (first input-syms)))
  "Takes a list (input-syms) of inputs, and proceeds from the HAVE-5 state
   as per input-syms."
  input-syms
  this-input)


(defun from-have-10 (input-syms &aux (this-input (first input-syms)))
  "Takes a list (input-syms) of inputs, and proceeds from the HAVE-10 state
   as per input-syms."
  input-syms
  this-input)
 
(defun from-start (input-syms &aux (this-input (first input-syms)))
  "Takes a list (input-syms) of inputs, and proceeds from the START state
   as per input-syms."
  (cond ((null input-syms) 'START)
	((equal this-input 'NICKEL)
	 (format t "~%Clunk!")
	 (from-have-5 (rest input-syms)))
	((equal this-input 'DIME)
	 (format t "~%Clink!")
	 (from-have-10 (rest input-syms)))
	((equal this-input 'COIN-RETURN)
	 (format t "~%Nothing to return!"))
	(t (error "~%No arc from START with label ~a" this-input))))

#|

(initialize-fsm)

(defnode START)
(defnode HAVE-5)
(defnode HAVE-10)
(defnode HAVE-15)
(defnode HAVE-20)
(defnode HAVE-25)
(defnode END)

CL-USER> *nodes*
(#<Node START> #<Node HAVE-5> #<Node HAVE-10> #<Node HAVE-15> #<Node HAVE-20>
 #<Node HAVE-25> #<Node END>)

(defarc  start    coin-return  start    "Nothing to return.")
(defarc  start    nickel       have-5   "Clunk!")
(defarc  start    dime         have-10  "Clink!")
(defarc  start    quarter      have-25  "Ker-chunk!")
(defarc  have-5   coin-return  start    "Returned five cents.")
(defarc  have-5   nickel       have-10  "Clunk!")
(defarc  have-5   dime         have-15  "Clink!")
(defarc  have-5   quarter      have-25  "Nickel returned.")
(defarc  have-10  coin-return  start    "Returned ten cents.")
(defarc  have-10  nickel       have-15  "Clunk!")
(defarc  have-10  dime         have-20  "Clink!")
(defarc  have-10  quarter      have-25  "Dime returned.")
(defarc  have-15  coin-return  start    "Returned fifteen cents.")
(defarc  have-15  nickel       have-20  "Clunk!")
(defarc  have-15  dime         have-25  "Clink!")
(defarc  have-15  quarter      have-25  "Nickel and dime returned.")
(defarc  have-15  gum-button   end      "Delivered gum.")
(defarc  have-20  coin-return  start    "Returned twenty cents.")
(defarc  have-20  nickel       have-25  "Clunk!")
(defarc  have-20  dime         have-25  "Nickel returned.")
(defarc  have-20  gum-button   end      "Delivered gum and nickel change.")
(defarc  have-20  mint-button  end      "Delivered mints.")
(defarc  have-25  coin-return  start    "Returned twenty-five cents.")
(defarc  have-25  nickel       have-25  "Nickel returned.")
(defarc  have-25  dime         have-25  "Dime returned.")
(defarc  have-25  gum-button   end      "Delivered gum and dime change.")
(defarc  have-25  mint-button  end      "Delivered mints and nickel change.")
(defarc  have-25  choc-button  end      "Delivered chocolate bar.")

|#


;;; a.

(defun compile-arc (arc)
  "Takes an arc (arc) as input, and returns a COND clause as per arc."
  (let* ((l       (arc-label arc))
	 (node-to (node-name (arc-to arc)))   
	 (f       (mk-smbl-frm-objcts 'from- node-to))
	 (a       (arc-action arc)))
    `((equal this-input ',l)
      (format t "~%~a" ,a)
      (,f (rest input-syms)))))

#|

CL-USER> *arcs*
(#<ARC START / COIN-RETURN / START> #<ARC START / NICKEL / HAVE-5>
 #<ARC START / DIME / HAVE-10> #<ARC START / QUARTER / HAVE-25>
 #<ARC HAVE-5 / COIN-RETURN / START> #<ARC HAVE-5 / NICKEL / HAVE-10>
 #<ARC HAVE-5 / DIME / HAVE-15> #<ARC HAVE-5 / QUARTER / HAVE-25>
 #<ARC HAVE-10 / COIN-RETURN / START> #<ARC HAVE-10 / NICKEL / HAVE-15>
 #<ARC HAVE-10 / DIME / HAVE-20> #<ARC HAVE-10 / QUARTER / HAVE-25>
 #<ARC HAVE-15 / COIN-RETURN / START> #<ARC HAVE-15 / NICKEL / HAVE-20>
 #<ARC HAVE-15 / DIME / HAVE-25> #<ARC HAVE-15 / QUARTER / HAVE-25>
 #<ARC HAVE-15 / GUM-BUTTON / END> #<ARC HAVE-20 / COIN-RETURN / START>
 #<ARC HAVE-20 / NICKEL / HAVE-25> #<ARC HAVE-20 / DIME / HAVE-25>
 #<ARC HAVE-20 / GUM-BUTTON / END> #<ARC HAVE-20 / MINT-BUTTON / END>
 #<ARC HAVE-25 / COIN-RETURN / START> #<ARC HAVE-25 / NICKEL / HAVE-25>
 #<ARC HAVE-25 / DIME / HAVE-25> #<ARC HAVE-25 / GUM-BUTTON / END>
 #<ARC HAVE-25 / MINT-BUTTON / END> #<ARC HAVE-25 / CHOC-BUTTON / END>)

CL-USER> (second *arcs*)
#<ARC START / NICKEL / HAVE-5>
CL-USER> (compile-arc (second *arcs*))
((EQUAL THIS-INPUT 'NICKEL) (FORMAT T "~a" "Clunk!")
 (FROM-HAVE-5 (REST INPUT-SYMS)))
CL-USER> (compile-arc (third *arcs*))
((EQUAL THIS-INPUT 'DIME) (FORMAT T "~a" "Clink!")
 (FROM-HAVE-10 (REST INPUT-SYMS)))
CL-USER> 
; No value
CL-USER> (compile-arc (fifth *arcs*))
((EQUAL THIS-INPUT 'COIN-RETURN) (FORMAT T "~a" "Change returned!")
 (FROM-START (REST INPUT-SYMS)))

|#


;;; b.

(defun compile-node (n)
  "Takes a node (n) as input and returns Lisp code to define the function
   which determines how to proceed from n."
  (let* ((nm           (node-name n))
	 (fn-name      (mk-smbl-frm-objcts 'from- nm))
	 (cond-clauses (mapcar #'compile-arc (node-outputs n))))
    	 
    `(defun ,fn-name (input-syms &aux (this-input (first input-syms)))
       (cond
         ;; Return the current node if there are no more inputs.
	 ((null input-syms) ',nm)
	 ,@cond-clauses
	 (t (error "~%No arc from ~a with label ~a." ',nm this-input))))))

#|

CL-USER> (compile-node (find-node 'START))
(DEFUN FROM-START (INPUT-SYMS &AUX (THIS-INPUT (FIRST INPUT-SYMS)))
  (COND ((NULL INPUT-SYMS) 'START)
        ((EQUAL THIS-INPUT 'COIN-RETURN) (FORMAT T "~a" "Nothing to return.")
         (FROM-START (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'NICKEL) (FORMAT T "~a" "Clunk!")
         (FROM-HAVE-5 (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'DIME) (FORMAT T "~a" "Clink!")
         (FROM-HAVE-10 (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'QUARTER) (FORMAT T "~a" "Ker-chunk!")
         (FROM-HAVE-25 (REST INPUT-SYMS)))
        (T (ERROR "No arc from ~a with label ~a." 'START THIS-INPUT))))

CL-USER> (compile-node (find-node 'HAVE-5))
(DEFUN FROM-HAVE-5 (INPUT-SYMS &AUX (THIS-INPUT (FIRST INPUT-SYMS)))
  (COND ((NULL INPUT-SYMS) 'HAVE-5)
        ((EQUAL THIS-INPUT 'COIN-RETURN) (FORMAT T "~a" "Returned five cents.")
         (FROM-START (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'NICKEL) (FORMAT T "~a" "Clunk!")
         (FROM-HAVE-10 (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'DIME) (FORMAT T "~a" "Clink!")
         (FROM-HAVE-15 (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'QUARTER) (FORMAT T "~a" "Nickel returned.")
         (FROM-HAVE-25 (REST INPUT-SYMS)))
        (T (ERROR "No arc from ~a with label ~a." 'HAVE-5 THIS-INPUT))))

CL-USER> (compile-node (find-node 'HAVE-25))
(DEFUN FROM-HAVE-25 (INPUT-SYMS &AUX (THIS-INPUT (FIRST INPUT-SYMS)))
  (COND ((NULL INPUT-SYMS) 'HAVE-25)
        ((EQUAL THIS-INPUT 'COIN-RETURN)
         (FORMAT T "~a" "Returned twenty-five cents.")
         (FROM-START (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'NICKEL) (FORMAT T "~a" "Nickel returned.")
         (FROM-HAVE-25 (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'DIME) (FORMAT T "~a" "Dime returned.")
         (FROM-HAVE-25 (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'GUM-BUTTON)
         (FORMAT T "~a" "Delivered gum and dime change.")
         (FROM-END (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'MINT-BUTTON)
         (FORMAT T "~a" "Delivered mints and nickel change.")
         (FROM-END (REST INPUT-SYMS)))
        ((EQUAL THIS-INPUT 'CHOC-BUTTON)
         (FORMAT T "~a" "Delivered chocolate bar.")
         (FROM-END (REST INPUT-SYMS)))
        (T (ERROR "No arc from ~a with label ~a." 'HAVE-25 THIS-INPUT))))

|#


;;; c.

(defmacro compile-machine ()
  "Produces and runs Lisp code containing a DEFUN for each node in *NODES*."
    `(progn
       ,@(mapcar #'compile-node *nodes*)))

#|

CL-USER> (from-start '(DIME DIME DIME GUM-BUTTON))

Clink!
Clink!
Nickel returned.
Delivered gum and dime change.
END
CL-USER> (from-start '(NICKEL NICKEL NICKEL GUM-BUTTON))

Clunk!
Clunk!
Clunk!
Delivered gum.
END
CL-USER> (from-start '(DIME DIME DIME CHOC-BUTTON))

Clink!
Clink!
Nickel returned.
Delivered chocolate bar.
END

|#
