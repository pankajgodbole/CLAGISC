;;;
;;; EXERCISE 12.3
;;;

#|

CL-USER> (type-of 'make-starship)
SYMBOL
CL-USER> (type-of #'make-starship)
FUNCTION
CL-USER> (type-of (make-starship))
STARSHIP

|#


;;;
;;; EXERCISE 12.5
;;;

(defstruct captain :print-function print-captain
  (name)
  (age)
  (ship))

(defun print-captain (s strm dpth)
  "Takes a structure (s) a stream (strm) and a value (dpth), and prints
   the name n to strm upto nesting-depth dpth."
  (format strm "#<CAPTAIN ~a>~%" (captain-name s)))

#|

CL-USER> (defparameter *captain-kirk* 
	   (make-captain :name "James T. Kirk"
			 :age 35
			 :ship "Enterprise"))

*CAPTAIN-KIRK*
CL-USER> (print-starcraft *starship-enterprise* t 0)
#<STARCRAFT Enterprise>
NIL
CL-USER> (print-captain *captain-kirk* t 0)
#<CAPTAIN James T. Kirk>
NIL

|#




