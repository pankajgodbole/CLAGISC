;;;
;;;
;;;
;;; CHAPTER 4  CONDITIONALS
;;;
;;;
;;;



;;;
;;; EXERCISES 4.12
;;;

(defun cycle (x)
  (cond ((< x 0) 0)
	((< x 99) (+ x 1))
	((> x 99) 99)
	(t 1)))

#|

CL-USER> (cycle -100)
0
CL-USER> (cycle 100)
99
CL-USER> (cycle 99)
1
CL-USER> (cycle 1)
2
CL-USER> (cycle 98)
99

|#



;;;
;;; EXERCISE 4.13
;;;

(defun howcompute (x y z)
  (cond ((equal (+ x y) z) 'SUM-OF)
	((equal (* x y) z) 'PRODUCT-OF)
	(t '(BEATS ME))))

#|

CL-USER> (howcompute 3 4 7)
SUM-OF
CL-USER> (howcompute 3 4 5)
(BEATS ME)

|#

;;;
;;; EXERCISE 4.18
;;;

(defun rps (f s)
  (cond ((or (and (equal f 'Rock) (equal s 'Paper))
	     (and (equal f 'Paper) (equal s 'Scissors))
             (and (equal f 'Scissors) (equal s 'Rock))) '(Second wins))
        ((or (and (equal f 'Rock) (equal s 'Scissors))
	     (and (equal f 'Paper) (equal s 'Rock))
	     (and (equal f 'Scissors) (equal s 'Paper))) '(First wins))
	((equal f s) '(Tie))))

#|

CL-USER> (rps 'Paper 'Rock)
(FIRST WINS)

|#



;;;
;;; EXERCISE 4.19
;;;

(defun cond-for-and (x y z w)
  (cond (x (cond (y (cond (z (cond (w w)))))))
	(t nil)))

#|

CL-USER> (and 'george nil 'harry 'abe)
NIL
CL-USER> (cond-for-and 'george nil 'harry 'abe)
NIL
CL-USER> (and 'george 'jack 'harry 'abe)
ABE
CL-USER> (cond-for-and 'george 'jack 'harry 'abe)
ABE

|#



;;;
;;; EXERCISE 4.27
;;;

#|

Answer
NIL

|#



;;;
;;; EXERCISE 4.28
;;;

(defun if-2 (p)
  (if p
      (evenp 7)
      '(Predicate is false)))

#|

CL-USER> (if-2 (oddp 5))
NIL

|#


(defun or-and-for-if (p)
  (or (and p (evenp 7)) '(Predicate is false)))

#|

CL-USER> (or-and-for-if (oddp 5))
(PREDICATE IS FALSE)

|#


;;; Corrected function
(defun or-and-for-if-2 (p)
  (or (and p (oddp 7)) nil))

#|

CL-USER> (or-and-for-if-2 (evenp 5))
NIL

|#



;;;
;;; EXERCISE  4.29 
;;;

(defun logical-and-using-cond (x y)
  "Takes two logical inputs (x) and (y), and implements logical-and by using
   COND instead of AND."
  (cond (x (cond (y t)))))

#|

CL-USER> (logical-and-using-cond *true* *true*)
T
CL-USER> (logical-and-using-cond *true* *nil*)
NIL
CL-USER> (logical-and-using-cond *nil* *nil*)
NIL
CL-USER> (logical-and-using-cond *nil* *true*)
NIL

|#

(defun logical-and-using-if (x y)
  "Takes two logical inputs (x) and (y), and implements logical-and by using
   IF instead of AND."
  (if x (if y t)))
;LOGICAL-AND-USING-IF



;;;
;;; EXERCISE  4.30 
;;;

(defun logical-or (x y)
  "Takes two logical inputs (x) and (y), and implements logical-or."
  (cond (x t)
	(y t)
	(t NIL)))

#|

CL-USER> (logical-or NIL NIL)
NIL
CL-USER> (logical-or NIL T)
T
CL-USER> (logical-or T NIL)
T
CL-USER> (logical-or T T)
T

|#


;;;
;;; EXERCISE  4.31 
;;;

#|

Is NOT a conditional?
=> No. NOT is a function because it always evaluates its input.

Is it a boolean function?
=> Yes.

Do you need to write a LOGICAL-NOT function?
=> No. Because it only returns T or NIL.

|#



;;;
;;; EXERCISE 4.37
;;;

(defun logical-and-using-nand (x y)
  "Takes two logical inputs (x) and (y), and implements LOGICAL-AND using the
   NAND function."
  (nand (nand x y) T))
 
;LOGICAL-AND-USING-NAND

#|

CL-USER> (logical-and-using-nand NIL NIL)
NIL
CL-USER> (logical-and-using-nand T T)
T
CL-USER> (logical-and-using-nand T NIL)
NIL
CL-USER> (logical-and-using-nand NIL T)
NIL

|#


(defun logical-or-using-nand (x y)
  "Takes two logical inputs (x) and (y), and implements LOGICAL-OR using the
   NAND function."
  (nand (nand x x) (nand y y)))
;LOGICAL-OR-USING-NAND

#|

CL-USER> (logical-or-using-nand NIL NIL)
NIL
CL-USER> (logical-or-using-nand NIL T)
T
CL-USER> (logical-or-using-nand T NIL)
T
CL-USER> (logical-or-using-nand T T)
T

|#



;;;
;;; EXERCISE 4.38
;;;

(defun nor (x y)
  (not (or x y)))
;NOR 


(defun not-using-nor (x)
  (nor x x))

#|

CL-USER> (not-using-nor *nil*)
T
CL-USER> (not-using-nor *true*)
NIL

|#


(defun logical-and-using-nor (x y)
  "Takes two logical inputs (x) and (y), and implements the LOGICAL-AND 
   function using only the NOR function.

   (and (nor (not x)   (not y)))
   (and (nor (nor x x) (nor y y)))"
  (nor (nor x x) (nor y y)))
;LOGICAL-AND-USING-NOR

#|

CL-USER> (logical-and-using-nor NIL NIL)
NIL
CL-USER> (logical-and-using-nor NIL T)
NIL
CL-USER> (logical-and-using-nor T NIL)
NIL
CL-USER> (logical-and-using-nor T T)
T

|#


(defun logical-or-using-nor (x y)
  "Takes two logical inputs (x) and (y), and implements the LOGICAL-OR
   function using only the NOR function.

   (or (not (nor x y)))
   (or (nor (nor x y) (nor x y))"
   (nor (nor x y) (nor x y)))
;LOGICAL-OR-USING-NOR


#|

CL-USER> (logical-or-using-nor NIL NIL)
NIL
CL-USER> (logical-or-using-nor NIL T)
T
CL-USER> (logical-or-using-nor T NIL)
T
CL-USER> (logical-or-using-nor T T)
T

|#


(defun nand-using-nor (x y)
  "Takes two logical inputs (x) and (y), and implements the NAND function
   using only the NOR function.

   (nand (not (and x y)))
   (nand (not (nor (nor x x) (nor y y))))
   (nand (nor (nor (nor x x) (nor y y)) (nor (nor x x) (nor y y))))"
  (nor (nor (nor x x) (nor y y))
       (nor (nor x x) (nor y y))))
;NAND-USING-NOR

#|

CL-USER> (nand-using-nor NIL NIL)
T
CL-USER> (nand-using-nor NIL T)
T
CL-USER> (nand-using-nor T NIL)
T
CL-USER> (nand-using-nor T T)
NIL

|#



;;;
;;; EXERCISE 4.39
;;;

#|

Answer:
LOGICAL-AND is not logically complete as one cannot construct NOT using
LOGICAL-ANDs. Therefore we cannot construct OR, AND and NOR using LOGICAL_ANDs.

|#
