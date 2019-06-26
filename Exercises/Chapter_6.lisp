;;;
;;; EXERCISE 6.3
;;;

#|

CL-USER> (last '(rosebud))
(ROSEBUD)

|#



;;;
;;; EXERCISE 6.4
;;;

#|

CL-USER> (last '((rosebud)))
((ROSEBUD))

|#



;;;
;;; EXERCISE 6.6
;;;

(defun last-element (l)
  (car (last l)))

#|

CL-USER> (last-element '(1 2 3 4 5 6))
6

|#



;;;
;;; EXERCISE 6.8
;;;

(defun my-butlast (l)
  (if (null l)
      NIL
      (reverse (cdr (reverse l)))))

#|

CL-USER> (my-butlast NIL)
NIL
CL-USER> (my-butlast '(a))
NIL
CL-USER> (my-butlast '(a b c d))
(A B C)
CL-USER> (my-butlast '(roses are red))
(ROSES ARE)
CL-USER> (my-butlast '(g a g a))
(G A G)

|#



;;;
;;; EXERCISE 6.10
;;;

(defun palindromep (list)
  (equal list (reverse list)))

#|

CL-USER> (palindromep (list 'a))
T
CL-USER> (palindromep (list '(a b c d c b a)))
T

|#




;;;
;;; EXERCISE 6.11
;;;

(defun make-palindrome (x)
  (if (atom x)
      (cons x x)
      (append x (reverse x))))

#|

CL-USER> (make-palindrome NIL)
(NIL)
CL-USER> (make-palindrome 'a)
(A . A)
CL-USER> (make-palindrome '(a b r a c a d))
(A B R A C A D D A C A R B A)
CL-USER> (make-palindrome '(You and me))
(YOU AND ME ME AND YOU)

|#



;;;
;;; EXERCISE 6.12
;;;

#|

Answer:
MEMBER does not need to copy its input, because the result is obtained by
simply moving the pointer to the CAR of the sublist containing the member item
or to that of nil, if the item is absent.

|#



;;;
;;; EXERCISE 6.13
;;;

(intersection '(fred john mary) nil)
;NIL



;;;
;;; EXERCISE 6.14
;;;

#|

CL-USER> (intersection '(fred john mary) '(fred john mary))
(MARY JOHN FRED)

|#



;;;
;;; EXERCISE 6.16
;;;

#|

CL-USER> (intersection '(fred john mary) '(fred john mary))
(MARY JOHN FRED)

|#

(union '(fred john mary) nil)
;(MARY JOHN FRED)



;;;
;;; EXERCISE 6.17
;;;

#|

CL-USER> (union '(fred john mary) '(fred john mary))
(FRED JOHN MARY)

|#


;(FRED JOHN MARY)



;;;
;;; EXERCISE 6.18
;;;

(defun add-vowels (letters)
  (union letters '(a e i o u)))

#|

CL-USER> (add-vowels '(x a e z))
(U O I X A E Z)

|#


;;;
;;; EXERCISE 6.19
;;;

#|

CL-USER> (set-difference '(echo alpha foxtrot) nil)
(ECHO ALPHA FOXTROT)
CL-USER> (set-difference nil '(echo alpha foxtrot))
NIL

|#



;;;
;;; EXERCISE 6.20
;;;

#|

Answer:

SET-DIFFERENCE needs to ever copy only its first input. It never needs to copy
its second input, since the difference is always in terms of the first input.
SET-DIFFERENCE copies its first input when the difference is a subset of the
first input.

|#



;;;
;;; EXERCISE 6.21
;;;

(defun my-subsetp (x y)
  (equal (set-difference x y) nil))

#|

CL-USER> (my-subsetp '(a i) '(a e i o u))
T
CL-USER> (my-subsetp '(a x) '(a e i o u))
NIL

|#



;;;
;;; EXERCISE 6.24
;;;

(defun set-equal (x y)
  "Returns T if X is equal to Y irrespective of the order of their elements."
  (and (subsetp x y) (subsetp y x)))

#|

CL-USER> (set-equal '(red green blue) '(green blue red))
T

|#



;;;
;;; EXERCISE 6.25
;;;

(defun proper-subsetp (x y)
  "Returns T if X is a subset of Y, but X is not equal to Y."
  (and (subsetp x y)
       (< (length x) (length y))))

#|

CL-USER> (proper-subsetp '(a e) '(a e i o u))
T
CL-USER> (proper-subsetp '(a e i o u) '(a e i o u))
NIL

|#



;;;
;;; EXERCISE 6.34
;;;

(defvar atlas '((pennsylvania pittsburgh johnstown)
		(new-jersey newark princeton trenton)
		(ohio columbus)))

(defun cities-in (s)
  (first (rest (assoc s atlas))))

#|

CL-USER> (cities-in 'pennsylvania)
PITTSBURGH

|#


;;;
;;; EXERCISE 6.36
;;;

(defun swap-first-last (l)
  (let ((len (length l)))
    (cond ((or (equal len 0) (equal len 1)) l)
	  ((equal len 2) (reverse l))
	  (t (swp l)))))
(defun swp (l)
  (let ((fst (first l))
	(lst (first (reverse l)))
	(mid (reverse (rest (reverse (rest l))))))
    (append (list lst) mid (list fst))))
#|
    (defvar (first l) lst)
    (let ((r-l (reverse l)))
      (defvar (first r-l) fst)
      (reverse r-l))))
|#

#|

CL-USER> (swap-first-last '(love loev evol ovel))
(OVEL LOEV EVOL LOVE)

|#



;;;
;;; EXERCISE 6.37
;;;

(defun rotate-left (l)
  (append (rest l) (list (first l))))

#|

CL-USER> (rotate-left '(a b a b))
(B A B A)

|#

(defun rotate-right (l)
  (let ((len (length l)))
    (cond ((or (equal len 0) (equal len 1)) l)
	  ((equal len 2) (reverse l))
	  (t (rotr l)))))

(defun rotr (l)
  (let* ((r-l (reverse l))
	 (lst (first r-l))
	 (r-rst (rest r-l)))
    (append (list lst) (reverse r-rst))))

#|

CL-USER> (rotate-right '(b a b a))
(A B A B)

|#
