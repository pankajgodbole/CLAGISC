;;;
;;; EXERCISE 10.3
;;;

(defparameter *cnt-persons-met-more-than-once* 0)

(defun meet-with-count (person)
  "Takes a person (person) to meet, and returns a symbol indicating the extent
   of their friendship and keeps a count of how many people have been met more
   than once."
  (cond ((equal person (first *friends*))
	 (progn
	   (incf *cnt-persons-met-more-than-once*)
	   'WE-JUST-MET))
	((member person *friends*)
	 (progn
	   (incf *cnt-persons-met-more-than-once*)
	   'WE-KNOW-EACH-OTHER))
	(t (push person *friends*) 'PLEASED-TO-MEET-YOU)))

#|

CL-USER> (setf *friends* NIL)
NIL
CL-USER> (meet-with-count 'fred)
PLEASED-TO-MEET-YOU
CL-USER> *cnt-persons-met-more-than-once*
2
CL-USER> (meet-with-count 'cindy)
PLEASED-TO-MEET-YOU
CL-USER> (meet-with-count 'fred)
WE-KNOW-EACH-OTHER
CL-USER> *cnt-persons-met-more-than-once*
3
CL-USER> (meet-with-count 'cindy)
WE-JUST-MET
CL-USER> *cnt-persons-met-more-than-once*
4

|#


;;;
;;; EXERCISE 10.4
;;;

(defun forget (p)
  "Takes a person (p), and removes p from the *FRIENDS* list."
  (if (not (member p *FRIENDS*))
      'SORRY-PERSON-NOT-IN-*FRIENDS*
      (progn
	(setf *FRIENDS* (remove p *FRIENDS*))
	(format t "*FRIENDS* = ~s~%" *FRIENDS*)
	'PERSON_REMOVED)))

#|

CL-USER> (setf *friends* NIL)
NIL
CL-USER> (forget 'fred)
SORRY-PERSON-NOT-IN-*FRIENDS*
CL-USER> (meet 'fred)
PLEASED-TO-MEET-YOU
CL-USER> *friends*
(FRED)
CL-USER> (forget 'fred)
*FRIENDS* = NIL
PERSON_REMOVED
CL-USER> (meet 'cindy)
PLEASED-TO-MEET-YOU
CL-USER> (meet 'fred)
PLEASED-TO-MEET-YOU
CL-USER> (meet 'cindy)
WE-KNOW-EACH-OTHER
CL-USER> *FRIENDS*
(FRED CINDY)
CL-USER> (forget 'Cindy)
*FRIENDS* = (FRED)
PERSON_REMOVED

|#


;;;
;;; EXERCISE 10.9
;;;

(defun chop (l)
  (if (equal l NIL)
      NIL
      (nsubst (first l) l l :test #'eq)))
      ;(defvar l (first l))))

#|

CL-USER> (chop '(fee fie foe fum))
FEE

|#


;;;
;;; EXERCISE 10.10
;;;

(defun ntack (l x)
  (nconc l (list x)))

(defvar test-list '(a b))

(defvar test-list-2 '(fee fie foe))

#|

CL-USER> (ntack NIL 'nil)
(NIL)
CL-USER> (ntack test-list 'c)
(A B C C)
CL-USER> test-list
(A B C C)
CL-USER> (ntack test-list-2 'fum)
(FEE FIE FOE FUM FUM)
CL-USER> test-list-2
(FEE FIE FOE FUM FUM)

|#


