;;;
;;; EXERCISE 11.9
;;;

(defun check-all-odd-with-do (l)
  "Takes a list (l) of numbers, and returns T if all elements in l are odd and
   NIL otherwise."
  (do ((x l (rest x)))
      ((and (not (null l)) (null x)) T)
    (when (or (null l) (evenp (car x)))
      (return NIL))))

#|

CL-USER> (check-all-odd-with-do NIL)
NIL
CL-USER> (check-all-odd-with-do '(2))
NIL
CL-USER> (check-all-odd-with-do '(1 2))
NIL
CL-USER> (check-all-odd-with-do '(1 3))
T

|#


;;;
;;; EXERCISE 11.10
;;;

(defun launch-with-dotimes (n)
  "Takes a natural number (n), and prints the value of n and decrements n by 1
   with each iteration until n is 0."
  (dotimes (i n (format t "BLAST OFF!!!"))
    (format t "~S... " (- n i))))

#|

CL-USER> (launch-with-dotimes 10)
10... 9... 8... 7... 6... 5... 4... 3... 2... 1... BLAST OFF!!!
NIL

|#


;;; DO with variables, test condition, actions, but no body.

(defun count-slices (loaf)
  (do ((num-slices 0 (+ num-slices 1))
       (loaf-left loaf (rest loaf-left)))
      ((null loaf-left) num-slices)))

#|

CL-USER> (count-slices '(1 2 3 4 5 6))
6

|#


;;;
;;; EXERCISE 11.11
;;;

#|

Rewrite the following function to use DO* instead of DOLIST.

(defun find-largest (list-of-numbers)
  (let ((largest (first list-of-numbers)))
    (dolist (element (rest list-of-numbers)
	     largest)
      (when (> element largest)
	(setf largest element)))))

|#

(defun find-largest (l)
  "Takes a list (l) of numbers, and using DO* instead of DOLIST, returns the
   largest among them."
  (do* ((x l (rest x))
	(e (first x) (first x))
	(lrgst 0))
       ((null x) lrgst)
    (when e 
      (setf lrgst (max lrgst e)))))

#|

CL-USER> (find-largest NIL)
0
CL-USER> (find-largest '(1))
1
CL-USER> (find-largest '(-1 0 2))
2
CL-USER> (find-largest '(-1 0 -2))
0

|#
       
  
;;;
;;; EXERCISE 11.12
;;;

#|

Rewrite the following function to use DO instead of DOTIMES.

(defun power-of-2 (n) ;2 to the Nth power.
  (let ((result 1))
    (dotimes (i n result)
      (incf result result))))

|#

(defun power-of-2 (n)
  "Takes a natural number (n), and using DO instead of DOTIMES, returns the 
   power of 2 to n."
  (do ((rslt 1)
       (i n (decf i)))
      ((zerop i) rslt)
    (if (minusp n)
	(error "~S is less than 0" n)
	(incf rslt rslt))))

#|

CL-USER> (power-of-2 -1)
; Evaluation aborted on #<SIMPLE-ERROR "~S is less than 0" {1003F38293}>.
CL-USER> (power-of-2 1)
2
CL-USER> (power-of-2 100)
1267650600228229401496703205376

|#


;;;
;;; EXERCISE 11.13
;;;

#|

Rewrite the following function using DOLIST instead of DO*.

(defun first-non-integer (x)
  "Return the first non-integer element of X."
  (do* ((z x (rest z))
	(z1 (first z) (first z)))
       ((null z) ’none)
    (unless (integerp z1)
      (return z1))))

|#

(defun first-non-integer (l)
  "Takes a list (l), and using DOLIST instead of DO*, returns the first element
   of l that is not an integer."
  (dolist (e l (format t "Only integers in list."))
    (unless (integerp e)
      (return e))))

#|

CL-USER> (first-non-integer NIL)
Only integers in list.
NIL
CL-USER> (first-non-integer '(1))
Only integers in list.
NIL
CL-USER> (first-non-integer '(e))
E
CL-USER> (first-non-integer '(1 o e 1))
O

|#


;;;
;;; EXERCISE 11.14
;;;

(defun ffo-with-do*-as-do (l)
  (do* ((x l (rest x)) ; Replaced DO* with DO
        (e (first x) (first x)))
       ((null x) NIL)
    (if (oddp e) (return e))))

#|

CL-USER> (ffo-with-do*-as-do '(2 4 6 7))
7

|#


;;;
;;; EXERCISE 11.15
;;;

(defun ffo-with-do (x)
  (do ((z x (rest z))
       (e (first x) (first z)))
      ((null z) nil)
    (if (oddp e) (return e))))

#|

Unlike DO* notation DO notation entails parallel assignment. Because of the 
parallel assignment of z and e in DO notation, at the end of the list e is 
assigned the last element (7, which is odd) at the same time that z is assigned
the empty list (NIL). Since z is now null, the termination test is satisfied,
and the function terminates returns NIL, without executing the body of the DO
block which checks whether e is odd.

|#


;;;
;;; EXERCISE 11.17
;;;

(dotimes (i 5 i)
  (format t "I = ~S~%" i))

#|

CL-USER> (dotimes (i 5 i)
	   (format t "I = ~S~%" i))
I = 0
I = 1
I = 2
I = 3
I = 4
5

|#


;;;
;;; EXERCISE 11.19
;;;

#|

Answer:
No. Because they are independent of each other.

|#


;;;
;;; EXERCISE 11.20
;;;

#|

Answer:
Yes.

|#


;;;
;;; EXERCISE 11.21
;;;

(defun fib-iterative (n)
  (do* ((cnt 2 (+ cnt 1))
	(fib-0 0)
	(fib-1 1)
	(fib-first fib-0 fib-second)
	(fib-second fib-1 fib-n)
	(fib-n (+ fib-first fib-second) (+ fib-first fib-second)))
       ((= cnt n) fib-n)
    (when (< n 2)
	(return 1))))

#|

CL-USER> (fib-iterative -1)
1
CL-USER> (fib-iterative 0)
1
CL-USER> (fib-iterative 1)
1
CL-USER> (fib-iterative 2)
1
CL-USER> (fib-iterative 21)
10946
CL-USER> (fib-iterative 200)
280571172992510140037611932413038677189525
CL-USER> (fib-iterative 240)
64202014863723094126901777428873111802307548623680
CL-USER> (fib-iterative 300)
222232244629420445529739893461909967206666939096499764990979600

|#


;;;
;;; EXERCISE 11.22d
;;;

(defun count-bases (s)
  "Takes a single or double strand (s) of DNA, and returns the number of bases
   in it."
  (cond ((null s) NIL)
	((listp (car s))
	 ;; This is a double-strand, so first flatten it.
	 (count-bases (flatten s)))
	(t (let ((a-cnt 0)
		 (c-cnt 0)
		 (g-cnt 0)
		 (t-cnt 0))
	     (do* ((bs s (rest bs))
		   (b (first bs) (first bs))
		   (all-cnts (list
			      (list 'A a-cnt)
			      (list 'T t-cnt)
			      (list 'G g-cnt)
			      (list 'C c-cnt))
			     (list
			      (list 'A a-cnt)
			      (list 'T t-cnt)
			      (list 'G g-cnt)
			      (list 'C c-cnt))))
		  ((null bs) all-cnts)
	       (cond ((equal b 'A) (incf a-cnt))
		     ((equal b 'C) (incf c-cnt))
		     ((equal b 'G) (incf g-cnt))
		     ((equal b 't) (incf t-cnt))
		     (t (format t "~&~S is not a valid base.~%" b))))))))

#|

CL-USER> (count-bases '())
((A 0) (C 0) (G 0) (T 0))
CL-USER> (count-bases '(A G T A C T C T))
((A 2) (T 3) (G 1) (C 2))
CL-USER> (count-bases '((G C) (A T) (T A) (T A) (C G)))
((A 3) (T 3) (G 2) (C 2))

|#


;;; e.

(defun prefix-p (p s)
  "Takes two strands of DNA (p, for prefix) and (s), and returns T if p is a
   prefix of s and NIL if not."
  (every #'(lambda (x y) (equal x y)) p s))
;PREFIX-P

#|

CL-USER> (prefix-p '(G C T) '(G T C A T))
NIL
CL-USER> (prefix-p '(G T C) '(G T C A T))
T
CL-USER> (prefix-p '(G T C) '(A A G T C))
NIL
CL-USER> (prefix-p '(A A G T C) '(G T C))
NIL
CL-USER> (prefix-p '(G T C A A) '(G T C))
T

|#


;;; f.

(defun appears-p (x y)
  "Takes two DNA strands (x) and (y), and returns T if x appears anywhere
   within y and NIL otherwise."
  (cond ((null y) NIL)
	((prefix-p x y) T)
	(t (appears-p x (rest y)))))
;APPEARS-P

#|
CL-USER> (appears-p '(C A T) '(T C A T G))
T
CL-USER> (appears-p '(C A T) '(T C C T G))
NIL
CL-USER> (appears-p '(C A T) '(T C C T G C A T))
T
CL-USER> (appears-p '(T C C) '(T C C T G C A T))
T
|#


;;; g.

(defun covers-p (x y)
  "Takes two inputs (x) and (y), and returns T if x, repeated some number of
   times, matches y and NIL otherwise."
  (let* ((n (floor (length y) (length x)))
 	 (s (flatten (loop repeat n :collect x))))
    (equal s y)))
;COVERS-P

#|

CL-USER> (covers-p '(A G C) '(A G C A G C A G C))
T
CL-USER> (covers-p '(A G C) '(A G C A G C A G C A))
NIL
CL-USER> (covers-p '(A G C T) '(A G C A G C A G C))
NIL
CL-USER> (covers-p '(A G C T) '(A G C T A G C T A G C T))
T
CL-USER> (covers-p '(A G C T A G C T A G C T) '(A G C T))
NIL

|#


;;; h.

(defun prefix (n s)
  "Takes a natural number (n) and a strand (s), and returns a prefix of s 
   whose length is n."
  (subseq s 0 n))
;PREFIX

#|
CL-USER> (prefix 4 '(C G A T T A G))
(C G A T)
CL-USER> (prefix 4 '(A G C A G C A G C))
(A G C A)
CL-USER> (prefix 4 '(A G C))
; Evaluation aborted on #<SB-KERNEL:BOUNDING-INDICES-BAD-ERROR expected-type:
                                       (CONS (INTEGER 0 3) (INTEGER 0 3))
                                       datum: (0 . 4)>.
|#


;;; i.

(defun kernel (s)
  "Takes a strand (s) of DNA, and returns the shortest sequence of bases that
   can be repeated over s.
   E.g. 
    (KERNEL ’(A G C A G C A G C)) should return (A G C)
    (KERNEL ’(A A A A A)) should return (A)
    (KERNEL ’(A G G T C)) should return (A G G T C)"
  (labels
      ((helper (x i)
	 (cond ((covers-p x s) x)
	       (t
		(let ((j (+ i 1)))
		  (helper (subseq s 0 j) j))))))
    (helper (subseq s 0 1) 1)))

#|

CL-USER> (kernel '(A G C A G C A G C))
(A G C)
CL-USER> (kernel '(A A A A A))
(A)
CL-USER> (kernel '(A G G T C))
(A G G T C)

|#


;;; j.

(defun draw-dna (s)
  "Takes a strand (s) of DNA, and draws its structure."
  (format t "~%")
  (let ((n (* (length s) 5)))
    (dotimes (i n (format t "~%"))
      (format t "-"))
    (dolist (x s (format t "~%"))
      (format t "  !  "))
    (dolist (x s (format t "~%"))
      (format t "  ~S  " x))
    (dolist (x s (format t "~%"))
      (format t "  .  "))
    (dolist (x s (format t "~%"))
      (format t "  .  "))
    (dolist (x (complement-strand s) (format t "~%"))
      (format t "  ~S  " x))
    (dolist (x (complement-strand s) (format t "~%"))
      (format t "  !  "))
    (dotimes (i n (format t "~%"))
      (format t "-"))))

#|

CL-USER> (draw-dna '(A G G T C))

-------------------------
  !    !    !    !    !  
  A    G    G    T    C  
  .    .    .    .    .  
  .    .    .    .    .  
  T    C    C    A    G  
  !    !    !    !    !  
-------------------------
NIL

CL-USER> (draw-dna '(A G C A G C A G C))

---------------------------------------------
  !    !    !    !    !    !    !    !    !  
  A    G    C    A    G    C    A    G    C  
  .    .    .    .    .    .    .    .    .  
  .    .    .    .    .    .    .    .    .  
  T    C    G    T    C    G    T    C    G  
  !    !    !    !    !    !    !    !    !  
---------------------------------------------
NIL

|#
