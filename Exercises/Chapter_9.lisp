;;;
;;; EXERCISE 9.4
;;;

(defun ninety-nine-bottles (n)
  "Takes a natural number, n, and print the lines of the song 'Ninety-nine
   Bottles of Beer on the Wall' beginning with n."
  (cond ((= n 1)
	 (format t "
                      1 bottle of beer on the wall,
                      1 bottle of beer!
                      Take one down,
                      Pass it around

		      No bottles of beer on the wall !~%~% "))
	(t
	 (progn
	   (format t "
                      ~S bottles of beer on the wall,
                      ~S bottles of beer!
                      Take one down,
                      Pass it around,
                      ~S bottles of beer on the wall.~%"
		   n n (- n 1))
	   (ninety-nine-bottles (- n 1))))))

#|

                      3 bottles of beer on the wall,
                      3 bottles of beer!
                      Take one down,
                      Pass it around,
                      2 bottles of beer on the wall.

                      2 bottles of beer on the wall,
                      2 bottles of beer!
                      Take one down,
                      Pass it around,
                      1 bottles of beer on the wall.

                      1 bottle of beer on the wall,
                      1 bottle of beer!
                      Take one down,
                      Pass it around

		      No bottles of beer on the wall !

 
NIL

|#	 

;;;
;;; EXERCISE 9.5
;;;

(defun print-board (l)
  "Takes a list (l) of nine elements as input, and prints a tic-tac-toe board
   displaying the given elements."
  (dotimes (i 9)
    (let ((e (nth i l))
	  (j (+ i 1)))
      (cond
	((and (< j 9) (zerop (rem j 3)))
	 (progn
	   (if (null e)
	       (format t "   ")
	       (format t " ~S" e))
	   (format t "~&-----------~%")))
	((zerop (rem j 3))
	 (if (null e)
	     (format t "   ")
	     (format t " ~S" e)))
	(t
	 (progn
	   (if (null e) 
	       (format t "   |")
	       (format t " ~S |" e))))))))

#|

CL-USER> (print-board '(NIL NIL NIL NIL NIL NIL NIL NIL NIL))
   |   |   
-----------
   |   |   
-----------
   |   |   
NIL
CL-USER> (print-board '(NIL O X NIL O X NIL O X))
   | O | X
-----------
   | O | X
-----------
   | O | X
NIL
CL-USER> (PRINT-BOARD '(X O O NIL X NIL O NIL X))
 X | O | O
-----------
   | X |   
-----------
 O |   | X
NIL

|#


;;;
;;; EXERCISE 9.9
;;;

#|

CL-USER> (format t "a~S" 'b)
aB
NIL
CL-USER> (format t "always~%broke")
always
broke
NIL
CL-USER> (format t "~S~S" 'alpha 'bet)
ALPHABET
NIL

|#
