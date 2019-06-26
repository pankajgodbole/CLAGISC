;;;
;;; EXERCISE 13.1
;;;

(defun sub-from-property (sym elmnt indctr)
  "Takes a symbol (sym), a value (elmnt) and a key (indctr), and removes elmnt
   from the property indicated by indctr."
  (let ((p (remove elmnt (get sym indctr))))
    (remprop sym indctr)
    (setf (get sym indctr) p)))

#|

CL-USER> (sub-from-property 'alpha 'd 'fooprop)
(A B C E F)
CL-USER> (symbol-plist 'alpha)
(FOOPROP (A B C E F))

|#


;;;
;;; EXERCISE 13.2
;;;

(defun forget-meeting (x y)
  "Takes the names of two individuals, and erases information about their 
   meeting."
  (sub-from-property x y 'has-met)
  (sub-from-property y x 'has-met)
  t)

#|

CL-USER> (forget-meeting 'wolfie 'grandma)
T
CL-USER> (symbol-plist 'wolfie)
(HAS-MET (LITTLE-RED))
CL-USER> (symbol-plist 'grandma)
(HAS-MET NIL)

|#


;;;
;;; EXERCISE 13.3
;;;

;;; TODO check with (setf ...)

(defun get-using-symbol-plist (s i &optional v)
  "Takes a symbol (s) a property indicator (i) and an optional value (v), and
   returns the value of the property of s indicated by i if it is present, or v
   as the default value if no such property is present."
  (let* ((plist (symbol-plist s))
	 (p     (second (member i plist))))
    (cond ((null p)
	   (if v v NIL))
	  (t p))))

#|

CL-USER> (get 'alpha 'fooprop)
(1 2 3 4 5 6)
CL-USER> (get-using-symbol-plist 'alpha 'fooprop)
(1 2 3 4 5 6)
CL-USER> (get-using-symbol-plist 'alpha 'gooprop)
NIL
CL-USER> (get-using-symbol-plist 'alpha 'gooprop 'DEFAULT_VALUE)
DEFAULT_VALUE
CL-USER> (setf (get 'alpha 'fooprop) '(1 2 3 4 5 6))
(1 2 3 4 5 6)
CL-USER> (setf (get-using-symbol-plist 'alpha 'fooprop) '(a b c d e f))
; in: SETF (GET-USING-SYMBOL-PLIST 'ALPHA 'FOOPROP)
;     (FUNCALL #'(SETF GET-USING-SYMBOL-PLIST) #:NEW1 'ALPHA 'FOOPROP)
; ==>
;   (SB-C::%FUNCALL #'(SETF GET-USING-SYMBOL-PLIST) #:NEW1 'ALPHA 'FOOPROP)
; 
; caught STYLE-WARNING:
;   undefined function: (COMMON-LISP:SETF COMMON-LISP-USER::GET-USING-SYMBOL-PLIST)
; 
; compilation unit finished
;   Undefined function:
;     (SETF GET-USING-SYMBOL-PLIST)
;   caught 1 STYLE-WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION (SETF GET-USING-SYMBOL-PLIST) {100554B893}>.

|#


;;;
;;; EXERCISE 13.4
;;;

(defun has-property (s i)
  "Takes a symbol (s) and an indicator (i) and returns T if a property
   corresponding to i exists in s, or NIL otherwise."
  (if (not (null (member i (symbol-plist s))))
      T
      NIL))

#|

CL-USER> (symbol-plist 'fred)
(CITY NIL SIBLINGS (GEORGE WANDA) SEX MALE)
CL-USER> (has-property 'fred 'siblings)
T
CL-USER> (has-property 'fred 'parents)
NIL
CL-USER> (setf (get 'fred 'city) NIL)
NIL
CL-USER> (symbol-plist 'fred)
(CITY NIL SIBLINGS (GEORGE WANDA) SEX MALE)
CL-USER> (has-property 'fred 'city)
T

|#




;;;
;;; EXERCISE 13.8
;;;

;;; a.

(defvar *total-bins* 0)
(defvar *hist-array* '#())
(defvar *total-points* 0)


;;; b.

(defun new-histogram (num-bins num-points)
  "Takes a number (*total-bins*) for the desired number of bins and initializes
   the appropriate global variables."
  (setf *total-bins* num-bins) 
  (setf *hist-array* (make-array *total-bins* :initial-element 0))
  (setf *total-points* num-points))


;;; c.

(defun record-value (val)
  "Takes a number (val) and increments the appropriate element of the
   histogram-array if the number is between 0 and *total-bins*."
  (if (valid-value-p val)
      (incf (aref *hist-array* val))
      (format t "~&Value ~S is not within range [0 to ~S]" val *total-bins*)))

(defun valid-value-p (val)
  "Takes a value (val) and returns T if it is a positive number less than the
   number of bins, or NIL otherwise."
  (if (and (> val -1) (< val *total-bins*)) T NIL))


;;; d.

(defun print-hist-line (val)
  "Takes a number (val), and prints a line corresponding to the number of times
   that value occurred in the histogram."
  (let ((o (get-occurrence val)))
    (format t "~&  ~2S  [~3S]  " val o)
    (dotimes (i o)
      (format t "*"))))

(defun get-occurrence (val)
  (aref *hist-array* val))


;;; e.

(defun print-histogram ()
  "Prints out the entire histogram."
  (format t "~&~% Total points: ~S~2%" *total-points*)
  (dotimes (i *total-bins*)
    (print-hist-line i))
  (format t "~2%;"))
 
#|

CL-USER> (new-histogram 31 2000)
2000
CL-USER> (dotimes (i *total-points*)
	   (record-value (random *total-bins*)))
NIL
CL-USER> (print-histogram)

 Total points: 2000

  0   [68 ]  ********************************************************************
  1   [60 ]  ************************************************************
  2   [58 ]  **********************************************************
  3   [56 ]  ********************************************************
  4   [60 ]  ************************************************************
  5   [49 ]  *************************************************
  6   [63 ]  ***************************************************************
  7   [67 ]  *******************************************************************
  8   [66 ]  ******************************************************************
  9   [75 ]  ***************************************************************************
  10  [76 ]  ****************************************************************************
  11  [80 ]  ********************************************************************************
  12  [51 ]  ***************************************************
  13  [69 ]  *********************************************************************
  14  [58 ]  **********************************************************
  15  [72 ]  ************************************************************************
  16  [59 ]  ***********************************************************
  17  [62 ]  **************************************************************
  18  [66 ]  ******************************************************************
  19  [52 ]  ****************************************************
  20  [64 ]  ****************************************************************
  21  [66 ]  ******************************************************************
  22  [73 ]  *************************************************************************
  23  [69 ]  *********************************************************************
  24  [65 ]  *****************************************************************
  25  [59 ]  ***********************************************************
  26  [71 ]  ***********************************************************************
  27  [65 ]  *****************************************************************
  28  [74 ]  **************************************************************************
  29  [61 ]  *************************************************************
  30  [66 ]  ******************************************************************

;
NIL

|#


;;;
;;; EXERCISE 13.9
;;;

(defparameter *crypto-text*
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
    "enlpo pib slafml pvv bfwkj"))


;;; a.

(defparameter *encipher-table* NIL)
(defparameter *decipher-table* NIL)

(defun init-enc-table ()
  (setf *encipher-table* (make-hash-table :size 26)))

(defun init-dec-table ()
  (setf *decipher-table* (make-hash-table :size 26)))

#|

CL-USER> (init-enc-table)
#<HASH-TABLE :TEST EQL :COUNT 0 {1004B8A3A3}>
CL-USER> (init-dec-table)
#<HASH-TABLE :TEST EQL :COUNT 0 {1004B8D9E3}>

|#


;;; b.

(defun make-substitution (c p)
  "Takes two characters (p, for plain text) and (c, for cipher text), and 
   stores them in the encipher and decipher tables appropriately."
  (setf (gethash p *encipher-table*) c)
  (setf (gethash c *decipher-table*) p))

#|

CL-USER> (make-substitution #\c #\p)
#\c
CL-USER> *encipher-table*
#<HASH-TABLE :TEST EQL :COUNT 1 {1003B46BB3}>
CL-USER> *decipher-table*
#<HASH-TABLE :TEST EQL :COUNT 1 {1003F7A963}>

|#


;;; c.

(defun undo-substitution (c)
  "Takes one character (c) as input, and deletes its entry from the cipher
   tables."
  (let ((p (gethash c *decipher-table*)))
    (remhash p *encipher-table*)
    (remhash c *decipher-table*)))

#|

CL-USER> *encipher-table*
#<HASH-TABLE :TEST EQL :COUNT 1 {1003B46BB3}>
CL-USER> *decipher-table*
#<HASH-TABLE :TEST EQL :COUNT 1 {1003F7A963}>
CL-USER> (clear-cipher-tables)
#<HASH-TABLE :TEST EQL :COUNT 0 {1003F7A963}>
CL-USER> *encipher-table*
#<HASH-TABLE :TEST EQL :COUNT 0 {1003B46BB3}>
CL-USER> *decipher-table*
#<HASH-TABLE :TEST EQL :COUNT 0 {1003F7A963}>

|#


;;; d.

(defun clear-cipher-tables ()
  "Removes all key-value pairs from the cipher tables."
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))


;;; e.

(defun decipher-string (s)
  "Takes a single encoded string (s) and returns another partially 
   decoded-string."
  (let* ((ct (string-downcase s))
	 (l  (length ct))
	 (pt (make-string l :initial-element #\Space)))
    (loop
       for x across ct
       for i from 0
       do
	 (progn
	   (let ((p (gethash x *decipher-table*)))
	     ;; (format t "x ~s, i ~a, p ~s~%" x i p)
	     (when p
	       (setf (char pt i) p))))
	 :finally (return pt))))
	   

;;; f.

(defun show-line (ct)
  "Takes a line of cipher-text (c), and displays c and the text obtained by
   deciphering c, beneath it."
  (format t "~%~%~a" ct)
  (format t "~%~a" (decipher-string ct)))


;;; g.

(defun show-text (l)
  "Takes a cryptogram as a list (l) of strings, and displays the lines as 
   shown.

   Substitute which letter? k
   What does ’k’ decipher to? a
   But ’p’ already deciphers to ’a’!
   --------------------
   zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
   i  i               ai   i     a              a        a

   enlpo pib slafml pvv bfwkj
      a  a          a
   --------------------
   Substitute which letter?"

  (format t "~%------")
  (dolist (ct l)
    (show-line ct))
  (format t "~%~%------~%~%"))

#|

CL-USER> (show-text
	  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
	    "enlpo pib slafml pvv bfwkj"))

------

zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
                   a          a              a        a    

enlpo pib slafml pvv bfwkj
   a  a          a        

------

NIL

|#


;;; h.

(defun get-first-char (o)
  "Takes an object (o), gets the first character (c) in the printed
   representation of o, and returns c after converting c to lowercase."
  (char-downcase (char (format NIL "~a" o) 0)))


;;; i.

(defun read-letter ()
  "Reads an object (o) from the keyboard, and returns o if o is the symbol 
  END or UNDO, and the first character of o otherwise."
  (let* ((x (read))
	 (s (write-to-string x)))
    (if (> (length s) 1)
	(if (or (equal x 'END) (equal x 'UNDO))
	    x
	    'NOT_A_VALID_INPUT)
	(get-first-char x))))

#|

CL-USER> (read-letter)
end
END
CL-USER> (read-letter)
undo
UNDO
CL-USER> (read-letter)
afdaa
NOT_A_VALID_INPUT
CL-USER> (read-letter)
e
#\e

|#


;;; j.

(defun sbsttt-ltr (o)
  "Takes a character object (o) as input, and processes o depending on whether
   o has already been deciphered or not."
  (if (not (characterp o))
      'NOT_A_VALID_INPUT
      (let ((p (gethash o *decipher-table*)))
	(if p
	    (format t "~a has already been deciphered as ~a." o p)
	    (progn
	      (format t "What does ~a decipher to? " o)
	      (let ((p (read-letter)))
		(format t "p ~s" p)
		(if (not (characterp p))
		    (progn
		      (format t "~%~s is not a valid input." p)
		      (return-from sbsttt-ltr 'NOT_A_VALID_INPUT))
		    (let ((c (gethash p *encipher-table*)))
		      (if c
			  (format t "~%But ~a already deciphers to ~a" c p)
			  (make-substitution o p))))))))))

#|

CL-USER> (sbsttt-ltr #\p)
p has already been deciphered to a.
NIL
CL-USER> (sbsttt-ltr #\a)
a has already been deciphered to e.
NIL
CL-USER> (sbsttt-ltr "asfd")
NOT_A_VALID_INPUT
CL-USER> (show-hashtable *encipher-table*)
i: 0, k: #\a, v: #\p
i: 1, k: #\e, v: #\a
NIL

|#


;;; k.

(defun undo-letter ()
  "Reads in a character (c) from the keyboard, and removes c from the cipher
   tables if c has already been deciphered or prints an appropriate error 
   message otherwise."
  (format t "~%Undo which letter? ")
  (let* ((c (read-letter))
	 (p (gethash c *decipher-table*)))
    (if p
	(prog2
	    (undo-substitution c)
	    'SUBSTITUTION_UNDONE)
	(prog2
	    (format t "~a has not yet been considered for substitution." c)
	    'CHARACTER_NOT_YET_CONSIDERED_FOR_SUBSTITUTION))))

#|

CL-USER> (undo-letter)

Undo which letter? k
k has not yet been considered for substitution.
CHARACTER_NOT_YET_CONSIDERED_FOR_SUBSTITUTION
CL-USER> (undo-letter)

Undo which letter? z

SUBSTITUTION_UNDONE
CL-USER> (show-hashtable *encipher-table*)
NIL
CL-USER> (show-hashtable *decipher-table*)
NIL

|#


;;; l.

(defun solve-cryptogram (ct)
  "Takes a cryptogram (ct, for cipher-text), and returns the deciphered plain-
   text."
  (init-enc-table)
  (init-dec-table)
  (clear-cipher-tables)
  
  (format t "Lets solve this cryptogram.~%")

  (labels ((helper (s)
	     (format t "~%Substitute which letter? ")
	     (let ((x (read-letter)))
	       (if (not (characterp x))
		   (cond ((equal x 'END) T)
			 ((equal x 'UNDO) (progn
					    (undo-letter)
					    (show-text s)
					    (helper s)))
			 (t (prog2
				(format t "~a is not a valid input!" x)
				'NOT_A_VALID_INPUT)))
		   (progn
		     (sbsttt-ltr x) 
		     (show-text s)
		     (helper s))))))

    (helper ct)))

