;;;
;;; EXERCISE 7.16
;;;

#|

CL-USER> (reduce #'union '((a b c) (c d a) (f e d) (g)))
(D E F B C A G)

|#


;;;
;;; EXERCISE 7.22
;;;

(defun not-none-odd (l)
  (format NIL "In not-none-odd: ~A" (not (every #'evenp l))))

#|

CL-USER> (not-none-odd '(1 3 5 7 8))
; in: NOT-NONE-ODD '(1 3 5 7 8)
;     (NOT-NONE-ODD '(1 3 5 7 8))
; 
; caught STYLE-WARNING:
;   undefined function: COMMON-LISP-USER::NOT-NONE-ODD
; 
; compilation unit finished
;   Undefined function:
;     NOT-NONE-ODD
;   caught 1 STYLE-WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION NOT-NONE-ODD {10023B4FF3}>.

|#


;;;
;;; EXERCISE 7.30
;;;

(defvar words-es '(uno dos tres quatro cinco))

#|

CL-USER> (mapcar #'append words (mapcar #'list words-es))
((ONE UN UNO) (TWO DEUX DOS) (THREE TROIX TRES) (FOUR QUATRE QUATRO)
 (FIVE CINQ CINCO))

|#


;;;
;;; EXERCISE 7.10.
;;;


;;; a.

(defvar note-table '((c         1)
		     (c-sharp   2)
		     (d         3)
		     (d-sharp   4)
		     (e         5)
		     (f         6)
		     (f-sharp   7)
		     (g         8)
		     (g-sharp   9)
		     (a        10)
		     (a-sharp  11)
		     (b        12)))


;;; b.

(defun get-numbers-by-notes (ns)
  "Takes a list of notes and returns a list of numbers corresponding to the
   notes."
  (mapcar #'(lambda (n) (second (assoc n note-table))) ns))

#|

CL-USER> (get-numbers-by-notes '(e d c d e e e))
(5 3 1 3 5 5 5)

|#


;;; c.

(defun get-note-by-number (n)
  "Takes a note and returns the number corresponding to it."
  (first (find-if #'(lambda (e) (equal n (second e))) note-table)))

(defun get-notes-by-numbers (ns)
  "Takes a list of numbers and returns a list of notes corresponding to the
   numbers."
  (mapcar #'get-note-by-number ns))

#|

CL-USER> (get-numbers-by-notes '(e d c d e e e))
(5 3 1 3 5 5 5)
CL-USER> (get-notes-by-numbers '(5 3 1 3 5 5 5))
(E D C D E E E)

|#


;;; d.

#|

CL-USER> (equal '(E D C D E E E)
	  (get-notes-by-numbers (get-numbers-by-notes '(E D C D E E E))))
T
CL-USER> (equal '(5 3 1 3 5 5 5)
	  (get-numbers-by-notes (get-notes-by-numbers '(5 3 1 3 5 5 5))))
T
CL-USER> (get-notes-by-numbers (get-notes-by-numbers '(5)))
(NIL)
CL-USER> (get-numbers-by-notes (get-numbers-by-notes '(E)))
(NIL)

|#


;;; Extra
(defun get-numbers-of-notes-all ()
  "Returns a list of the numbers corresponding to all the notes in the 
   notes table."
  (mapcar #'(lambda (entry) (second entry)) note-table))

#|

CL-USER> (get-numbers-of-notes-all)
(1 2 3 4 5 6 7 8 9 10 11 12)

|#


;;; e.

(defun raise (l n)
  "Takes a list of numbers (l) and a number (n), and returns a new list with
   all numbers in it increased by n."
  (mapcar #'(lambda (x) (+ x n)) l))

#|

CL-USER> (raise '(5 3 1 3 5 5 5) 5)
(10 8 6 8 10 10 10)

|#

;;; f.

(defun normalize (l)
  "Takes a list of numbers (l) and normalizes l such that each number in l is
   greater than 0 and less than 13."
  (mapcar #'(lambda (x)
	      (cond ((< x  1) (+ x 12))
		    ((> x 12) (- x 12))
		    (t x))) l))

#|

CL-USER> (normalize '(6 10 13))
(6 10 1)
CL-USER> (normalize '(16 0 3))
(4 12 3)

|#


;;; g.

(defun transpose-song (l n)
  "Takes a list of notes (l) and number (n), and returns the song denoted by 
   l but transposed by n half-steps."
  (get-notes-by-numbers (normalize (raise (get-numbers-by-notes l) n))))

#|

CL-USER> (transpose-song '(e d c d e e e) 5)
(A G F G A A A)
CL-USER> (transpose-song '(e d c d e e e) 12)
(E D C D E E E)
CL-USER> (transpose-song '(e d c d e e e) -1)
(D-SHARP C-SHARP B C-SHARP D-SHARP D-SHARP D-SHARP)
CL-USER> (transpose-song '(e d c d e e e) -3)
(C-SHARP B A B C-SHARP C-SHARP C-SHARP)
CL-USER> (transpose-song '(e d c d e e e) -12)
(E D C D E E E)

|#


;;;
;;; EXERCISE 7.15
;;;

;;; a.

(defun get-rank (c)
  "Takes a card (c) and returns its rank."
  (first c))

(defun get-suit (c)
  "Takes a card (c), and returns its suit."
  (second c))

#|

CL-USER> (get-rank '(2 CLUBS))
2
CL-USER> (get-suit '(2 CLUBS))
CLUBS

|#


;;; b.

(defparameter *my-hand* '((3 HEARTS)
			  (5 CLUBS)
			  (2 DIAMONDS)
			  (4 DIAMONDS)
			  (A SPADES)))

(defun count-suit (s h)
  "Takes a suit (s) and a hand (h) of cards and returns the number of cards in
   h that belong to s."
  ;; (reduce #'+ (mapcar #'(lambda (x) (if (equal (get-suit x) s) 1 0)) h)))
  (count-if #'(lambda (x) (equal (get-suit x) s)) h))

#|

CL-USER> (count-suit 'diamonds *my-hand*)
2
CL-USER> (count-suit 'spades *my-hand*)
1

|#


;;; c.

(defparameter *card-colors* '((CLUBS    BLACK)
			      (DIAMONDS RED)
			      (HEARTS   RED)
			      (SPADES   BLACK)))

(defun color-of (c)
  "Takes a card (c), and returns its color."
  (second (assoc (get-suit c) *card-colors*)))

#|

CL-USER> (color-of '(6 HEARTS))
RED

|#

;;; d.

(defun first-red (h)
  "Takes a hand of cards (h), and using an applicative approach, returns the
   first card that is of a red suit or NIL if none are."
  (first (mapcar #'(lambda (x) (if (equalp (color-of x) 'RED) x NIL)) h)))

#|

CL-USER> *my-hand*
((3 HEARTS) (5 CLUBS) (2 DIAMONDS) (4 DIAMONDS) (A SPADES))
CL-USER> (first-red *my-hand*)
(3 HEARTS)

|#

;;; e.

(defun black-cards (h)
  "Takes a hand of cards (h), and returns a list of all the black cards in h."
  (remove-if #'null
	     (mapcar #'(lambda (x) (if (equalp (color-of x) 'BLACK) x NIL)) h)))

#|

CL-USER> (black-cards *my-hand*)
((5 CLUBS) (A SPADES))

|#


;;; f.

(defun what-ranks (s h)
  "Takes a suit (s) and a hand (h) of cards, and returns the ranks of all the
   cards in h that belong to s."
  (mapcar #'(lambda (x) (get-rank x))
	  (remove-if #'null
	     (mapcar #'(lambda (x) (if (equal (get-suit x) s) x NIL)) h))))

#|

CL-USER> (what-ranks 'DIAMONDS *my-hand*)
(2 4)
CL-USER> (what-ranks 'SPADES *my-hand*)
(A)

|#


;;; g.

(defparameter *all-ranks* '(2 3 4 5 6 7 8 9 10 J Q K A))

(defun higher-rank-p (c1 c2)
  "Takes two cards (c1) and (c2), and returns T if c1's rank is higher than
   c2's"
  (let* ((l1 (member (get-rank c1) *all-ranks*))
	 (l2 (member (get-rank c2) *all-ranks*)))
    (< (length l1) (length l2))))

#|

CL-USER> (higher-rank-p '(3 CLUBS) '(4 SPADES))
NIL
CL-USER> (higher-rank-p '(4 CLUBS) '(3 SPADES))
T
CL-USER> (higher-rank-p '(4 CLUBS) '(A SPADES))
NIL
CL-USER> (higher-rank-p '(A CLUBS) '(A SPADES))
NIL
CL-USER> (higher-rank-p '(J CLUBS) '(A SPADES))
NIL
CL-USER> (higher-rank-p '(J CLUBS) '(2 SPADES))
T

|#


;;; g.

(defun get-higher-ranked (c1 c2)
  "Takes two cards (c1) and (c2), and returns the card with the higher rank."
  (if (higher-rank-p c1 c2) c1 c2))

(defun highest-in-hand (h)
  "Takes a hand of cards (h), and returns the highest-ranked card in h."
  (reduce #'(lambda (x y) (get-higher-ranked x y)) h))

#|

CL-USER> (highest-in-hand *my-hand*)
(A SPADES)

|#


;;;
;;; EXERCISE 7.29
;;;

(defparameter *blocks-database* '((b1 shape brick)
				  (b1 color green)
				  (b1 size small)
				  (b1 supported-by b2)
				  (b1 supported-by b3)
				  (b2 shape brick)
				  (b2 color red)
				  (b2 size small)
				  (b2 supports b1)
				  (b2 left-of b3)
				  (b3 shape brick)
				  (b3 color red)
				  (b3 size small)
				  (b3 supports b1)
				  (b3 right-of b2)
				  (b4 shape pyramid)
				  (b4 color blue)
				  (b4 size large)
				  (b4 supported-by b5)
				  (b5 shape cube)
				  (b5 color green)
				  (b5 size large)
				  (b5 supports b4)
				  (b6 shape brick)
				  (b6 color purple)
				  (b6 size large)))

;;; a.

(defun elmnts-match-p (e1 e2)
  "Takes two elements (e1) and (e2), and returns T if e1 and e2 are equal or if
   e2 is a questions mark, and NIL otherwise."
  (or (equal e1 e2) (equal e2 '?)))

#|

CL-USER> (elmnts-match-p 'RED 'RED)
T
CL-USER> (elmnts-match-p 'RED 'READ)
NIL
CL-USER> (elmnts-match-p 'RED '?)
T

|#


;;; b.

(defun triples-match-p (t1 t2)
  "Takes two triples (t1) and (t2), and returns T if each element in t1
   matches the corresponding one in t2, and NIL otherwise."
  (every #'elmnts-match-p t1 t2))
;TRIPLES-MATCH-P

#|

CL-USER> (triples-match-p '(B2 COLOR RED) '(B2 COLOR RED))
T
CL-USER> (triples-match-p '(B2 COLOR RED) '(B2 COLOR ?))
T
CL-USER> (triples-match-p '(B2 COLOR RED) '(B2 COLOR BLUE))
NIL
CL-USER> (triples-match-p '(B1 COLOR RED) '(B2 COLOR RED))
NIL
CL-USER> (triples-match-p '(B2 COLOR RED) '(B2 SHAPE BRICK))
NIL

|#


;;; c.

(defun fetch (p)
  "Takes a pattern (p), and returns all the assertions in the database that
   match p."
  (remove-if-not #'(lambda (x) (triples-match-p x p)) *blocks-database*))

#|

CL-USER> (fetch '(B2 COLOR ?))
((B2 COLOR RED))
CL-USER> (fetch '(? SUPPORTS B1))
((B2 SUPPORTS B1) (B3 SUPPORTS B1))

|#


;;; d.

#|

What shape is block B4?
CL-USER> (fetch '(B4 SHAPE ?))
((B4 SHAPE PYRAMID))

Which blocks are bricks?
CL-USER> (fetch '(? SHAPE BRICK))
((B1 SHAPE BRICK) (B2 SHAPE BRICK) (B3 SHAPE BRICK) (B6 SHAPE BRICK))

What relation is block B2 to block B3?
CL-USER> (fetch '(B2 ? B3))
((B2 LEFT-OF B3))

List the color of every block.
CL-USER> (fetch '(? COLOR ?))
((B1 COLOR GREEN) (B2 COLOR RED) (B3 COLOR RED) (B4 COLOR BLUE)
 (B5 COLOR GREEN) (B6 COLOR PURPLE))

What facts are known about block B4?
CL-USER> (fetch '(B4 ? ?))
((B4 SHAPE PYRAMID) (B4 COLOR BLUE) (B4 SIZE LARGE) (B4 SUPPORTED-BY B5))

|#


;;; e.

(defun blck-to-clr-pttrn (b)
  "Takes a block (b) as input, and returns a pattern asking the color of the
   block."
  (list b 'COLOR '?))
;BLCK-TO-CLR-PTTRN

#|

CL-USER> (blck-to-clr-pttrn 'B3)
(B3 COLOR ?)
CL-USER> (blck-to-clr-pttrn 'B6)
(B6 COLOR ?)

|#


;;; f.

(defun supporters (b)
  "Takes a block (b) as input, and returns a list of blocks that support it,
   or NIL if none exist."
  (mapcar #'first (fetch (list '? 'SUPPORTS b))))
;SUPPORTERS

#|

CL-USER> (fetch '(? SUPPORTS ?))
((B2 SUPPORTS B1) (B3 SUPPORTS B1) (B5 SUPPORTS B4))
CL-USER> (supporters 'B1)
(B2 B3)
CL-USER> (supporters 'B4)
(B5)
CL-USER> (supporters 'B6)
NIL

|#


;;; g.

(defun spprtd-by-cb-p (b)
  "Takes a block (b) as input, and returns T if b is supported by a cube, and
   NIL otherwise."

  (labels ((get-shape (blk)
	     "Takes a block (b) as input, and returns its shape."
	     (car (last (first (fetch (list blk 'SHAPE '?)))))))

    ;; If the result required is boolean (T / NIL) use:
    ;; (some #'(lambda (x) (equal (get-shape x) 'CUBE)) (supporters b))

    ;; If the result required is list ((a) / NIL) use:
    (member 'CUBE (mapcar #'(lambda (x) (get-shape x)) (supporters b)))))

#|

CL-USER> (spprtd-by-cb-p 'B4)
T
CL-USER> (spprtd-by-cb-p 'B1)
NIL
CL-USER> (spprtd-by-cb-p 'B5)
NIL

|#


;;; h.

(defun assertions (b)
  "Takes a block (b) as input and all assertions dealing with b."
  (fetch (list b '? '?)))

#|

CL-USER> (assertions 'B1)
((B1 SHAPE BRICK) (B1 COLOR GREEN) (B1 SIZE SMALL) (B1 SUPPORTED-BY B2)
 (B1 SUPPORTED-BY B3))
CL-USER> (assertions 'B2)
((B2 SHAPE BRICK) (B2 COLOR RED) (B2 SIZE SMALL) (B2 SUPPORTS B1)
 (B2 LEFT-OF B3))
CL-USER> (assertions 'B3)
((B3 SHAPE BRICK) (B3 COLOR RED) (B3 SIZE SMALL) (B3 SUPPORTS B1)
 (B3 RIGHT-OF B2))
CL-USER> (assertions 'B4)
((B4 SHAPE PYRAMID) (B4 COLOR BLUE) (B4 SIZE LARGE) (B4 SUPPORTED-BY B5))
CL-USER> (assertions 'B5)
((B5 SHAPE CUBE) (B5 COLOR GREEN) (B5 SIZE LARGE) (B5 SUPPORTS B4))
CL-USER> (assertions 'B6)
((B6 SHAPE BRICK) (B6 COLOR PURPLE) (B6 SIZE LARGE))

|#


;;; i

(defun attributes (b)
  "Takes a block (b) as input, and returns all its attributes."
  (mapcar #'rest (assertions b)))

#|

CL-USER> (attributes 'B1)
((SHAPE BRICK) (COLOR GREEN) (SIZE SMALL) (SUPPORTED-BY B2) (SUPPORTED-BY B3))
CL-USER> (attributes 'B2)
((SHAPE BRICK) (COLOR RED) (SIZE SMALL) (SUPPORTS B1) (LEFT-OF B3))
CL-USER> (attributes 'B3)
((SHAPE BRICK) (COLOR RED) (SIZE SMALL) (SUPPORTS B1) (RIGHT-OF B2))
CL-USER> (attributes 'B4)
((SHAPE PYRAMID) (COLOR BLUE) (SIZE LARGE) (SUPPORTED-BY B5))
CL-USER> (attributes 'B5)
((SHAPE CUBE) (COLOR GREEN) (SIZE LARGE) (SUPPORTS B4))
CL-USER> (attributes 'B6)
((SHAPE BRICK) (COLOR PURPLE) (SIZE LARGE))

|#


;;; j, k.

(defun dscrb-blk (b)
  "Takes a block (b), and returns its description."
  (rest (remove-duplicates (flatten (assertions b)))))

#|

CL-USER> (dscrb-blk 'B1)
(BRICK COLOR GREEN SIZE SMALL B2 B1 SUPPORTED-BY B3)
CL-USER> (dscrb-blk 'B2)
(BRICK COLOR RED SIZE SMALL SUPPORTS B1 B2 LEFT-OF B3)
CL-USER> (dscrb-blk 'B3)
(BRICK COLOR RED SIZE SMALL SUPPORTS B1 B3 RIGHT-OF B2)
CL-USER> (dscrb-blk 'B4)
(PYRAMID COLOR BLUE SIZE LARGE B4 SUPPORTED-BY B5)
CL-USER> (dscrb-blk 'B5)
(CUBE COLOR GREEN SIZE LARGE B5 SUPPORTS B4)
CL-USER> (dscrb-blk 'B6)
(BRICK COLOR PURPLE B6 SIZE LARGE)

|#


;;; l.


#|

CL-USER> (setf *blocks-database* (cons '(B1 MADE-OF WOOD) *blocks-database*))
CL-USER> *blocks-database*
((B1 MADE-OF WOOD) (B1 SHAPE BRICK) (B1 COLOR GREEN) (B1 SIZE SMALL)
 (B1 SUPPORTED-BY B2) (B1 SUPPORTED-BY B3) (B2 SHAPE BRICK) (B2 COLOR RED)
 (B2 SIZE SMALL) (B2 SUPPORTS B1) (B2 LEFT-OF B3) (B3 SHAPE BRICK)
 (B3 COLOR RED) (B3 SIZE SMALL) (B3 SUPPORTS B1) (B3 RIGHT-OF B2)
 (B4 SHAPE PYRAMID) (B4 COLOR BLUE) (B4 SIZE LARGE) (B4 SUPPORTED-BY B5)
 (B5 SHAPE CUBE) (B5 COLOR GREEN) (B5 SIZE LARGE) (B5 SUPPORTS B4)
 (B6 SHAPE BRICK) (B6 COLOR PURPLE) (B6 SIZE LARGE))

CL-USER> (setf *blocks-database* (cons '(B2 MADE-OF PLASTIC) *blocks-database*))
CL-USER> *blocks-database*
((B2 MADE-OF PLASTIC) (B1 MADE-OF WOOD) (B1 SHAPE BRICK) (B1 COLOR GREEN)
 (B1 SIZE SMALL) (B1 SUPPORTED-BY B2) (B1 SUPPORTED-BY B3) (B2 SHAPE BRICK)
 (B2 COLOR RED) (B2 SIZE SMALL) (B2 SUPPORTS B1) (B2 LEFT-OF B3)
 (B3 SHAPE BRICK) (B3 COLOR RED) (B3 SIZE SMALL) (B3 SUPPORTS B1)
 (B3 RIGHT-OF B2) (B4 SHAPE PYRAMID) (B4 COLOR BLUE) (B4 SIZE LARGE)
 (B4 SUPPORTED-BY B5) (B5 SHAPE CUBE) (B5 COLOR GREEN) (B5 SIZE LARGE)
 (B5 SUPPORTS B4) (B6 SHAPE BRICK) (B6 COLOR PURPLE) (B6 SIZE LARGE))
|#

