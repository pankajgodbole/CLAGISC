;;;
;;;
;;; KEYBOARD EXERCISES
;;;
;;;



;;;
;;; EXERCISE 5.6
;;;


;;; a

(defun throw-die ()
  "Returns a random integer from 1 to 6."
  (+ (random 6) 1))
;THROW-DIE

#|

CL-USER> (throw-die)
1
CL-USER> (throw-die)
6
CL-USER> (throw-die)
4

|#


;;; b

(defun throw-dice ()
  "Returns a list of two random integers from 1 to 6."
  (list (+ (random 6) 1) (+ (random 6) 1)))

#|

CL-USER> (throw-dice)
(3 3)
CL-USER> (throw-dice)
(4 3)
CL-USER> (throw-dice)
(3 2)
CL-USER> (throw-dice)
(5 1)

|#


;;; c

(defun snake-eyes-p (l)
  "Takes a list (l) denoting a throw, and returns T if l is (1 1) or NIL
   otherwise."
  (if (equal l '(1 1)) T NIL))

#|

CL-USER> (snake-eyes-p ())
NIL
CL-USER> (snake-eyes-p '(1))
NIL
CL-USER> (snake-eyes-p '(1 2))
NIL
CL-USER> (snake-eyes-p '(1 1))
T

|#


(defun boxcars-p (l)
  "Takes a list (l) denoting a throw, and returns T if l is (6 6) or NIL
   otherwise."
  (if (equal l '(6 6)) T NIL))

#|

CL-USER> (boxcars-p ())
NIL
CL-USER> (boxcars-p '(1))
NIL
CL-USER> (boxcars-p '(6 1))
NIL
CL-USER> (boxcars-p '(1 6))
NIL
CL-USER> (boxcars-p '(6 6))
T

|#


;;; d

(defun instant-win-p (thrw)
  "Takes a throw (thrw) as input, and returns T if thrw corresponds to a sum of
   7 or 11 (instant win) or NIL otherwise."
  (if (or (equal (reduce #'+ thrw) 7)
	  (equal (reduce #'+ thrw) 11))
      T
      NIL))

#|

CL-USER> (instant-win-p ())
NIL
CL-USER> (instant-win-p '(1))
NIL
CL-USER> (instant-win-p '(1 1))
NIL
CL-USER> (instant-win-p '(1 7))
NIL
CL-USER> (instant-win-p '(1 6))
T
CL-USER> (instant-win-p '(5 6))
T
CL-USER> (instant-win-p (throw-dice))
T
CL-USER> (instant-win-p (throw-dice))
NIL

|#


(defun instant-loss-p (thrw)
  "Takes a throw (thrw) as input, and returns T if thrw corresponds to a sum of
   2, 3 or 12 (instant loss) or NIL otherwise."
  (if (or (equal (reduce #'+ thrw) 2)
	  (equal (reduce #'+ thrw) 3)
	  (equal (reduce #'+ thrw) 12))
      T
      NIL))

#|

CL-USER> (instant-loss-p ())
NIL
CL-USER> (instant-loss-p '(1))
NIL
CL-USER> (instant-loss-p '(1 1))
T
CL-USER> (instant-loss-p '(1 2))
T
CL-USER> (instant-loss-p '(6 6))
T
CL-USER> (instant-loss-p (throw-dice))
NIL
CL-USER> (instant-loss-p (throw-dice))
NIL

|#


;;; e

(defun say-throw (thrw)
  "Takes a throw (thrw) as input, and returns either SNAKE-EYES, BOXCARS
   or the value of thrw if the value is equal to neither SNAKE-EYES nor 
   BOXCARS."
  (cond ((null thrw) NIL)
	((/= (length thrw) 2) NIL)
	((snake-eyes-p thrw) 'SNAKE-EYES)
	((boxcars-p    thrw) 'BOXCARS)
	(t (reduce #'+ thrw))))

#|

CL-USER> (say-throw ())
NIL
CL-USER> (say-throw '(1))
NIL
CL-USER> (say-throw '(1 1 1))
NIL
CL-USER> (say-throw '(1 1))
SNAKE-EYES
CL-USER> (say-throw '(1 2))
3
CL-USER> (say-throw '(6 2))
8
CL-USER> (say-throw '(6 6))
BOXCARS

|#


;;; f

(defun craps ()
  "Throws two dice, and prints a statement based on the outcome of the throw."
  (let* ((thrw     (throw-dice))
	 (t1       (first thrw))
	 (t2       (second thrw))
	 (thrw-sum (reduce #'+ thrw)))
    (format t
	    "(THROW ~a AND ~a -- ~a -- ~a)"
	    t1
	    t2
	    thrw-sum
	    (cond ((or (snake-eyes-p thrw) (boxcars-p thrw))
		   "YOU LOSE")
		  ((or (= thrw-sum 7) (= thrw-sum 11))
		   "YOU WIN")
		  (t (concatenate
		      'string
		      "YOUR POINT IS "
		      (write-to-string thrw-sum)))))))

#|

CL-USER> (craps)
(THROW 1 AND 4 -- 5 -- YOUR POINT IS 5)
NIL
CL-USER> (craps)
(THROW 1 AND 1 -- SNAKE-EYES -- YOU LOSE)
NIL
CL-USER> (craps)
(THROW 6 AND 4 -- 10 -- YOUR POINT IS 10)
NIL
CL-USER> (craps)
(THROW 2 AND 3 -- 5 -- YOUR POINT IS 5)
NIL
CL-USER> (craps)
(THROW 6 AND 5 -- 11 -- YOU WIN)
NIL

|#


;;; g

(defun try-for-point (p)
  "Takes a number (p) between 2 and 12, and simulates the game of craps with
   p as the winning throw."
  (let* ((thrw (throw-dice))
	 (t1   (first thrw))
	 (t2   (second thrw))
	 (thrw-sum (reduce #'+ thrw)))
    (format t
	    "(THROW ~a AND ~a -- ~a -- ~a)"
	    t1
	    t2
	    thrw-sum
	    (cond ((= thrw-sum p) "YOU WIN")
		  (t "THROW AGAIN")))))

#|

CL-USER> (try-for-point 6)
(THROW 3 AND 5 -- 8 -- THROW AGAIN)
NIL
CL-USER> (try-for-point 6)
(THROW 4 AND 6 -- 10 -- THROW AGAIN)
NIL
CL-USER> (try-for-point 6)
(THROW 2 AND 4 -- 6 -- YOU WIN)
NIL
CL-USER> (craps)
(THROW 2 AND 2 -- 4 -- YOUR POINT IS 4)
NIL
CL-USER> (try-for-point 4)
(THROW 3 AND 6 -- 9 -- THROW AGAIN)
NIL
CL-USER> (try-for-point 4)
(THROW 2 AND 2 -- 4 -- YOU WIN)
NIL

|#


;;;
;;; EXERCISE 6.26
;;;
 
(defvar delimiter '-VS-)


;;; a

(defun right-side (l)
  (rest (member delimiter l)))


;;; b

(defun left-side (l)
  (rest (member delimiter (reverse l))))


;;; c

(defun count-common (l r)
  (length (intersection l r)))


;;; d

(defun compare-features (l)
  (list (count-common (left-side l) (right-side l)) 'COMMON 'FEATURES))

(defvar f-1 '(large red shiny cube -VS- small shiny red four-sided pyramid))
(defvar f-2 '(small red metal cube -VS- red plastic small cube))

#|

CL-USER> (compare-features f-2)
(3 COMMON FEATURES)

|#



;;;
;;; EXERCISE 6.35
;;;

;;; a

(defvar nerd-states-successive
  '((Sleeping               . Eating)
    (Eating                 . Waiting-for-a-Computer)
    (Waiting-for-a-Computer . Programming)
    (Programming            . Debugging)
    (Debugging              . Sleeping)))


;;; b

(defun next-nerdus-state (s)
  (cdr (assoc s nerd-states-successive)))

#|

CL-USER> (next-nerdus-state 'Sleeping)
EATING
CL-USER> (next-nerdus-state 'Debugging)
SLEEPING

|#



;;; c

#|

CL-USER> (next-nerdus-state 'Playing-Guitar)
NIL

|#


;;; d

(defun sleepless-nerd (s)
  (let ((nds (next-nerdus-state s)))
    (if (equal nds 'Sleeping)
	(next-nerdus-state 'Sleeping)
	nds)))

#|

CL-USER> (sleepless-nerd 'Debugging)
EATING


|#


;;; e

(defun nerd-on-caffeine (s)
  (next-nerdus-state (next-nerdus-state s)))

#|

CL-USER> (nerd-on-caffeine 'Sleeping)
WAITING-FOR-A-COMPUTER
CL-USER> (nerd-on-caffeine 'Waiting-for-a-Computer)
DEBUGGING
CL-USER> (nerd-on-caffeine 'Debugging)
EATING

|#


;;; f

(defun cnt-states-from-to (sf st)
  "Takes two states (sf) and (st), of Nerdus on caffeine, and returns the 
   number of states Nerdus will have to go from sf to st."
  (labels
      ((helper (sf st ss)
	 (cond ((equal sf st) (values ss (length ss)))
	       (t
		(let ((sn (nerd-on-caffeine sf)))
		  (helper sn st (append ss (list sn))))))))

    (helper sf st NIL)))

#|

CL-USER> (cnt-states-from-to 'programming 'debugging)
(SLEEPING WAITING-FOR-A-COMPUTER DEBUGGING)
3

|#



;;;
;;; EXERCISE 6.41
;;;

(defvar rooms
  '(
    (living-room        (north front-stairs)
                        (south dining-room)
                        (east kitchen))

    (upstairs-bedroom   (west library)
                        (south front-stairs))

    (dining-room        (north living-room)
                        (east pantry)
                        (west downstairs-bedroom))

    (kitchen            (west living-room)
                        (south pantry))

    (pantry             (north kitchen)
                        (west dining-room))

    (downstairs-bedroom (north back-stairs)
                        (east dining-room))

    (back-stairs        (south downstairs-bedroom)
                        (north library))

    (front-stairs       (north upstairs-bedroom)
                        (south living-room))

    (library            (east upstairs-bedroom)
                        (south back-stairs))))



#|

CL-USER> (sdraw rooms)

[*|*]--->etc.
 |
 v
[*|*]-------->[*|*]------------------>[*|*]------------------>[*|*]--->NIL
 |             |                       |                       |
 v             v                       v                       v
LIVING-ROOM   [*|*]--->[*|*]--->NIL   [*|*]--->[*|*]--->NIL   [*|*]--->etc.
               |        |              |        |              |
               v        v              v        v              v
              NORTH    FRONT-STAIRS   SOUTH    DINING-ROOM    EAST

CL-USER> (scrawl rooms)
Crawl through list:  'H' for help, 'Q' to quit.


[*|*]--->etc.
 |
 v
[*|*]-------->[*|*]------------------>[*|*]------------------>[*|*]--->NIL
 |             |                       |                       |
 v             v                       v                       v
LIVING-ROOM   [*|*]--->[*|*]--->NIL   [*|*]--->[*|*]--->NIL   [*|*]--->etc.
               |        |              |        |              |
               v        v              v        v              v
              NORTH    FRONT-STAIRS   SOUTH    DINING-ROOM    EAST

'((LIVING-ROOM (NORTH FRONT-STAIRS) (SOUTH DINING-ROOM) (EAST KITCHEN)) (UP...)

SCRAWL> 

|#


;;; a

(defun choices (r)
  "Take the name of a room and returns the table of permissible directions
   Robbie may take from that room."
  (rest (assoc r rooms)))

#|

CL-USER> (choices 'pantry)
((NORTH KITCHEN) (WEST DINING-ROOM))

|#


;;; b

(defun look (r d)
  "Takes a room (r) and a direction (d), and tells Robbie where he would end
   up if he moved in that direction from that room."
  (car (rest (assoc d (choices r)))))

#|

CL-USER> (look 'pantry 'north)
KITCHEN
CL-USER> (look 'pantry 'south)
NIL
CL-USER> (look 'pantry 'west)
DINING-ROOM

|#


;;; c

#|

CL-USER> (defparameter *loc* NIL)
*LOC*
CL-USER> *loc*
NIL
CL-USER> (setf *loc* 'dining-room)
DINING-ROOM
CL-USER> *loc*
DINING-ROOM

|#


(defun set-robbie-location (r)
  "Moves Robbie to room r by setting the variable LOC."
  (setq *loc* r))


;;; d

(defun how-many-choices ()
  "Reads the global variable *LOC* and returns the number of choices that
   Robbie has for where to move next."
  (length (choices *loc*)))

#|

CL-USER> (how-many-choices)
3

|#


;;; e

(defun upstairsp (l)
  "Takes a location and returns true if it is upstairs."
  (or (equal l 'upstairs-bedroom) (equal l 'library)))

(defun onstairsp (l)
  "Takes a location and returns true if it is stairs."
  (or (equal l 'front-stairs) (equal l 'back-stairs)))


;;; f

(defun whr-is (l)
  "Takes a location and returns a list appropriate to it."
  (if (onstairsp l)
      '(on the)
      (progn
	(if (upstairsp l)
	  '(upstairs in the)
	  '(downstairs in the)))))

(defun where-is-robbie ()
  "Reads the global variable *LOC* and tells where Robbie is."
  (append '(Robbie is) (whr-is *loc*) (list *loc*)))

#|

CL-USER> (whr-is 'pantry)
(DOWNSTAIRS IN THE)
CL-USER> (setf *loc* 'front-stairs)
FRONT-STAIRS
CL-USER> (where-is-robbie)
(ROBBIE IS ON THE FRONT-STAIRS)
CL-USER> (setf *loc* 'upstairs-bedroom)
UPSTAIRS-BEDROOM
CL-USER> (where-is-robbie)
(ROBBIE IS UPSTAIRS IN THE UPSTAIRS-BEDROOM)

|#


;;; g

(defun move-robbie (d)
  "Takes a direction (d), )and sets Robbies location accordingly."
  (let ((pnl (look *loc* d)))
    (cond ((null pnl)
	   '(OUCH!!))
	  (t (set-robbie-location pnl)
	     (where-is-robbie)))))

#|

CL-USER> *loc*
KITCHEN
CL-USER> (setf *loc* 'pantry)
PANTRY
CL-USER> (move-robbie 'south)
OUCH!!
; No value
CL-USER> (move-robbie 'north)
(ROBBIE IS DOWNSTAIRS IN THE KITCHEN)

|#


;;; h

#|

CL-USER> (set-robbie-location 'pantry)
PANTRY
CL-USER> (move-robbie 'west)
(ROBBIE IS DOWNSTAIRS IN THE DINING-ROOM)
CL-USER> (move-robbie 'west)
(ROBBIE IS DOWNSTAIRS IN THE DOWNSTAIRS-BEDROOM)
CL-USER> (move-robbie 'north)
(ROBBIE IS ON THE BACK-STAIRS)
CL-USER> (move-robbie 'north)
(ROBBIE IS UPSTAIRS IN THE LIBRARY)
CL-USER> (move-robbie 'east)
(ROBBIE IS UPSTAIRS IN THE UPSTAIRS-BEDROOM)
CL-USER> (move-robbie 'south)
(ROBBIE IS ON THE FRONT-STAIRS)
CL-USER> (move-robbie 'south)
(ROBBIE IS DOWNSTAIRS IN THE LIVING-ROOM)
CL-USER> (move-robbie 'east)
(ROBBIE IS DOWNSTAIRS IN THE KITCHEN)

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



;;;
;;; EXERCISE 8.60
;;;

(defvar *family* '((colin nil nil)
		   (deirdre nil nil)
		   (arthur nil nil)
		   (kate nil nil)
		   (frank nil nil)
		   (linda nil nil)
		   (suzanne colin deirdre)
		   (bruce arthur kate)
		   (charles arthur kate)
		   (david arthur kate)
		   (ellen arthur kate)
		   (george frank linda)
		   (hillary frank linda)
		   (andre nil nil)
		   (tamara bruce suzanne)
		   (vincent bruce suzanne)
		   (wanda nil nil)
		   (ivan george ellen)
		   (julie george ellen)
		   (marie george ellen)
		   (nigel andre hillary)
		   (frederick nil tamara)
		   (zelda vincent wanda)
		   (joshua ivan wanda)
		   (quentin nil nil)
		   (robert quentin julie)
		   (olivia nigel marie)
		   (peter nigel marie)
		   (erica nil nil)
		   (yvette robert zelda)
		   (diane peter erica)))

(defun remove-nulls (x)
  "Takes a simple list (x), and returns another list without any NILs in x."
  (cond ((null x) NIL)
	((null (first x)) (remove-nulls (rest x)))
	(t (append (list (first x)) (remove-nulls (rest x))))))

#|

CL-USER> (remove-nulls NIL)
NIL
CL-USER> (remove-nulls '( NIL 1 2 () 3 NIL NIL 4 5 NIL NIL 6))
(1 2 3 4 5 6)

|#


(defun all-members (l)
  "Takes a table (l), and returns a list of all its members."
  (cond ((null l) NIL)
	(t (append (list (caar l)) (all-members (cdr l))))))

#|

CL-USER> (all-members *family*)
(COLIN DEIRDRE ARTHUR KATE FRANK LINDA SUZANNE BRUCE CHARLES DAVID ELLEN GEORGE
 HILLARY ANDRE TAMARA VINCENT WANDA IVAN JULIE MARIE NIGEL FREDERICK ZELDA
 JOSHUA QUENTIN ROBERT OLIVIA PETER ERICA YVETTE DIANE)

|#


(defun all-fathers (l)
  "Takes a tree as a table (l), and returns a list of all members who are
   fathers."
  (cond ((null l) NIL)
	(t (remove-duplicates
	    (remove-nulls
	     (append (list (cadar l)) (all-fathers (rest l))))))))

#|

CL-USER> (all-fathers NIL)
NIL
CL-USER> (all-fathers *family*)
(COLIN ARTHUR FRANK BRUCE GEORGE ANDRE VINCENT IVAN QUENTIN NIGEL ROBERT PETER)

|#
 

(defun all-mothers (l)
  "Takes a *family*-tree as a table (l), and returns a list of all members who
   are mothers."
  (cond ((null l) NIL)
	(t (remove-duplicates
	    (remove-nulls
	     (append (list (caddar l)) (all-mothers (rest l))))))))

#|

CL-USER> (all-mothers NIL)
NIL
CL-USER> (all-mothers *family*)
(DEIRDRE KATE LINDA SUZANNE ELLEN HILLARY TAMARA WANDA JULIE MARIE ZELDA ERICA)

|#


;;; a.

(defun father (n)
  "Takes a name (n), and returns the name of the person's father."
  (cond ((null n) NIL)
	(t (second (assoc n *family*)))))

(defun mother (n)
  "Takes a name (n), and returns the name of the person's mother."
  (cond ((null n) NIL)
	(t (third (assoc n *family*)))))

(defun parents (n)
  "Takes a name (n), and returns a list of the names of the person's parents."
  (cond ((null n) NIL)
	(t (remove-nulls (list (father n) (mother n))))))

(defun children (x)
  "Takes a name (x) of a family-member, and returns a list of the names of the
   person's children."
  (if (null x)
      NIL
      (children-helper x *family*)))

(defun children-helper (x l)
  (cond ((null l) NIL)
	((not (or (equal x (cadar l)) (equal x (caddar l))))
	 (children-helper x (cdr l)))
	(t (append (list (caar l)) (children-helper x (cdr l))))))

(defun descendents (x)
  "Takes a person (x), and returns the descendents of x, including the 
   children and grandchildren."
  (let ((cn (children x)))
    (append cn (flatten (mapcar #'children cn)))))
;DESCENDENTS

(defun all-children (x)
  "Takes a list (x) of the names of family-members, and returns a list of 
   names of children of each member of x."
  (cond ((null x) NIL)
	((atom x) 'ERROR-NOT-A-LIST-OF-NAMES)
	(t
	 (remove-duplicates
	  (append (children (first x)) (all-children (rest x)))))))

#|

CL-USER> (all-children NIL)
NIL
CL-USER> (all-children 'Arthur)
ERROR-NOT-A-LIST-OF-NAMES
CL-USER> (all-children '(Arthur Kate))
(BRUCE CHARLES DAVID ELLEN)
CL-USER> (all-children '(Arthur Colin))
(BRUCE CHARLES DAVID ELLEN SUZANNE)

|#


;;; b.

(defun siblings (x)
  "Takes the name (x) of a family-member, and returns the siblings of x."
  (if (null x)
      NIL
      (remove-if #'(lambda (y) (if (equal x y) T NIL)) (all-children (parents x)))))

#|

CL-USER> (siblings NIL)
NIL
CL-USER> (siblings 'Arthur)
NIL
CL-USER> (siblings 'Zelda)
(JOSHUA)
CL-USER> (siblings 'Bruce)
(CHARLES DAVID ELLEN)
CL-USER> (siblings 'Peter)
(OLIVIA)

|#

  
;;; c.

(defun mapunion (f l)
  "Takes a function (f) and a list (l), and returns the list obtained by 
   applying f to every element of l and computing the union of all the
   results."
  (remove-duplicates (flatten (mapcar f l))))

#|

CL-USER> (mapunion #'rest '((1 A B C) (2 E C J) (3 F A B C D)))
(E J F A B C D)

|#


;;; d.

(defun grandparents (p)
  "Takes a person (p), and returns a list of the grandparents of p."
  (mapunion #'parents (parents p)))

#|

CL-USER> (grandparents 'tamara)
(ARTHUR KATE COLIN DEIRDRE)
CL-USER> (grandparents 'marie)
(FRANK LINDA ARTHUR KATE)
CL-USER> (grandparents 'yvette)
(QUENTIN JULIE VINCENT WANDA)
CL-USER> (grandparents 'diane)
(NIGEL MARIE)

|#


;;; e.

(defun cousins (p)
  "Takes a person (p), and returns all the cousins of p."
  (mapunion #'children (flatten (mapcar #'siblings (parents p)))))

#|

CL-USER> (cousins 'julie)
(NIGEL TAMARA VINCENT)

|#


;;; f.

(defun parent-p (p c)
  "Takes two persons (p) and (c), and returns T if p is a parent of c, and NIL
   otherwise."
  (member p (parents c)))

(defun descended-from-p (d a)
  "Takes two persons (d, for descendent) and (a, for ancestor), and returns
   T if d is descended from a and NIL otherwise."
  (cond ((null d) NIL)
	((parent-p a d) T)
	(t (or (descended-from-p (father d) a)
	       (descended-from-p (mother d) a)))))

#|

CL-USER> (descended-from-p 'tamara 'linda)
NIL
CL-USER> (descended-from-p 'tamara 'arthur)
T

|#


;;; g.

(defun ancestors (p)
  "Takes a person (p), and returns a list of all the ancestors of p, which
   include the parents of p and all of the parents' ancestors."
  (cond ((null (parents p)) NIL)
	(t (union (parents p)
		  (union (ancestors (father p))
			 (ancestors (mother p)))))))

#|

CL-USER> (ancestors 'marie)
(GEORGE ELLEN FRANK LINDA ARTHUR KATE)
CL-USER> (ancestors 'colin)
NIL
CL-USER> (ancestors 'deirdre)
NIL
CL-USER> (ancestors 'suzanne)
(COLIN DEIRDRE)
CL-USER> (ancestors 'bruce)
(ARTHUR KATE)
CL-USER> (ancestors 'tamara)
(BRUCE SUZANNE ARTHUR KATE COLIN DEIRDRE)
CL-USER> (ancestors 'ivan)
(GEORGE ELLEN FRANK LINDA ARTHUR KATE)
CL-USER> (ancestors 'nigel)
(ANDRE HILLARY FRANK LINDA)
CL-USER> (ancestors 'frederick)
(TAMARA BRUCE SUZANNE ARTHUR KATE COLIN DEIRDRE)
CL-USER> (ancestors 'peter)
(NIGEL MARIE ANDRE HILLARY FRANK LINDA GEORGE ELLEN FRANK LINDA ARTHUR KATE)
WARNING: redefining COMMON-LISP-USER::ANCESTORS in DEFUN
CL-USER> (ancestors 'peter)
(LINDA FRANK ANDRE HILLARY ARTHUR KATE GEORGE ELLEN NIGEL MARIE)

|#


;;; h.

(defun generation-gap (p1 p2)
  "Takes two person (p1) and (p2), and returns the number of generations 
   separating p1 from p2 in the family-tree."
  (cond ((null p1) 0)
	((null (parents p1)) 0)
	((parent-p p2 p1) 1)
	(t
	 (max (+ 1 (generation-gap (father p1) p2))
	     (+ 1 (generation-gap (mother p1) p2))))))

#|

CL-USER> (generation-gap 'suzanne 'colin)
1
CL-USER> (generation-gap 'frederick 'suzanne)
2
CL-USER> (generation-gap 'frederick 'colin)
3
CL-USER> (generation-gap 'zelda 'wanda)
1
CL-USER> (generation-gap 'zelda 'colin)
3
CL-USER> (generation-gap 'yvette 'colin)
4
CL-USER> (generation-gap 'diane 'frank)
4

|#


;;; i.

#|

1. Is Robert descended from Deirdre?
CL-USER> (descended-from-p 'robert 'deirdre)
NIL

2. Who are Yvette’s ancestors?
CL-USER> (ancestors 'yvette)
(WANDA VINCENT SUZANNE BRUCE ARTHUR KATE DEIRDRE COLIN LINDA FRANK GEORGE ELLEN
 QUENTIN JULIE ROBERT ZELDA)

3. What is the generation gap between Olivia and Frank?
CL-USER> (generation-gap 'olivia 'frank)
3

4. Who are Peter’s cousins?
CL-USER> (cousins 'peter)
(JOSHUA ROBERT)

5. Who are Olivia’s grandparents?
CL-USER> (grandparents 'olivia)
(ANDRE HILLARY GEORGE ELLEN)

|#




;;;
;;; EXERCISE 9.10
;;;

;;; SKIP


 
;;;
;;; EXERCISE 10.8
;;;

;;; TODO



;;;
;;; EXERCISE 11.22a
;;;

(defun complement-base (b)
  "Takes a base and returns its complement. A's complement is T, C's complement
   is G."
  (cond ((equal b 'A) 'T)
	((equal b 'C) 'G)
	((equal b 'G) 'C)
	((equal b 'T) 'A)))

#|

CL-USER> (complement-base 'A)
T
CL-USER> (complement-base NIL)
NIL

|#


;;;
;;; EXERCISE 11.22b
;;;

(defun complement-strand (s)
  "Takes a strand (list) of DNA bases and returns the complement of each base
   in the strand."
  (do* ((x s (rest x))
        (b (first x) (first x))
	(bc (list (complement-base b)) (list (complement-base b)))
	(cs bc (append cs bc)))
       ((null (rest x)) cs)))

;;; b.

#|

CL-USER> (complement-strand '())
(NIL)
CL-USER> (complement-strand '(A A C G G C T T C))
(T T G C C G A A G)

|#


;;;
;;; EXERCISE 11.22c
;;;

(defun make-double (s)
  "Takes a strand (s) of bases, and returns a list of pairs of bases and their
   complement."
  (do* ((bs s (rest bs))
	(cs (complement-strand bs) (rest cs))
	(p (list (first bs) (first cs)) (list (first bs) (first cs)))
	(ps (list p) (append ps (list p))))
       ((null (rest bs)) ps)))

#|

CL-USER> (make-double '())
((NIL NIL))
CL-USER> (make-double '(A C))
((A T) (C G))
CL-USER> (make-double '(A A C G G C T T C))
((A T) (A T) (C G) (G C) (G C) (C G) (T A) (T A) (C G))

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



;;;
;;; EXERCISE 12.4
;;;

;;; a.

(defstruct dn-node
  (name 'NAME_OF_NODE)
  (question "Question to ask.")
  (yes-case 'YES_CASE)
  (no-case 'NO_CASE))


;;; b.

(defparameter *dn-node-list* NIL)

(defun init-dn-node-list ()
  "Initializes the global variable to NIL."
  (setf *dn-node-list* NIL))

#|

CL-USER> (init-dn-node-list)
NIL

|#


;;; c.

(defun add-dn-node (n q yc nc)
  "Takes the various conponents of a DN node, i.e. a name (n), question (q),
   yes-case (yc), and a no-case (nc), and returns the corresponding dn-node."
  (setf *dn-node-list*
	(append *dn-node-list*
		(list (make-dn-node :name n
			      :question q
			      :yes-case yc
			      :no-case nc)))))

#|

CL-USER> (add-dn-node 'start 
		      "Does the engine turn over?"
		      'engine-turns-over
		      'engine-wont-turn-over)
CL-USER> (add-dn-node 'engine-turns-over
		      "Will the engine run for any period of time?"
		      'engine-will-run-briefly
		      'engine-wont-run)
CL-USER> (add-dn-node 'engine-wont-run
		      "Is there gas in the tank?"
		      'gas-in-tank
		      "Fill the tank and try starting the engine again.")
CL-USER> (add-dn-node 'engine-wont-turn-over
		      "Do you hear any sound when you turn the key?"
		      'sound-when-turn-key
		      'no-sound-when-turn-key)
CL-USER> (add-dn-node 'no-sound-when-turn-key
		      "Is the battery voltage low?"
		      "Replace the battery"
		      'battery-voltage-ok)
CL-USER> (add-dn-node 'battery-voltage-ok
		      "Are the battery cables dirty or loose?"
		      "Clean the cables and tighten the connections."
		      'battery-cables-good)

(#S(DN-NODE
    :NAME START
    :QUESTION "Does the engine turn over?"
    :YES-CASE ENGINE-TURNS-OVER
    :NO-CASE ENGINE-WONT-TURN-OVER)
 #S(DN-NODE
    :NAME ENGINE-TURNS-OVER
    :QUESTION "Will the engine run for any period of time?"
    :YES-CASE ENGINE-WILL-RUN-BRIEFLY
    :NO-CASE ENGINE-WONT-RUN)
 #S(DN-NODE
    :NAME ENGINE-WONT-RUN
    :QUESTION "Is there gas in the tank?"
    :YES-CASE GAS-IN-TANK
    :NO-CASE "Fill the tank and try starting the engine again.")
 #S(DN-NODE
    :NAME ENGINE-WONT-TURN-OVER
    :QUESTION "Do you hear any sound when you turn the key?"
    :YES-CASE SOUND-WHEN-TURN-KEY
    :NO-CASE NO-SOUND-WHEN-TURN-KEY)
 #S(DN-NODE
    :NAME NO-SOUND-WHEN-TURN-KEY
    :QUESTION "Is the battery voltage low?"
    :YES-CASE "Replace the battery"
    :NO-CASE BATTERY-VOLTAGE-OK)
 #S(DN-NODE
    :NAME BATTERY-VOLTAGE-OK
    :QUESTION "Are the battery cables dirty or loose?"
    :YES-CASE "Clean the cables and tighten the connections."
    :NO-CASE BATTERY-CABLES-GOOD))

|#


;;; e.

(defun find-dn-node (n)
  "Takes a name (n), and returns the corresponding node in *DN-NODE-LIST* or
   NIL if it doesn't exist."
  (or (find n *dn-node-list* :key #'dn-node-name)
      (format t "No node exists named ~A.~%" n)))

#|

CL-USER> (find-dn-node 'start)
#S(DN-NODE
   :NAME START
   :QUESTION "Does the engine turn over?"
   :YES-CASE ENGINE-TURNS-OVER
   :NO-CASE ENGINE-WONT-TURN-OVER)
CL-USER> (find-dn-node 'battery-voltage-ok)
#S(DN-NODE
   :NAME BATTERY-VOLTAGE-OK
   :QUESTION "Are the battery cables dirty or loose?"
   :YES-CASE "Clean the cables and tighten the connections."
   :NO-CASE BATTERY-CABLES-GOOD)
CL-USER> (find-dn-node 'no-sound-when-turn-key)
#S(DN-NODE
   :NAME NO-SOUND-WHEN-TURN-KEY
   :QUESTION "Is the battery voltage low?"
   :YES-CASE "Replace the battery"
   :NO-CASE BATTERY-VOLTAGE-OK)

|#


;;; e.

(defun process-dn-node (n)
  "Takes as input the name (n) of a DN-NODE. If it can’t find the node, it 
   prints a message that the node hasn’t been defined yet, and returns NIL.
   Otherwise it asks the user the question associated with that node, and 
   then returns the node’s yes action or no action depending on how the user 
   responds."
  (let ((dnn (find-dn-node n)))
    (cond ((null dnn) (format t "Node not yet defined. ~%"))
	  (t
	   (let ((q (dn-node-question dnn)))
	     (if (y-or-n-p q)
		 (dn-node-yes-case dnn)
		 (dn-node-no-case dnn)))))))
	     
#|

CL-USER> (process-dn-node 'stop)
No node exists named STOP.
Node not yet defined. 
NIL
CL-USER> (process-dn-node 'start)

Does the engine turn over? (yes or no) yes

ENGINE-TURNS-OVER
CL-USER> (process-dn-node 'start)

Does the engine turn over? (yes or no) no

ENGINE-WONT-TURN-OVER
CL-USER> (process-dn-node 'battery-voltage-ok)

Are the battery cables dirty or loose? (yes or no) no

BATTERY-CABLES-GOOD

|#


;;; f.

(defun run-dn (cn)
  "Runs the discrimination network beginning with START."	 
  (cond ((null cn) 'DONE)
	((stringp cn) (format t "~S~%" cn))
	(t (run-dn (process-dn-node cn)))))

#|

CL-USER> (run-dn 'START)
Does the engine turn over? (y or n) y

Will the engine run for any period of time? (y or n) n

Is there gas in the tank? (y or n) n
"Fill the tank and try starting the engine again."
NIL

|#


(defun add-new-dn-node ()
  "Prompts the user for all aspects of a DN-NODE include its name, question
   yes-case and no-case, builts a DN-NODE from the user's responses, and adds
   the new-node to the list of DN-NODES."
  (let ((n  (progn
	      (format t "Node name: ")
	      (read)))
	(q  (progn
	      (format t "Question:  ")
	      (read)))
	(yc (progn
	      (format t "If 'Yes':  ")
	      (read)))
	(nc (progn
	      (format t "If 'No':   ")
	      (read))))
    (add-dn-node n q yc nc)))


;;; h.

#|

CL-USER> (add-new-dn-node)
Node name: 'engine-will-run-briefly
Question:  "Does the engine stall when cold but not when warm?"
If 'Yes':  'stalls-only-when-cold
If 'No':   'stalls-even-when-warm

(#S(DN-NODE
    :NAME 'ENGINE-WILL-RUN-BRIEFLY
    :QUESTION "Does the engine stall when cold but not when warm?"
    :YES-CASE 'STALLS-ONLY-WHEN-COLD
    :NO-CASE 'STALLS-EVEN-WHEN-WARM))
WARNING: redefining COMMON-LISP-USER::ADD-NEW-DN-NODE in DEFUN
CL-USER> (add-new-dn-node)
Node name: 'stalls-only-when-cold
Question:  "Is the cold idle speed at least 700rpm?"
If 'Yes':  'cold-idle-speed-normal
If 'No':   "Adjust the cold idle speed."

(#S(DN-NODE
    :NAME 'ENGINE-WILL-RUN-BRIEFLY
    :QUESTION "Does the engine stall when cold but not when warm?"
    :YES-CASE 'STALLS-ONLY-WHEN-COLD
    :NO-CASE 'STALLS-EVEN-WHEN-WARM)
 #S(DN-NODE
    :NAME 'STALLS-ONLY-WHEN-COLD
    :QUESTION "Is the cold idle speed at least 700rpm?"
    :YES-CASE 'COLD-IDLE-SPEED-NORMAL
    :NO-CASE "Adjust the cold idle speed."))

CL-USER> *dn-node-list*
(#S(DN-NODE
    :NAME 'ENGINE-WILL-RUN-BRIEFLY
    :QUESTION "Does the engine stall when cold but not when warm?"
    :YES-CASE 'STALLS-ONLY-WHEN-COLD
    :NO-CASE 'STALLS-EVEN-WHEN-WARM)
 #S(DN-NODE
    :NAME 'STALLS-ONLY-WHEN-COLD
    :QUESTION "Is the cold idle speed at least 700rpm?"
    :YES-CASE 'COLD-IDLE-SPEED-NORMAL
    :NO-CASE "Adjust the cold idle speed."))

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

