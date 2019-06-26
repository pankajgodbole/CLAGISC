;;;
;;; EXERCISE 8.1
;;;

#|

Answer:
The cond clause (oddp (first x) ...) is never true in this case.

|#


;;;
;;; EXERCISE 8.3
;;;

#|

CL-USER> (fact 20)
2432902008176640000

CL-USER> (fact 20.0)
2.432902e18

|#
  


;;;
;;; EXERCISE 8.9
;;;

(defun rec-nth (n l)
  "Takes a number and a list, and returns the member of the list
   corresponding to the number (beginning with 0)."
  (rec-nth-helper n l 0))
    
(defun rec-nth-helper (n l i)
  (cond ((null l) NIL)
	((equal i n) (first l))
	(t (rec-nth-helper n (rest l) (+ i 1)))))

#|

CL-USER> (rec-nth 2 '(1 2 3))
3

|#


(defun rec-nth-2 (n l)
  "Takes a number and a list, and returns the member of the list
   corresponding to the number (beginning with 0)."
  (cond ((null l) NIL)
        ((zerop n) (first l))
	(t (rec-nth-2 (- n 1) (rest l)))))

#|

CL-USER> (rec-nth-2 0 '(1 2))
1

|#


;;;
;;; EXERCISE 8.11
;;;

(defun fib (n)
  "Takes a number,n, and returns the nth number in the Fibonacci sequence."
  ;(break "Inside fib.")
  (if (<= n 1)
      ;(/ 1 0)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

#|

CL-USER> (fib 10)
55

|#


;;;
;;; EXERCISE 8.12
;;;

(defun any-7-p (l)
  (cond ((equal (first l) 7) t)
	(t (any-7-p (rest l)))))

#|

CL-USER> (trace any-7-p)
(ANY-7-P)

The function will recurse infinitely on input NIL.

|#


;;;
;;; EXERCISE 8.13
;;;

#|

Answer:
fact will recurse infinitely on any negative input.

|#


;;;
;;; EXERCISE 8.17
;;;

(defun find-first-odd-with-tail-rec (x)
  (cond ((null x) NIL)
	((oddp (first x)) (first x))
	(T (find-first-odd-with-tail-rec (rest x)))))

#|

CL-USER> (find-first-odd-with-tail-rec '(0 2 4 6 8 1))
1
CL-USER> (find-first-odd-with-tail-rec '())
NIL
CL-USER> (find-first-odd-with-tail-rec '(0 2 4 6 8 10))
NIL

|#


;;;
;;; EXERCISE 8.18
;;;

(defun find-last-element (l)
  "Takes an arbitrarily-nested list (l), and returns its last element."
  (cond ((atom (cdr l)) (car l))
	(t (find-last-element (rest l)))))

#|

CL-USER> (find-last-element NIL)
NIL
CL-USER> (find-last-element '(a))
A
CL-USER> (find-last-element '((((a f)) i) r))
R
CL-USER> (find-last-element '(((()))))
((NIL))

|#


;;;
;;; EXERCISE 8.22
;;;

(defun all-equal (l)
  "Takes a list (l) and returns T if all its members are equal to each other,
   and NIL otherwise."
  (cond ((< (length l) 2) T)
	((not (equal (first l) (second l))) NIL)
	(t (all-equal (rest l)))))

#|

CL-USER> (all-equal NIL)
T
CL-USER> (all-equal '(a))
T
CL-USER> (all-equal '(i i i i))
T
CL-USER> (all-equal '(i i e i))
NIL

|#
 

;;;
;;; EXERCISE 8.24
;;;

(defun count-down (n)
  "Takes a positive number (n), and returns a list of numbers starting with n,
   and counting down to 1."
  (cond ((< n 1) NIL)
	(t (cons n (count-down (- n 1))))))

#|

CL-USER> (count-down 0)
NIL
CL-USER> (count-down -12)
NIL
CL-USER> (count-down 1)
(1)
CL-USER> (count-down 10)
(10 9 8 7 6 5 4 3 2 1)

|#



;;;
;;; EXERCISE 8.26
;;;

(defun count-down-to-zero-1 (n)
  "Takes a positive number (n), and returns a list of numbers starting with n,
   and counting down to 0."
  (cond ((< n 0) NIL)
	(t (cons n (count-down-to-zero-1 (- n 1))))))

#|

CL-USER> (count-down-to-zero-1 10)
(10 9 8 7 6 5 4 3 2 1 0)

|#


;;;
;;; EXERCISE 8.27
;;;

(defun square-list-recursively (l)
  "Takes a list (l) of numbers, and returns a list of their squares."
  (cond ((null l) NIL)
	(t (append (list (* (first l) (first l)))
		   (square-list-recursively (rest l))))))

#|

CL-USER> (square-list-recursively NIL)
NIL
CL-USER> (square-list-recursively '(0))
(0)
CL-USER> (square-list-recursively '(1))
(1)
CL-USER> (square-list-recursively '(1 2 3 4 5 6))
(1 4 9 16 25 36)

|#


;;;
;;; EXERCISE 8.28
;;;

(defun nth-recursively-1 (n l)
  "Takes a positive number (n) and a list (l) and returns the nth element of
   the list. The function returns immediately if n exceeds the length of the
   list minus one."
  (cond ((or (< n 0) (> n (- (length l) 1))) NIL)
	((zerop n) (first l))
	(t (nth-recursively-1 (- n 1) (rest l)))))

(nth-recursively-1 0 '())
;NIL

(nth-recursively-1 0 '(a))
;A

(nth-recursively-1 3 '(a b c d))
;D

(nth-recursively-1 60 '(a b c d))
;NIL


;;;
;;; EXERCISE 8.29
;;;

(defun my-member (v l)
  "Takes a value (v) and a list (l), and returns T if v is a member of l, or
   NIL otherwise."
  (cond ((null l) NIL)
	((equal v (first l)) T)
	(t (my-member v (rest l)))))

#|

CL-USER> (my-member 1 NIL)
NIL
CL-USER> (my-member 1 '(1))
T
CL-USER> (my-member NIL NIL)
NIL
CL-USER> (my-member NIL '(1))
NIL

|#


;;;
;;; EXERCISE 8.31
;;;

(defun compare-lengths (x y)
  "Takes two lists (x, y), compares their lengths, and returns FIRST-IS-LONGER,
   SECOND-IS-LONGER or BOTH-ARE-THE-SAME-LENGTH if the first list is longer, 
   the second list is longer, or both are equal in size, respectively."
  (cond ((and (null x) (null y)) 'BOTH-ARE-THE-SAME-LENGTH)
	((and (not (null x)) (null y)) 'FIRST-IS-LONGER)
	((and (null x) (not (null y))) 'SECOND-IS-LONGER)
	(t (compare-lengths (rest x) (rest y)))))

#|

CL-USER> (compare-lengths NIL NIL)
BOTH-ARE-THE-SAME-LENGTH
CL-USER> (compare-lengths NIL '(a))
SECOND-IS-LONGER
CL-USER> (compare-lengths '(a) NIL)
FIRST-IS-LONGER
CL-USER> (compare-lengths '(a) '(a))
BOTH-ARE-THE-SAME-LENGTH

|#


;;;
;;; EXERCISE 8.32
;;;

(defun sum-numeric-elements (l)
  "Takes a list (l), and returns the sum of only the numeric elements of the
   list, while ignoring the others."
  (cond ((null l) 0)
	((numberp (first l)) (+ (first l) (sum-numeric-elements (rest l))))
	(t (sum-numeric-elements (rest l)))))

#|

CL-USER> (sum-numeric-elements NIL)
0
CL-USER> (sum-numeric-elements '(a))
0
CL-USER> (sum-numeric-elements '(1))
1
CL-USER> (sum-numeric-elements '(3 bears 3 bowls and 1 girl))
7

|#


;;;
;;; EXERCISE 8.36
;;;

(defun count-odds (l)
  "Takes a list (l) of numbers, and returns the number of odd numbers among
   them."
  (cond ((null l) 0)
	((oddp (first l)) (+ 1 (count-odds (rest l))))
	(t (count-odds (rest l)))))

#|

CL-USER> (count-odds NIL)
0
CL-USER> (count-odds '(0))
0
CL-USER> (count-odds '(0 1 2 3 4 5))
3
CL-USER> (count-odds '(4 5 6 7 8))
2

|#


;;;
;;; EXERCISE 8.38
;;; 

(defun atoms-to-q-wo-null (x)
  "Takes a binary-tree (x), and returns a new binary-tree in which every
   non-NIL atom in x is replaced by Q."
  (cond ((atom x) 'Q)
	(t (cons (atoms-to-q (car x)) (atoms-to-q (cdr x))))))

#|

CL-USER> (atoms-to-q-wo-null '(a .b))
(Q Q)
CL-USER> (atoms-to-q-wo-null '(hark (harold the angel) sings))
(Q (Q Q Q) Q)

Answer:
The effect would be the same as before since NIL is also an atom.

|#


;;;
;;; EXERCISE 8.39
;;;

(defun count-atoms (x)
  "Takes a binary-tree (x), and returns the number of atoms (including NIL)
   present in it."
  (cond ((null x) 1)
	((atom x) 1)
	(t (+ (count-atoms (car x)) (count-atoms (cdr x))))))

#|

CL-USER> (count-atoms NIL)
1
CL-USER> (count-atoms '(a b c d e))
6

|#


;;;
;;; EXERCISE 8.40
;;;

(defun count-conses (x)
  "Takes a binary-tree (x), and returns the number of cons-cells present
   in it."
  (cond ((null x) 0)
	((atom x) 0)
	(t (+ 1 (count-conses (car x)) (count-conses (cdr x))))))

#|

CL-USER> (count-conses NIL)
0
CL-USER> (count-conses 'fred)
0
CL-USER> (count-conses '(foo))
1
CL-USER> (count-conses '(foo bar))
2
CL-USER> (count-conses '((foo)))
2

|#


;;;
;;; EXERCISE 8.43
;;;

(defun sum-tree (x)
  "Takes a binary-tree (x) as input, and returns the sum of all the numbers
   appearing in, whi le ignoring all the elements which are not numbers."
  (cond ((null x) 0)
	((numberp x) x)
	((and (not (numberp x)) (atom x)) 0)
	(t (+ (sum-tree (car x)) (sum-tree (cdr x))))))

#|

CL-USER> (sum-tree NIL)
0
CL-USER> (sum-tree '(1))
1
CL-USER> (sum-tree '((3 BEARS) (3 BOWLS) (1 GIRL)))
7

|#


;;;
;;; EXERCISE 8.40
;;;

(defun flatten (l)
  "Takes an arbitrarily-nested list (l), and returns all its elements in a 
   single-level list."
  (cond ((null l) NIL)
	((atom l) (list l))
	(t (append (flatten (car l)) (flatten (cdr l))))))

#|

CL-USER> (flatten NIL)
NIL
CL-USER> (flatten '(a))
(A)
CL-USER> (flatten '(a (b)))
(A B)
CL-USER> (FLATTEN '((A B (R)) A C (A D ((A (B)) R) A)))
(A B R A C A D A B R A)

|#


;;;
;;; EXERCISE 8.44
;;;

(defun tree-depth (x)
  "Takes a binary-tree (x), and returns its depth."
  (cond ((null x) 0)
	((atom x) 0)
	(t (+ 1 (max (tree-depth (car x)) (tree-depth (cdr x)))))))

#|

CL-USER> (tree-depth NIL)
0
CL-USER> (tree-depth '(a))
1
CL-USER> (tree-depth '(a . b))
1
CL-USER> (TREE-DEPTH '((A . B) . (C . D)))
2
CL-USER> (tree-depth '(a b c))
3
CL-USER> (tree-depth '((a b c d)))
5
CL-USER> (tree-depth '((3 BEARS) (3 BOWLS) (1 GIRL)))
9
CL-USER> (tree-depth '(((GOLDILOCKS . AND)) (THE . 3) BEARS))
6
CL-USER> (tree-depth '((A B (R)) A C (A D ((A (B)) R) A)))
11

|#


;;;
;;; EXERCISE 8.45
;;;

(defun paren-depth (l)
  "Takes an arbitrarily-nested list (l), and returns the depth of the 
   parentheses in the list."
  (cond ((null l) 0)
	((atom l) 0)
	(t (+ 1 (paren-depth (car l)) (paren-depth (cadr l))))))

#|

CL-USER> (paren-depth '(a))
1
CL-USER> (paren-depth '(a b c))
1
CL-USER> (paren-depth '((a)))
2
CL-USER> (paren-depth '((a b c d)))
2
CL-USER> (paren-depth '(a b ((c) d) e))
1

|#


;;;
;;; EXERCISE 8.46
;;;

(defun count-up-using-no-helper (n v)
  "Takes a number (n) and a starting value (v), and displays all numbers from
   v upto and including n, without making use of a helper function."
  (cond ((> v n) NIL)
	(t (append (list v) (count-up-using-no-helper n (+ v 1))))))

#|

CL-USER> (count-up-using-no-helper 5 6)
NIL
CL-USER> (count-up-using-no-helper 5 5)
(5)
CL-USER> (count-up-using-no-helper 5 1)
(1 2 3 4 5)
CL-USER> (count-up-using-no-helper 5 -10)
(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5)

|#


(defun count-up-using-no-helper-2 (n)
  "Takes a number (n), and displays all the numbers from 1 upto and including
   n, while not making use of a helper function."
  (cond ((<= n 1) (list 1))
	(t (append
	    (count-up-using-no-helper-2 (- n 1))
	    (list n)))))

#|

CL-USER> (count-up-using-no-helper-2 1)
(1)
CL-USER> (count-up-using-no-helper-2 0)
(1)
CL-USER> (count-up-using-no-helper-2 -3)
(1)
CL-USER> (count-up-using-no-helper-2 3)
(1 2 3)

|#


;;;
;;; EXERCISE 8.47
;;;

(defun make-loaf (n)
  "Takes a number (n), and returns a loaf of size n."
  (if (< n 1)
      NIL
      (cons 'X (make-loaf (- n 1)))))

#|

CL-USER> (make-loaf 0)
NIL
CL-USER> (make-loaf 1)
(X)
CL-USER> (make-loaf 20)
(X X X X X X X X X X X X X X X X X X X X)

|#


;;;
;;; EXERCISE 8.48
;;;

(defun bury (i n)
  "Takes an item (i) and a level (l), and returns a list with i buried under
   n levels of parentheses."
  (cond ((< n 1) i)
	(t (list (bury i (- n 1))))))

#|

CL-USER> (bury 'X 0)
X
CL-USER> (bury 'X 1)
(X)
CL-USER> (bury 'fred 5)
(((((FRED)))))

|#


;;;
;;; EXERCISE 8.49
;;;

(defun pairings (x y)
  "Takes two lists (x and y) of equal length, and returns a list of the pairs
   of elements of the two lists."
  (cond ((or (null x) (null y)) NIL)
	((or (atom x) (atom y)) NIL)
	(t (append
	    (list (list (car x) (car y)))
	    (pairings (cdr x) (cdr y))))))

#|

CL-USER> (pairings NIL NIL)
NIL
CL-USER> (pairings NIL 1)
NIL
CL-USER> (pairings 1 NIL)
NIL
CL-USER> (pairings '(a b c d) '(1 2))
((A 1) (B 2))
CL-USER> (pairings '(a b) '(1 2 3 4))
((A 1) (B 2))

|#
 

;;;
;;; EXERCISE 8.50
;;;

(defun sublists (l)
  "Takes a list (l), and returns all the sublists of l."
  (cond ((null l) NIL)
	((atom l) NIL)
	(t (append (list l) (sublists (rest l))))))

#|

CL-USER> (sublists NIL)
NIL
CL-USER> (sublists 'a)
NIL
CL-USER> (sublists '(a))
((A))
CL-USER> (sublists '(a b c))
((A B C) (B C) (C))
CL-USER> (sublists '(fee fie foe))
((FEE FIE FOE) (FIE FOE) (FOE))

|#
 

;;;
;;; EXERCISE 8.53
;;;

(defun largest-even (x)
  "Takes a list (x) of non-negative integers, and returns the largest even
   number in the list."
  (cond ((or (null x) (atom x)) 0)
	(t (if (or (< (car x) 0) (oddp (car x)))
	       (max 0 (largest-even (cdr x)))
	       (max (car x) (largest-even (cdr x)))))))
#|

CL-USER> (largest-even NIL)
0
CL-USER> (largest-even 0)
0
CL-USER> (largest-even '(1))
0
CL-USER> (largest-even '(-1 -2 2))
2
CL-USER> (largest-even '(1 2 3 4 5 7))
4
CL-USER> (largest-even '(5 2 4 3))
4

|#
 

;;;
;;; EXERCISE 8.54
;;;

(defun huge (n)
  "Takes a number (n), and returns the number obtained by raising n it to its
   own power."
  (helper-huge n 1))

(defun helper-huge (n i)
  "Takes a number (n) and a count (i), and returns the number obtained by 
   raising n to the power of i."  
  (cond ((<= n 1) 1)
	(t (if (> i n)
	       1
	       (* n (helper-huge n (+ i 1)))))))

#|

CL-USER> (huge 0)
1
CL-USER> (huge 1)
1
CL-USER> (huge 5)
3125
CL-USER> (huge 10)
10000000000

|#


;;;
;;; EXERCISE 8.55
;;;

(defun every-other (x)
  "Takes a list (x), and returns another list containing every other element
   from the input list."
  (cond ((null x) NIL)
	((atom x) (list x))
	(t (append (list (car x)) (every-other (cddr x))))))

#|

CL-USER> (every-other NIL)
NIL
CL-USER> (every-other '1)
(1)
CL-USER> (every-other '(1 2 3 4 5 6))
(1 3 5)
CL-USER> (every-other '(I came I saw I conquered))
(I I I)

|#


;;;
;;; EXERCISE 8.57
;;;

(defun left-half (x)
  "Takes a list (x), and returns another list containing just the left half
   of the elements of the input list."
  (left-half-helper x (ceiling (length x) 2)))

(defun left-half-helper (x i)  
  (cond ((or (null x) (zerop i)) NIL)
	(t (append (list (car x)) (left-half-helper (cdr x) (- i 1))))))

#|

CL-USER> (left-half NIL)
NIL
CL-USER> (left-half '(1 2))
(1)
CL-USER> (left-half '(1 2 3 4 5))
(1 2 3)

|#



(defun right-half (x)
  "Takes a list (x), and returns another list containing just the right half
   of the elements of the input list."
  (reverse (left-half-helper (reverse x) (floor (length x) 2))))

#|

CL-USER> (right-half NIL)
NIL
CL-USER> (right-half '(1 2))
(2)
CL-USER> (right-half '(1 2 3 4 5))
(4 5)

|#


;;;
;;; EXERCISE 8.58
;;;

(defun all-less-than (x n)
  "Takes a list (l) of numbers and another number, and returns a new list of
   numbers that are less than n."
  (cond ((null x) NIL)
	((>= (first x) n) (all-less-than (rest x) n))
	(t (append (list (first x)) (all-less-than (rest x) n)))))

#|

CL-USER> (all-less-than NIL 1)
NIL
CL-USER> (all-less-than '(1) 0)
NIL
CL-USER> (all-less-than '(1) 1)
NIL
CL-USER> (all-less-than '(-1) 0)
(-1)
CL-USER> (all-less-than '(-1 -2 -3 -4 3 2 1) 0)
(-1 -2 -3 -4)

|#


(defun all-greater-than (x n)
  "Takes a list (l) of numbers and another number, and returns a new list of
   numbers that are greater than n."
  (cond ((null x) NIL)
	((<= (first x) n) (all-greater-than (rest x) n))
	(t (append (list (first x)) (all-greater-than (rest x) n)))))

#|

CL-USER> (all-greater-than NIL 1)
NIL
CL-USER> (all-greater-than '(0) 1)
NIL
CL-USER> (all-greater-than '(1) 1)
NIL
CL-USER> (all-greater-than '(1) 0)
(1)
CL-USER> (all-greater-than '(-1 -2 -3 -4 3 2 1) 0)
(3 2 1)

|#


(defun all-equal-to (x n)
  "Takes a list (l) of numbers and another number, and returns a new list of
   numbers that are equal to n."
  (cond ((null x) NIL)
	((or (< (first x) n) (> (first x) n)) (all-equal-to (rest x) n))
	(t (append (list (first x)) (all-equal-to (rest x) n)))))

#|

CL-USER> (all-equal-to NIL 1)
NIL
CL-USER> (all-equal-to '(0) 1)
NIL
CL-USER> (all-equal-to '(1) 0)
NIL
CL-USER> (all-equal-to '(1) 1)
(1)
CL-USER> (all-equal-to '(0 -1 -2 -3 -4 3 2 1 0) 0)
(0 0)

|#


(defun sort-list (x)
  "Takes a list (x), and sorts it in increasing order." 
  (cond ((null x) NIL)
	(t (append (sort-list (all-less-than x (first x)))
		   (all-equal-to x (first x))
		   (sort-list (all-greater-than x (first x)))))))

#|

CL-USER> (sort-list NIL)
NIL
CL-USER> (sort-list '(1))
(1)
CL-USER> (sort-list '(1 2))
(1 2)
CL-USER> (sort-list '(5 -1 1 -1 -4 0 -2 -3 4 2 3 0))
(-4 -3 -2 -1 -1 0 0 1 2 3 4 5)

|#


(defun merge-lists (x y)
  "Takes two lists (x, y), whose inputs are in increasing order, and returns
   a new list that is a merger of the elements in its inputs, in order."
  (cond ((null x) y)
	((null y) x)
	((and (null x) (null y)) NIL)
	(t (sort-list (append x y)))))

#|

CL-USER> (merge-lists NIL NIL)
NIL
CL-USER> (merge-lists NIL '(1))
(1)
CL-USER> (merge-lists '(1) NIL)
(1)
CL-USER> (merge-lists '(1 2 6 8 10 12) '(2 3 5 9 13))
(1 2 2 3 5 6 8 9 10 12 13)

|#


;;;
;;; EXERCISE 8.61
;;;

(defun tr-count-up (n)
  "Takes a number (n), and displays all the numbers from 1 upto and including
   x, using a helper-function."
  (cond ((< n 1) NIL)
	(t (tr-count-up-helper n 1))))
  

(defun tr-count-up-helper (n i)
  "Takes a number (n) and a counter (i), and displays all the numbers from
   1 upto and including x, using tail-recursion."
  (cond ((> i n) NIL)
	(t (append (list i) (tr-count-up-helper n (+ i 1))))))

#|

CL-USER> (tr-count-up 0)
NIL
CL-USER> (tr-count-up 6)
(1 2 3 4 5 6)

|#


;;;
;;; EXERCISE 8.62
;;;

(defun tr-factorial (n)
  "Takes a number (n), and returns the factorial of n."
  (cond ((< n 1) 0)
	((= n 1) 1)
	(t (* n (tr-factorial (- n 1))))))

#|

CL-USER> (tr-factorial 0)
0
CL-USER> (tr-factorial -10)
0
CL-USER> (tr-factorial 1)
1
CL-USER> (tr-factorial 3)
6

|#


;;;
;;; EXERCISE 8.64
;;;

(defun tree-find-if (p x)
  "Takes a predicate function (p) which takes a single argument, and a 
   tree (x), and returns the first non-NIL atom which satisfies p."
  (cond ((null x) NIL)
	(t (tree-find-if-helper p (flatten x)))))

(defun tree-find-if-helper (p l)
  (cond ((null l) NIL)
	((funcall p (first l)) (first l))
	(t (tree-find-if p (rest l)))))

#|

CL-USER> (tree-find-if #'oddp NIL)
NIL
CL-USER> (tree-find-if #'oddp '((1)))
1
CL-USER> (tree-find-if #'oddp '((2)))
NIL
CL-USER> (tree-find-if #'oddp '((2 4) (5 6) 7))
5
CL-USER> (tree-find-if #'oddp '(((-17) (2 4) (5 6)) 7))
-17

|#


;;;
;;; EXERCISE 8.65
;;;

(defun tr-count-slices-using-labels (l)
  "Takes a list (l), and returns the number of elements in it making use of 
   the LABELS form."
  (labels ((tr-count-slices-helper (l cnt)
	     (if (null l)
		 cnt
		 (tr-count-slices-helper (rest l) (+ cnt 1)))))
    (tr-count-slices-helper l 0)))

#|

CL-USER> (tr-count-slices-using-labels NIL)
0
CL-USER> (tr-count-slices-using-labels '(1 2 3 4 5 6))
6

|#


(defun tr-reverse-using-labels (l)
  "Takes a list (l), and returns its reverse while making use of the LABELS
   form."
  (labels ((tr-reverse-helper-2 (l rslt)
	     (if (null l)
		 rslt
		 (tr-reverse-helper-2 (rest l)
				      (cons (first l) rslt)))))
    (tr-reverse-helper-2 l NIL)))

#|

CL-USER> (tr-reverse-using-labels NIL)
NIL
CL-USER> (tr-reverse-using-labels '(1))
(1)
CL-USER> (tr-reverse-using-labels '(1 2 3 4 5 6))
(6 5 4 3 2 1)

|#


;;;
;;;  EXERCISE 8.66
;;;

(defun arith-eval-2 (e)
  (cond ((numberp e) e)
	(t (funcall (second e)
		    (arith-eval-2 (first e))
		    (arith-eval-2 (third e))))))

#|

CL-USER> (arith-eval-2 3)
3
CL-USER> (arith-eval-2 '(1 + 2))
3
CL-USER> (arith-eval-2 '(2 + (3 * 4)))
14

|#
  

;;;
;;;  EXERCISE 8.67
;;;

(defun legal-p (l)
  "Takes a list (l) representing an arithmetic expression, and returns true if
   l is a valid arithmetical expression, or NIL otherwise."
  (cond ((null l) NIL)
	((numberp l) T)
	((atom l)
	 (prog2
	     (format t
		     "~&ERROR: Not a valid operand: ~S~%"
		     l)
	     NIL))
	((< (length l) 3)
	 (prog2
	     (format t
		     "~&ERROR: Not enough inputs: ~S~%"
		     l)
	     NIL))
	(t
	 (labels
	     ((legal-p-helper (x op y)
		(format t
			"~&x=~S, op=~S, y=~S~%"
			x op y)
	   	(cond
		  ;; ((or
		  ;;   (or (null op) (null y))
		  ;;   (and (null x) (null op))
		  ;;   (and (null x) (null y))
		  ;; ;;  (and (null op) (null y))
		  ;;   )
		  ;;  (prog2
		  ;;      (format t
		  ;; 	       "~&ERROR: Not enough inputs: ~S~%"
		  ;; 	       l)
		  ;;      NIL))
		  ((and (null op) (null y)) (legal-p x))
		  ((listp x) (legal-p x))
		  ((not (numberp x))
		   (prog2
		       (format t
			       "~&ERROR: Not a valid operand: ~S~%"
			       x)
                       NIL))
		  ((not (numberp y))
		   (prog2
		       (format t
			       "~&ERROR: Not a valid operand: ~S~%"
			       y)
                       NIL))
		  ;; ((or (not (eq op '+)) (not (eq op '-)))
		  ;;  (prog2
		  ;;      (format t
		  ;; 	       "~&ERROR: Not a valid operator: ~S~%"
		  ;; 	       op)
                  ;;      NIL))
		  (t (legal-p-helper
		      (funcall op x y)
		      (cdddr l)
		      (cddddr l))))))

	   (legal-p-helper (first l) (second l) (third l))))))

#|

CL-USER> (legal-p NIL)
NIL
CL-USER> (legal-p 0)
T
CL-USER> (legal-p -1)
T
CL-USER> (legal-p 'A)
ERROR: Not a valid operand: A
NIL
CL-USER> (legal-p '(a b c d))
x=A, op=B, y=C
ERROR: Not a valid operand: A
NIL
CL-USER> (legal-p '+)
ERROR: Not a valid operand: +
NIL
CL-USER> (legal-p '(2 +))
ERROR: Not enough inputs: (2 +)
NIL
CL-USER> (legal-p '(- 3))
ERROR: Not enough inputs: (- 3)
NIL
CL-USER> (legal-p '(3 NIL NIL))
x=3, op=NIL, y=NIL
T
CL-USER> (legal-p '(- 3 3))
x=-, op=3, y=3
ERROR: Not a valid operand: -
NIL
CL-USER> (legal-p '(3 3 *))
x=3, op=3, y=*
ERROR: Not a valid operand: *
NIL
CL-USER> (legal-p '(3 + 3))
x=3, op=+, y=3
x=6, op=NIL, y=NIL
T
CL-USER> (legal-p '(3 * 3))
x=3, op=*, y=3
x=9, op=NIL, y=NIL
T
CL-USER> (legal-p '((2 * 2) - 3))
x=(2 * 2), op=-, y=3
x=2, op=*, y=2
x=4, op=NIL, y=NIL
T

|#

  
;;;
;;;  EXERCISE 8.68
;;;

#|

Answer:
 
Definition of "proper-list":
A proper-list is NIL, or any cons-cell whose CAR is a proper-list, and CDR 
is NIL.

|#
  

;;;
;;;  EXERCISE 8.69
;;;

#|

Answer:

Definition:
A positive number greater than 1 is a primer-number, or a proper-list whose
CAR is a prime-number, and whose CDR is a proper-list.

|#
  

;;;
;;;  EXERCISE 8.70
;;;

(defun factors (n)
  "Takes a positive number (n), and returns a list of the factors of n."
  (factors-helper n 2))

(defun factors-helper (n p)
  (cond ((< n 2) NIL)
	((zerop (rem n p))
	 (cons p (factors-helper (/ n p) p)))
	(t (factors-helper n (+ p 1)))))

#|

CL-USER> (factors 1)
NIL
CL-USER> (factors -1)
NIL
CL-USER> (factors 0)
NIL
CL-USER> (factors 2)
(2)
CL-USER> (factors 3)
(3)
CL-USER> (factors 5)
(5)
CL-USER> (factors 15)
(3 5)
CL-USER> (factors 60)
(2 2 3 5)

|#

  
(defun factors-tree (n)
  "Takes a positive-number (n), and returns the factors of n as a tree."
  (labels ((factors-tree-helper (n f)
	     (cond ((< n 2) NIL)
		   ((= n f) (list n))
		   ((zerop (rem n f))
		    (list n f (factors-tree-helper (/ n f) f)))
		   (t (factors-tree-helper n (+ f 1)))))
	   
	   (factors-tree-helper-2 (n f)
	     (cond ((< n 2) NIL)
		   ((zerop (rem n f))
		    (remove-nulls
		     (append (list n) (list f)
			     (list (factors-tree-helper-2 (/ n f) f)))))
		   (t (factors-tree-helper-2 n (+ f 1))))))
    
    (factors-tree-helper n 2)))

#|

CL-USER> (factors-tree 1)
NIL
CL-USER> (factors-tree 0)
NIL
CL-USER> (factors-tree -1)
NIL
CL-USER> (factors-tree 2)
(2)
CL-USER> (factors-tree 3)
(3)
CL-USER> (factors-tree 12)
(12 2 (6 2 (3)))
CL-USER> (factors-tree 60)
(60 2 (30 2 (15 3 (5))))

|#
