;;;
;;;
;;;

(declaim (optimize (speed 0) (debug 3)))


;;; -*- Mode: Lisp; Package: DTRACE -*-

;;; DTRACE is a portable alternative to the Common Lisp TRACE and UNTRACE
;;; macros.  It offers a more detailed display than most tracing tools.
;;;
;;; From the book "Common Lisp:  A Gentle Introduction to
;;;      Symbolic Computation" by David S. Touretzky.  
;;; The Benjamin/Cummings Publishing Co., 1990.
;;;
;;; This is the generic version.  It should work in any legal Common Lisp.
;;; Revised August, 2003, to work with ANSI Common Lisp and Allegro v6.
;;;
;;; User-level routines:
;;;   DTRACE  - same syntax as TRACE
;;;   DUNTRACE - same syntax as UNTRACE

;; (defpackage :dtrace
;;   (:use :common-lisp)
;;   (:export dtrace duntrace
;; 	   *dtrace-print-length* *dtrace-print-level*
;; 	   *dtrace-print-circle* *dtrace-print-pretty*
;; 	   *dtrace-print-array*))

;; (in-package :dtrace)	    

;; (eval-when (eval load)
;;   (shadowing-import '(dtrace duntrace) (find-package :common-lisp-user)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DTRACE and subordinate routines.

(defparameter *dtrace-print-length* 7)
(defparameter *dtrace-print-level*  4)
(defparameter *dtrace-print-circle* t)
(defparameter *dtrace-print-pretty* nil)
(defparameter *dtrace-print-array* *print-array*)

(defvar *traced-functions* nil)
(defvar *trace-level* 0)

(defmacro dtrace (&rest function-names)
  "Turns on detailed tracing for specified functions.  Undo with DUNTRACE."
  (if (null function-names)
      (list 'quote *traced-functions*)
      (list 'quote (mapcan #'dtrace1 function-names))))

(defun dtrace1 (name)
  (unless (symbolp name)
    (format *error-output* "~&~S is an invalid function name." name)
    (return-from dtrace1 nil))
  (unless (fboundp name)
    (format *error-output* "~&~S undefined function." name)
    (return-from dtrace1 nil))
  (eval `(untrace ,name))	;; if they're tracing it, undo their trace
  (duntrace1 name)		;; if we're tracing it, undo our trace
  (when (special-operator-p name)
    (format *error-output*
	    "~&Can't trace ~S because it's a special form." name)
    (return-from dtrace1 nil))
  (if (macro-function name)
      (trace-macro name)
      (trace-function name))
  (setf *traced-functions* (nconc *traced-functions* (list name)))
  (list name))

;;; The functions below reference DISPLAY-xxx routines that can be made
;;; implementation specific for fancy graphics.  Generic versions of
;;; these routines are defined later in this file.

(defmacro with-dtrace-printer-settings (&body body)
  `(let ((*print-length* *dtrace-print-length*)
	 (*print-level* *dtrace-print-level*)
	 (*print-circle* *dtrace-print-circle*)
	 (*print-pretty* *dtrace-print-pretty*)
	 (*print-array* *dtrace-print-array*))
     ,@body))

(defun trace-function (name)
  (let* ((formal-arglist (fetch-arglist name))
	 (old-defn (symbol-function name))
	 (new-defn
	  #'(lambda (&rest argument-list)
	      (let ((result nil))
		(display-function-entry name)
		(let ((*trace-level* (1+ *trace-level*)))
		  (with-dtrace-printer-settings
		   (show-function-args argument-list formal-arglist))
		  (setf result (multiple-value-list
				(apply old-defn argument-list))))
		(display-function-return name result)
		(values-list result)))))
    (setf (get name 'original-definition) old-defn)
    (setf (get name 'traced-definition) new-defn)
    (setf (get name 'traced-type) 'defun)
    (setf (symbol-function name) new-defn)))

(defun trace-macro (name)
  (let* ((formal-arglist (fetch-arglist name))
	 (old-defn (macro-function name))
	 (new-defn
	  #'(lambda (macro-args env)
	      (let ((result nil))
		(display-function-entry name 'macro)
		(let ((*trace-level* (1+ *trace-level*)))
		  (with-dtrace-printer-settings
		   (show-function-args macro-args formal-arglist))
		  (setf result (funcall old-defn macro-args env)))
	(display-function-return name (list result) 'macro)
		(values result)))))
    (setf (get name 'original-definition) old-defn)
    (setf (get name 'traced-definition) new-defn)
    (setf (get name 'traced-type) 'defmacro)
    (setf (macro-function name) new-defn)))

(defun show-function-args (actuals formals &optional (argcount 0))
  (cond ((null actuals) nil)
	((null formals) (handle-args-numerically actuals argcount))
	(t (case (first formals)
	     (&optional (show-function-args
			 actuals (rest formals) argcount))
	     (&rest (show-function-args
		     (list actuals) (rest formals) argcount))
	     (&key (handle-keyword-args actuals))
	     (&aux (show-function-args actuals nil argcount))
	     (t (handle-one-arg (first actuals) (first formals))
		(show-function-args (rest actuals)
				    (rest formals)
				    (1+ argcount)))))))

(defun handle-args-numerically (actuals argcount)
  (dolist (x actuals)
    (incf argcount)
    (display-arg-numeric x argcount)))

(defun handle-one-arg (val varspec)
  (cond ((atom varspec) (display-one-arg val varspec))
	(t (display-one-arg val (first varspec))
	   (if (third varspec)
	       (display-one-arg t (third varspec))))))

(defun handle-keyword-args (actuals)
  (cond ((null actuals))
	((keywordp (first actuals))
	 (display-one-arg (second actuals) (first actuals))
	 (handle-keyword-args (rest (rest actuals))))
	(t (display-one-arg actuals "Extra args:"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DUNTRACE and subordinate routines.

(defmacro duntrace (&rest function-names)
  "Turns off tracing for specified functions.  
   With no args, turns off all tracing."
  (setf *trace-level* 0)  ;; safety precaution
  (list 'quote
	(mapcan #'duntrace1 (or function-names *traced-functions*))))

(defun duntrace1 (name)
  (unless (symbolp name)
    (format *error-output* "~&~S is an invalid function name." name)
    (return-from duntrace1 nil))
  (setf *traced-functions* (delete name *traced-functions*))
  (let ((orig-defn (get name 'original-definition 'none))
	(traced-defn (get name 'traced-definition))
	(traced-type (get name 'traced-type 'none)))
    (unless (or (eq orig-defn 'none)
		(not (fboundp name))
		(not (equal traced-defn  ;; did it get redefined?
			 (ecase traced-type
			   (defun (symbol-function name))
			   (defmacro (macro-function name))))))
      (ecase traced-type
	(defun (setf (symbol-function name) orig-defn))
	(defmacro (setf (macro-function name) orig-defn)))))
  (remprop name 'traced-definition)
  (remprop name 'traced-type)
  (remprop name 'original-definition)
  (list name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display routines.
;;;
;;; The code below generates vanilla character output for ordinary 
;;; displays.  It can be replaced with special graphics code if the
;;; implementation permits, e.g., on a PC you can use the IBM graphic
;;; character set to draw nicer-looking arrows.  On a color PC you 
;;; can use different colors for arrows, for function names, for 
;;; argument values, and so on.

(defparameter *entry-arrow-string* "----")
(defparameter *vertical-string*    "|   ")
(defparameter *exit-arrow-string*  " \\--")

(defparameter *trace-wraparound* 15)

(defun display-function-entry (name &optional ftype)
  (space-over)
  (draw-entry-arrow)
  (format *trace-output* "Enter ~S" name)
  (if (eq ftype 'macro)
      (format *trace-output* " macro")))

(defun display-one-arg (val name)
  (space-over)
  (format *trace-output*
	  (typecase name
	    (keyword "  ~S ~S")
	    (string  "  ~A ~S")
	    (t "  ~S = ~S"))
	  name val))

(defun display-arg-numeric (val num)
  (space-over)
  (format *trace-output* "  Arg-~D = ~S" num val))

(defun display-function-return (name results &optional ftype)
  (with-dtrace-printer-settings
    (space-over)
    (draw-exit-arrow)
    (format *trace-output* "~S ~A"
	    name
	    (if (eq ftype 'macro) "expanded to" "returned"))
    (cond ((null results))
	  ((null (rest results)) (format *trace-output* " ~S" (first results)))
	  (t (format *trace-output* " values ~{~S, ~}~s"
		     (butlast results)
		     (car (last results)))))))

(defun space-over ()
  (format *trace-output* "~&")
  (dotimes (i (mod *trace-level* *trace-wraparound*))
    (format *trace-output* "~A" *vertical-string*)))

(defun draw-entry-arrow ()
  (format *trace-output* "~A" *entry-arrow-string*))

(defun draw-exit-arrow ()
  (format *trace-output* "~A" *exit-arrow-string*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The function FETCH-ARGLIST is implementation dependent.  It 
;;; returns the formal argument list of a function as it would 
;;; appear in a DEFUN or lambda expression, including any lambda
;;; list keywords.  Here are versions of FETCH-ARGLIST for three
;;; Lisp implementations.

;;; Minimal generic version
#-(or lucid allegro gclisp kcl cmu)
  (defun fetch-arglist (fn)
    (declare (ignore fn))
    nil)

;;; Lucid version
#+lucid
  (defun fetch-arglist (fn)
    (system::arglist fn))


#+allegro
  (defun fetch-arglist (fn)
    (excl::arglist fn))

;;; GCLisp 1.1 version
#+gclisp
  (defun fetch-arglist (fn)
    (if (macro-function fn)
	'(&rest "Form =")
	(lambda-list fn)))

;;; KCL version
#+kcl
(defun fetch-arglist (fn)
  (let ((x (symbol-function fn)))
    (cond ((atom x) nil)
	  ((eq (first x) 'macro) (list '&rest "Form ="))
	  (t (third x)))))

;;; CMU Common Lisp version.  This version looks in a symbol's
;;; function cell and knows how to take apart lexical closures
;;; and compiled code objects found there.
#+cmu
  (defun fetch-arglist (x &optional original-x)
    (cond ((symbolp x) (fetch-arglist (symbol-function x) x))
	  ((compiled-function-p x)
	   (read-from-string
	    (lisp::%primitive header-ref x
			      lisp::%function-arg-names-slot)))
	  ((listp x) (case (first x)
		       (lambda (second x))
		       (lisp::%lexical-closure% (fetch-arglist (second x)))
		       (system:macro '(&rest "Form ="))
		       (t '(&rest "Arglist:"))))
	  (t (cerror (format nil
                        "Use a reasonable default argument list for ~S"
		        original-x)
		"Unkown object in function cell of ~S:  ~S" original-x x)
	     '())))



;;; -*- Mode: Lisp -*-
;;;
;;; SDRAW - draws cons cell structures.
;;;
;;; From the book "Common Lisp:  A Gentle Introduction to
;;;      Symbolic Computation" by David S. Touretzky.  
;;; The Benjamin/Cummings Publishing Co., 1990.
;;;
;;; This is the generic version; it will work in any legal Common Lisp.
;;; Revised to include support for circular structures.
;;; Revised again, August, 2003, to work with ANSI Common Lisp and Allegro v6.
;;;
;;; User-level routines:
;;;   (sdraw obj)  - draws obj on the display
;;;   (sdraw-loop) - puts the user in a read-eval-draw loop
;;;   (scrawl obj) - interactively crawl around obj
;;;
;;; Variables:
;;;   *sdraw-print-circle*    If bound, overrides *print-circle*.
;;;   *sdraw-leading-arrow*   Initially nil.  Set to t to get leading arrows.
;;;

;; (defpackage :sdraw
;;   (:use :common-lisp)
;;   (:export sdraw sdraw-loop scrawl *sdraw-print-circle* *sdraw-leading-arrow*))

;; (in-package :sdraw)

;; (export '(sdraw::sdraw sdraw::sdraw-loop sdraw::scrawl
;; 	  sdraw::*sdraw-print-circle* sdraw::*sdraw-leading-arrow*))

;; (shadowing-import  '(sdraw::sdraw sdraw::sdraw-loop sdraw::scrawl
;; 		     sdraw::*sdraw-print-circle*
;; 		     sdraw::*sdraw-leading-arrow*)
;; 		   (find-package :common-lisp-user))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The parameters below are in units of characters (horizontal)
;;; and lines (vertical).  They apply to all versions of SDRAW,
;;; but their values may change if cons cells are being drawn as
;;; bit maps rather than as character sequences.

(defparameter *sdraw-display-width* 79.)
(defparameter *sdraw-horizontal-atom-cutoff* 79.)
(defparameter *sdraw-horizontal-cons-cutoff* 65.)

(defparameter *etc-string* "etc.")
(defparameter *etc-spacing* 4.)

(defparameter *inter-atom-h-spacing* 3.)
(defparameter *cons-atom-h-arrow-length* 9.)
(defparameter *inter-cons-v-arrow-length* 3.)
(defparameter *cons-v-arrow-offset-threshold* 2.)
(defparameter *cons-v-arrow-offset-value* 1.)
(defparameter *leading-arrow-length* 4)

(defparameter *sdraw-num-lines* 25)
(defparameter *sdraw-vertical-cutoff* 22.)

(defvar *sdraw-leading-arrow* nil)
(defvar *sdraw-print-circle*)
(defvar *sdraw-circular-switch*)
(defvar *circ-detected* nil)
(defvar *circ-label-counter* 0)
(defparameter *circ-hash-table* (make-hash-table :test #'eq :size 20))

(defvar *line-endings* (make-array *sdraw-num-lines*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SDRAW and subordinate definitions.

(defun sdraw (obj &aux (*circ-detected* nil))
  (let ((*sdraw-circular-switch*
	 (if (boundp '*sdraw-print-circle*) *sdraw-print-circle*
	     *print-circle*))
	(start-col (if *sdraw-leading-arrow* *leading-arrow-length* 0)))
    (init-struct1 start-col)
    (clrhash *circ-hash-table*)
    (let* ((first-layout (struct1 obj 0 start-col 0 nil))
	   (second-layout (when *circ-detected*
			    (init-struct1 start-col)
			    (struct1 obj 0 start-col 0 t))))
      (draw-structure (or second-layout first-layout))
      (values))))



(defun init-struct1 (start-col)
  (setf *circ-label-counter* 0)
  (fill *line-endings* most-negative-fixnum)
  (struct-record-position 0 (- start-col *inter-atom-h-spacing*)))

(defun never-seen? (obj)
  (null (gethash obj *circ-hash-table*)))

(defun seen-twice? (obj)
  (numberp (gethash obj *circ-hash-table*)))

(defun needs-label? (obj)
  (zerop (gethash obj *circ-hash-table*)))



(defun struct1 (obj row root-col adj second-pass)
  (cond ((>= row *sdraw-vertical-cutoff*) (struct-process-etc row root-col adj))
	((not second-pass)
	 (enter-in-hash-table obj)
	 (struct-first-pass obj row root-col adj))
	(t (struct-second-pass obj row root-col adj))))

(defun enter-in-hash-table (obj)
  (unless (or (not *sdraw-circular-switch*)
	      (numberp obj)
	      (and (symbolp obj) (symbol-package obj)))
    (cond ((never-seen? obj) (setf (gethash obj *circ-hash-table*) t))
	  (t (setf (gethash obj *circ-hash-table*) 0)
	     (setf *circ-detected* t)))))

(defun struct-first-pass (obj row root-col adj)
  (if (seen-twice? obj)
      (struct-process-circ-reference obj row root-col adj)
      (if (atom obj)
	  (struct-unlabeled-atom (format nil "~S" obj) row root-col adj)
	  (struct-unlabeled-cons obj row root-col adj nil))))

(defun struct-second-pass (obj row root-col adj)
  (cond ((not (seen-twice? obj))
	 (if (atom obj)
	     (struct-unlabeled-atom (format nil "~S" obj) row root-col adj)
	     (struct-unlabeled-cons obj row root-col adj t)))
	((needs-label? obj)
	 (if (atom obj)
	     (struct-label-atom obj row root-col adj)
	     (struct-label-cons obj row root-col adj)))
	(t (struct-process-circ-reference obj row root-col adj))))


;;; Handle the simplest case:  an atom or cons with no #n= label.

(defun struct-unlabeled-atom (atom-string row root-col adj)
  (let* ((start-col (struct-find-start row root-col adj))
	 (end-col (+ start-col adj (length atom-string))))
    (cond ((< end-col *sdraw-horizontal-atom-cutoff*)
	   (struct-record-position row end-col)
	   (list 'atom row (+ start-col adj) atom-string))
	  (t (struct-process-etc row root-col adj)))))

(defun struct-unlabeled-cons (obj row root-col adj second-pass)
  (let* ((cons-start (struct-find-start row root-col adj))
	 (car-structure
	  (struct1 (car obj)
		   (+ row *inter-cons-v-arrow-length*)
		   cons-start adj second-pass))
	 (start-col (third car-structure)))
    (if (>= start-col *sdraw-horizontal-cons-cutoff*)
	(struct-process-etc row root-col adj)
	(progn
	  (struct-record-position row (- (+ start-col
					    *cons-atom-h-arrow-length*)
					 adj *inter-atom-h-spacing*))
	  (list 'cons row start-col car-structure
		(struct1 (cdr obj) row (+ start-col *cons-atom-h-arrow-length*)
			 0 second-pass))))))

(defun struct-process-etc (row root-col adj)
  (let ((start-col (struct-find-start row root-col adj)))
    (struct-record-position
      row
      (+ start-col adj (length *etc-string*) *etc-spacing*))
    (list 'msg row (+ start-col adj) *etc-string*)))




;;; Handle objects that need to be labeled with #n=.
;;; Called only on the second pass.

(defun struct-label-atom (obj row root-col adj)
  (assign-label obj)
  (let* ((circ-string (format nil "#~S=" (gethash obj *circ-hash-table*)))
	 (newadj (struct-find-adj row root-col adj (length circ-string)))
	 (atom-string (format nil "~S" obj))
	 (start-col (struct-find-start row root-col adj))
	 (end-col (+ start-col newadj (length atom-string))))
    (cond ((< end-col *sdraw-horizontal-atom-cutoff*)
	   (struct-record-position row end-col)
	   (list 'atom row (+ start-col newadj) atom-string circ-string))
	  (t (struct-process-etc row root-col adj)))))

(defun struct-label-cons (obj row root-col adj)
  (assign-label obj)
  (let* ((string (format nil "#~S=" *circ-label-counter*))
	 (newadj (struct-find-adj row root-col adj (length string)))
	 (cons-start (struct-find-start row root-col adj))
	 (car-structure
	  (struct1 (car obj)
		   (+ row *inter-cons-v-arrow-length*)
		   cons-start newadj t))
	 (start-col (third car-structure)))
    (if (>= start-col *sdraw-horizontal-cons-cutoff*)
	(struct-process-etc row root-col adj)
	(progn
	  (struct-record-position row (- (+ start-col
					    *cons-atom-h-arrow-length*)
					 adj *inter-atom-h-spacing*))
	  (list 'cons row start-col car-structure
		(struct1 (cdr obj) row
			 (+ start-col *cons-atom-h-arrow-length*) 0 t)
		string)))))

(defun assign-label (obj)
  (setf (gethash obj *circ-hash-table*)
	(incf *circ-label-counter*)))


;;; Handle circular references by displaying them as #n#.
;;; When called on the first pass, this function always uses a label of 0.
;;; It will get the label right on the second pass.

(defun struct-process-circ-reference (obj row root-col adj)
  (let ((start-col (struct-find-start row root-col adj))
	(string (format nil "#~S#" (gethash obj *circ-hash-table*))))
    (struct-record-position
      row
      (+ (+ start-col adj) (length string)))
    (list 'msg row (+ start-col adj) string)))



;;; Support functions.

(defun struct-find-start (row root-col adj)
  (max root-col
       (- (+ *inter-atom-h-spacing* (aref *line-endings* row)) adj)))

(defun struct-find-adj (row col adj size)
  (let* ((line-end (max 0 (+ *inter-atom-h-spacing*
			     (aref *line-endings* row))))
	 (newadj (- line-end (- col (max size adj)))))
    (max adj (min (max newadj 0) size))))

(defun struct-record-position (row end-col)
  (setf (aref *line-endings* row) end-col))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SDRAW-LOOP and subordinate definitions.

(defparameter *sdraw-loop-prompt-string* "S> ")

(defun sdraw-loop ()
  "Read-eval-print loop using sdraw to display results."
  (format t "~&Type any Lisp expression, or :ABORT to exit.~%~%")
  (sdl1))

(defun sdl1 ()
  (loop
    (format t "~&~A" *sdraw-loop-prompt-string*)
    (force-output t)
    (let ((form (read)))
      (setf +++ ++
            ++  +
            +   -
            -   form)
      (if (eq form :abort) (return-from sdl1))
      (let ((result (eval form)))
        (setf /// //
              //  /
              /   (list result)
              *** **
              **  *
              *   result)
        (display-sdl-result *)))))

(defun display-sdl-result (result)
  (sdraw result)
  (let* ((*print-circle* (if (boundp '*sdraw-print-circle*)
			     *sdraw-print-circle*
		             *print-circle*))
	 (*print-length* nil)
	 (*print-level* nil)
	 (*print-pretty* #+cmu t #-cmu nil)
	 (full-text (format nil "Result:  ~S" result))
	 (text (if (> (length full-text)
		      *sdraw-display-width*)
		   (concatenate 'string
		     (subseq full-text 0 (- *sdraw-display-width* 4))
		     "...)")
		   full-text)))
    (if (consp result)
        (format t "~%~A~%" text))
    (terpri)))

(defun display-sdl-error (error)
  (format t "~A~%~%" error))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SCRAWL and subordinate definitions.

(defparameter *scrawl-prompt-string* "SCRAWL> ")
(defvar *scrawl-object* nil)
(defvar *scrawl-current-obj*)
(defvar *extracting-sequence* nil)

(defun scrawl (obj)
  "Read-eval-print loop to travel through list"
  (format t "~&Crawl through list:  'H' for help, 'Q' to quit.~%~%")
  (setf *scrawl-object* obj)
  (scrawl-start-cmd)
  (scrawl1))

(defun scrawl1 ()
  (loop
    (format t "~&~A" *scrawl-prompt-string*)
    (force-output t)
    (let ((command (read-uppercase-char)))
      (case command
	(#\A (scrawl-car-cmd))
	(#\D (scrawl-cdr-cmd))
	(#\B (scrawl-back-up-cmd))
	(#\S (scrawl-start-cmd))
	(#\H (display-scrawl-help))
	(#\Q (return))
	(t (display-scrawl-error))))))

(defun scrawl-car-cmd ()
  (cond ((consp *scrawl-current-obj*)
	 (push 'car *extracting-sequence*)
	 (setf *scrawl-current-obj* (car *scrawl-current-obj*)))
	(t (format t
	     "~&Can't take CAR or CDR of an atom.  Use B to back up.~%")))
  (display-scrawl-result))

(defun scrawl-cdr-cmd ()
  (cond ((consp *scrawl-current-obj*)
	 (push 'cdr *extracting-sequence*)
	 (setf *scrawl-current-obj* (cdr *scrawl-current-obj*)))
	(t (format t
	     "~&Can't take CAR or CDR of an atom.  Use B to back up.~%")))
  (display-scrawl-result))

(defun scrawl-back-up-cmd ()
  (cond (*extracting-sequence*
	 (pop *extracting-sequence*)
	 (setf *scrawl-current-obj*
	       (extract-obj *extracting-sequence* *scrawl-object*)))
	(t (format t "~&Already at beginning of object.")))
  (display-scrawl-result))

(defun scrawl-start-cmd ()
  (setf *scrawl-current-obj* *scrawl-object*)
  (setf *extracting-sequence* nil)
  (display-scrawl-result))

(defun extract-obj (seq obj)
  (reduce #'funcall
	  seq
	  :initial-value obj
	  :from-end t))

(defun get-car/cdr-string ()
  (if (null *extracting-sequence*)
      (format nil "'~S" *scrawl-object*)
      (format nil "(c~Ar '~S)"
	      (map 'string #'(lambda (x)
			       (ecase x
				 (car #\a)
				 (cdr #\d)))
		   *extracting-sequence*)
	      *scrawl-object*)))

(defun display-scrawl-result (&aux (*print-length* nil)
				   (*print-level* nil)
				   (*print-pretty* #+cmu t #-cmu nil)
				   (*print-circle* t))
  (let* ((extract-string (get-car/cdr-string))
	 (text (if (> (length extract-string) *sdraw-display-width*)
		   (concatenate 'string
		    (subseq extract-string 0
			    (- *sdraw-display-width* 4))
		    "...)")
		   extract-string)))
    (sdraw *scrawl-current-obj*)
    (format t "~&~%~A~%~%" text)))

(defun display-scrawl-help ()
  (format t "~&Legal commands:  A)car   D)cdr  B)back up~%")
  (format t "~&                 S)start Q)quit H)help~%"))

(defun display-scrawl-error ()
  (format t "~&Illegal command.~%")
  (display-scrawl-help))

(defun read-uppercase-char ()
  (let ((response (read-line)))
    (and (plusp (length response))
	 (char-upcase (char response 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following definitions are specific to the tty implementation.

(defparameter *cons-string* "[*|*]")
(defparameter *cons-cell-flatsize* 5.)
(defparameter *cons-h-arrowshaft-char* #\-)
(defparameter *cons-h-arrowhead-char* #\>)
(defparameter *cons-v-line* "|")
(defparameter *cons-v-arrowhead* "v")

(defvar *textline-array* (make-array *sdraw-num-lines*))
(defvar *textline-lengths* (make-array *sdraw-num-lines*))

(eval-when (eval load)
  (dotimes (i *sdraw-num-lines*)
    (setf (aref *textline-array* i)
	  (make-string *sdraw-display-width*))))

(defun char-blt (row start-col string)
  (let ((spos (aref *textline-lengths* row))
	(line (aref *textline-array* row)))
    (do ((i spos (1+ i)))
	((>= i start-col))
      (setf (aref line i) #\Space))
    (replace line string :start1 start-col)
    (setf (aref *textline-lengths* row)
	  (+ start-col (length string)))))

(defun draw-structure (directions)
  (fill *textline-lengths* 0.)
  (when *sdraw-leading-arrow* (draw-leading-arrow))
  (follow-directions directions)
  (dump-display))

(defun draw-leading-arrow ()
  (do ((i 0 (1+ i)))
      ((>= (1+ i) *leading-arrow-length*)
       (char-blt 0 i (string *cons-h-arrowhead-char*)))
    (char-blt 0 i (string *cons-h-arrowshaft-char*))))

(defun follow-directions (dirs &optional is-car)
  (ecase (car dirs)
    (cons (draw-cons dirs))
    ((atom msg) (draw-msg dirs is-car))))

(defun draw-cons (obj)
  (let* ((row (second obj))
	 (col (third obj))
	 (car-component (fourth obj))
	 (cdr-component (fifth obj))
	 (string (sixth obj))
	 (line (aref *textline-array* row))
	 (h-arrow-start (+ col *cons-cell-flatsize*))
	 (h-arrowhead-col (1- (third cdr-component)))
	 (cdr-string? (if (eq 'cons (first cdr-component))
			  (sixth cdr-component)
			  (fifth cdr-component))))
    (if cdr-string? (decf h-arrowhead-col (length cdr-string?)))
    (char-blt row (- col (length string))
	      (if string (concatenate 'string string *cons-string*)
		  *cons-string*))
    (do ((i h-arrow-start (1+ i)))
	((>= i h-arrowhead-col))
      (setf (aref line i) *cons-h-arrowshaft-char*))
    (setf (aref line h-arrowhead-col) *cons-h-arrowhead-char*)
    (setf (aref *textline-lengths* row) (1+ h-arrowhead-col))
    (char-blt (+ row 1) (+ col 1) *cons-v-line*)
    (char-blt (+ row 2) (+ col 1) *cons-v-arrowhead*)
    (follow-directions car-component t)
    (follow-directions cdr-component)))

(defun draw-msg (obj is-car)
  (let* ((row (second obj))
	 (col (third obj))
	 (string (fourth obj))
	 (circ-string (fifth obj)))
    (if circ-string (setf string (concatenate 'string circ-string string)))
    (char-blt row
	      (+ (- col (length circ-string))
		 (if (and is-car
			  (<= (length string)
			      *cons-v-arrow-offset-threshold*))
		     *cons-v-arrow-offset-value*
		     0))
	      string)))

(defun dump-display ()
  (terpri)
  (dotimes (i *sdraw-num-lines*)
    (let ((len (aref *textline-lengths* i)))
      (if (plusp len)
	  (format t "~&~A"
		  (subseq (aref *textline-array* i) 0 len))
	  (return nil))))
  (terpri))




;;; -*- Mode: Lisp; Package: USER -*-
;;;
;;; PPMX - pretty prints a macro expansion
;;;
;;; From the book "Common Lisp:  A Gentle Introduction to
;;;      Symbolic Computation" by David S. Touretzky.  
;;; The Benjamin/Cummings Publishing Co., 1990.
;;;
;;; Example of use:  (ppmx (incf a))

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
     (cond ((equal exp exp1)
	    (format t "~&Macro expansion:")
	    (pprint exp))
	   (t (format t "~&First step of expansion:")
	      (pprint exp1)
	      (format t "~%~%Final expansion:")
	      (pprint exp)))
     (format t "~%~%")
     (values)))  



;;;
;;; END DTOOLS
;;;



;;;
;;;
;;;
;;; Tools
;;;
;;;
;;;



;;;
;;; Inspect the fields of any symbol.
;;;

(defun show-symbol (symbol)
  "Takes a symbol and prints its various fields out."
  (format t "~S ~S ~S ~S ~S" (symbol-name symbol)
	     	             (symbol-package symbol)
	                     (symbol-value symbol)
	                     (fdefinition symbol)
	                     (symbol-plist symbol)))



;;;
;;; show-macexp
;;;

(defmacro show-macexp (mac)
  "Takes a Lisp form and pretty-prints its macro-expansion."
  `(pprint (macroexpand-1 ',mac)))



;;;
;;; List all the symbols in a package.
;;;

(defun list-all-symbols-in-package (&optional package)
  (let ((smbls ())
	(pckg (find-package package)))
    (do-all-symbols (s smbls)
      (when (fboundp s)
	(if pckg
	    (when (eql (symbol-package s) pckg)
	      (push s smbls))
	    (push s smbls))))
    smbls))


;;;
;;; Unbind all symbols in a package.
;;;

(defun makunbound-all (&optional (package *package*)) 
  "Make unbound all the symbols whose home package is PACKAGE." 
  (setq package (find-package package)) 
  (do-symbols (symbol package) 
    (when (eq (symbol-package symbol) package) 
      (makunbound symbol))))









;;;
;;;
;;;
;;;
;;; CHAPTER 3  EVAL NOTATION
;;;
;;;
;;;
;;;


;;; defun is a macro-function which does not evaluate its arguments.

;;; Every function defined with DEFUN comes with its own lexical context.

(defun avrg (x y)
  (/ (+ x y) 2.0))

(defun square (n)
  (* n n))

(defun total-cost (quantity
		   price
		   handling-charge)
    (+ (* quantity price) handling-charge))
 
(list 'james 't 'kirk)

(defun riddle (x y)
  (list 'why 'is 'a x 'like 'a y))

(riddle 'six 'seven)

(format nil "Result: ~A" (+ 1 2)) 


;;; Building a list up from individual elements

(list 'foo 'bar 'baz)
(cons 'foo '(bar baz))

;;; Some elements in a list can be computed instead of specifying them directly
(list 33 'squared 'is (* 33 33))
;(33 SQUARED IS 1089)





;;;
;;;
;;; SECTION 3.11  FOUR WAYS TO MISDEFINE A FUNCTION 
;;;
;;;

(defun intro1 (x y) (list x 'this 'is y))

#|

CL-USER> (intro1 'stanley 'livingstone)
(STANLEY THIS IS LIVINGSTONE)

|#



;;;
;;; EXERCISE 3.15
;;;

(defun scrabble (word) (list word 'is 'a 'word))

#|

CL-USER> (scrabble 'word)
(WORD IS A WORD)

CL-USER> (scrabble word)
; Evaluation aborted on #<UNBOUND-VARIABLE WORD {1003D22143}>.

|#


(defun stooge (larry moe curly)
  (list larry moe (list 'moe curly) curly 'larry))

#|

CL-USER> (stooge 'moe 'curly 'larry)
(MOE CURLY (MOE LARRY) LARRY LARRY)

|#



;;;
;;; EXERCISE 3.16
;;;
(defun stooge-2 (larry moe curly)
  (list larry (list 'moe curly) curly 'larry))

#|

CL-USER> (stooge-2 'moe 'curly 'larry)
(MOE (MOE LARRY) LARRY LARRY)

|#



;;;
;;; EXERCISE 3.17
;;;

#|

Answer: 
T and NIL cannot be used as regular variables because T and NIL evaluate to 
themselves, whereas a regular variable needs to evaluate to whatever value it 
holds.

|#



;;;
;;; EXERCISE 3.18
;;;

#|

Answer:
1) Both functions and data can be expressed in EVAL notation.
2) EVAL notation is concise and easy to type on a keyboard.

|#




;;;
;;; EXERCISE 3.19
;;;

#|

CL-USER> (cons 'grapes '(of wrath))
(GRAPES OF WRATH)
CL-USER> (list t 'is 'not NIL)
(T IS NOT NIL)
CL-USER> (first '(list moose goose))
LIST
CL-USER> (first (list 'moose 'goose))
MOOSE
CL-USER> (cons 'home ('sweet 'home))
; in: CONS 'HOME
;     ('SWEET 'HOME)
; 
; caught ERROR:
;   illegal function call
; 
; compilation unit finished
;   caught 1 ERROR condition
; Evaluation aborted on #<SB-INT:COMPILED-PROGRAM-ERROR {1003EA2133}>.

|#




;;;
;;; EXERCISE 3.20
;;;

(defun mystery (x)
  (list (second x) (first x)))

#|

CL-USER> (mystery '(dancing bear))
(BEAR DANCING)
CL-USER> (mystery (list 'first 'second))
(SECOND FIRST)

|#



;;;
;;; EXERCISE 3.21.
;;;

#| What is wrong with each of the following function definitions?

(defun speak (x y) (list ’all ’x ’is ’y))
Answer: The symbols 'x and 'y instead of the variables x and y are used in the
        body of the function.

(defun speak (x) (y) (list ’all x ’is y))
Answer: There is a separate argument list for each argument instead of a common
        one for all arguments.

(defun speak ((x) (y)) (list all ’x is ’y))
Answer: The arguments are presented as functions (x) and (y) instead of as
        variables.
|#




;;;
;;;
;;; SECTION 3.16  FUNCTIONS OF NO ARGUMENTS
;;;
;;;

(defun test nil (* 85 97))

#|

CL-USER> (test)
8245

|#





;;;
;;;
;;; SECTION 3.17  THE QUOTE SPECIAL FUNCTION
;;;
;;;

;;; quote is a special function: the inputs to quote do not get evaluated.

#|

CL-USER> (list 'quote 'foo)
'FOO
CL-USER> (list ''foo)
('FOO)
CL-USER> (first ''foo)
QUOTE
CL-USER> (rest ''foo)
(FOO)

|#





;;;
;;;
;;; SECTION 3.18  INTERNAL STRUCTURE OF SYMBOLS
;;;
;;;

#|

CL-USER> (symbol-name 'equal)
"EQUAL"
CL-USER> (symbol-function 'equal)
#<FUNCTION EQUAL>

|#





;;;
;;;
;;; SECTION 3.19  LAMBDA NOTATION
;;;
;;;

;;; Unlike defun, lambda is not a function; it is a marker treated specially by
;;; EVAL.





;;;
;;;
;;; SECTION 3.20  SCOPE OF VARIABLES
;;;
;;;

;;; The scope of a variable is the region in which it may be referenced.
;;; A variable whose scope is limited to the body of the function in which the
;;; variable holds the function's input is called a 'local' variable. A variable
;;; whose scope is unbouded is called a 'global' variable. A global variable can
;;; be referenced from anywhere.

#|

(defun parent (n)
  child (+ n 2))

(defun child (p)
  ;; n is interpreted as a reference to a global variable which is unassigned.
  (list n p))  

(parent 3)
;ERROR: Variable CHILD is unbound.

|#






;;;
;;;
;;; SECTION 3.21  EVAL AND APPLY
;;;
;;;

;;; EVAL is a Lisp primitive function. Each use of EVAL give one level of
;;; evaluation.

#|

CL-USER> '(+ 2 2)
(+ 2 2)
CL-USER> (eval '(+ 2 2))
4
CL-USER> '''boing
''BOING
CL-USER> (eval (eval (eval '''boing)))
; Evaluation aborted on #<UNBOUND-VARIABLE BOING {10050B0393}>.
CL-USER> (eval '(list '* 9 6))
(* 9 6)
CL-USER> (eval (eval '(list '* 9 6)))
54

|#


;;; We can think of the computer as a physical manifestation of EVAL. When the
;;; computer runs, everything we type is evaluated.


;;; APPLY

;;; APPLY is also a Lisp primitive function. APPLY takes a function and a list
;;; of objects as inputs. It applies that function to the list of objects as
;;; inputs.

;;; The objects that APPLY applies the function to are not evaluated first.

#|

CL-USER> (apply #'+ '(2 3))
5
CL-USER> (apply #'equal '(12 13))
NIL
CL-USER> (apply #'cons '(as (you like it)))
(AS YOU LIKE IT)

|#



;;;
;;; EXERCISE 3.25
;;;

#|

CL-USER> (list 'cons t nil)
(CONS T NIL)
CL-USER> (eval (list 'cons t nil))
(T)
CL-USER> (apply #'cons '(t nil))
(T)
CL-USER> (eval (eval (list 'cons t nil)))
; in: T
;     (T)
; 
; caught WARNING:
;   The function T is undefined, and its name is reserved by ANSI CL so that even
;   if it were defined later, the code doing so would not be portable.
; 
; compilation unit finished
;   Undefined function:
;     T
;   caught 1 WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION T {1004233393}>.
CL-USER> (eval (list 'eval nil))
NIL

|#



;;;
;;;
;;;
;;; CHAPTER 4  CONDITIONALS
;;;
;;;
;;;



;;;
;;;
;;; SECTION 4.2  THE IF SPECIAL FUNCTION
;;;
;;;

;;; Conditionals are macros or special functions, so that their arguments
;;; do not get evaluated automatically. IF is the simplest Lisp conditional.

(defun my-abs (x)
  (if (< x 0) (- x) x))

(defun symbol-test (x)
  (if (symbolp x)
      (list 'Yes x 'is 'a 'symbol)
      (list 'No x 'is 'not 'a 'symbol)))

#|

CL-USER> (symbol-test 'rutabaga)
(YES RUTABAGA IS A SYMBOL)
CL-USER> (symbol-test 1)
(NO 1 IS NOT A SYMBOL)
CL-USER> (if t 'happy)
HAPPY
CL-USER> (if nil 'happy)
NIL

|#





;;;
;;;
;;; SECTION 4.3  THE COND MACRO
;;;
;;;

(defun compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
        ((< x y) 'first-number-is-smaller)
	((> x y) 'first-number-is-bigger)))

#|

CL-USER> (compare 3 5)
FIRST-NUMBER-IS-SMALLER
CL-USER> (compare 3 3)
NUMBERS-ARE-THE-SAME

|#





;;;
;;;
;;; SECTION 4.4  USING T AS A TEST
;;;
;;;

(defun where-is (x)
  (cond ((equal x 'Paris) 'France)
	((equal x 'London) 'England)
	((equal x 'Beijing) 'China)
	(t 'unknown)))

#|

CL-USER> (where-is 'Mumbai)
UNKNOWN

|#





;;;
;;;
;;; SECTION 4.5  TWO MORE EXAMPLES OF COND
;;;
;;;

(defun emphasize (x)
  (cond ((equal (first x) 'Good) (cons 'Great (rest x)))
	((equal (first x) 'Bad) (cons 'Awful (rest x)))))

#|

CL-USER> (emphasize '(Good mystery story))
(GREAT MYSTERY STORY)
CL-USER> (emphasize '(OK mystery story))
NIL

|#


(defun emphasize2 (x)
  (cond ((equal (first x) 'Good) (cons 'Great (rest x)))
	((equal (first x) 'Bad) (cons 'Awful (rest x)))
	(t x)))

#|

CL-USER> (emphasize2 '(OK mystery story))
(OK MYSTERY STORY)

|#


(defun compute (op x y)
  (cond ((equal op 'sum-of) (+ x y))
	((equal op 'product-of) (* x y))
	(t '(That does not compute))))

#|

CL-USER> (compute 'sum-of 3 7)
10
CL-USER> (compute 'diff-of 3 7)
(THAT DOES NOT COMPUTE)

|#



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
;;;
;;; SECTION 4.7  THE AND AND OR MACROS
;;;
;;;

;;; Like COND, AND and OR are macros. They can take any number of clauses, and
;;; they don't evaluate their arguments first.





;;;
;;;
;;; SECTION 4.8  EVALUATING AND AND OR
;;;
;;;

;;; Rule for evaluating AND: Evaluate each clause in turn and return either
;;; nil or the value of the last clause (if no clause evaluates to nil).

;;; Rule for evaluating OR: Evaluate each clause in turn and return either the
;;; value of the first non-nil, or nil (if no clause evaluates to non-nil).

#|

CL-USER> (and 'george nil 'harry)
NIL
CL-USER> (and 'george 'jack 'harry)
HARRY
CL-USER> (and 1 2 3 4 5)
5
CL-USER> (or 'george nil 'harry)
GEORGE
CL-USER> (or 'george 'jack 'harry)
GEORGE
CL-USER> (or 1 2 3 4 5)
1

|#





;;;
;;;
;;; SECTION 4.9  BUILDING COMPLEX PREDICATES
;;;
;;;

(defun how-alike (a b)
  (cond ((equal a b) 'the-same)
	((and (oddp a) (oddp b)) 'both-odd)
	((and (not (oddp a)) (not (oddp b))) 'both-even)
	((and (< a 0) (< b 0)) 'both-negative)
	(t 'not-alike)))
;HOW-ALIKE

#|

CL-USER> (how-alike 7 7)
THE-SAME
CL-USER> (how-alike 3 5)
BOTH-ODD
CL-USER> (how-alike 5 8)
NOT-ALIKE

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
;;; SECTION 4.10  WHY AND AND OR ARE CONDITIONALS
;;;
;;;

;;; AND and OR are conditionals, and not regular functions, because unlike
;;; regular functions AND and OR are not required to evaluate every clause.





;;;
;;;
;;; SECTION 4.11  CONDITIONALS ARE INTERCHANGEABLE
;;;
;;;

;;; Functions that use AND and OR can also be implemented using COND or IF,
;;; and vice-versa.

(defun posnum-and (x)
  (and (numberp x) (plusp x)))

(defun posnum-if (x)
  (if (numberp x) (plusp x) nil))

(defun posnum-cond (x)
  (cond ((numberp x) (plusp x))
	(t nil)))


;;; Implementing where-is using AND and OR instead of COND.
(defun where-is-or-and (x)
  (or (and (equal x 'Paris) 'France)
      (and (equal x 'London) 'England)
      (and (equal x 'Beijing) 'China)
      'Unknown))

#|

CL-USER> (where-is-or-and 'Paris)
FRANCE
CL-USER> (where-is-or-and 'Wai)
UNKNOWN

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
;;;
;;; SECTION 4.12  BOOLEAN FUNCTIONS
;;;
;;;

;;; Boolean functions are simpler than conditionals and are the primitive
;;; logical operations from which computer circuitry is build. Boolean
;;; functions in Lisp correspond to boolean circuits in electronics.

(defun logical-and (x y)
  (and x y t))

#|

Differences between LOGICAL-AND and AND:
                  AND                               LOGICAL-AND     
1) Macro                               1) Function
2) Can take any number of arguments.   2) Must take exactly two arguments.
3) Can control whether it evaluates    3) Needs to evaluate all of its  
   all its arguments.                     arguments.

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
;;; EXERCISE  4.32
;;;

;;; SKIP



;;;
;;; EXERCISE  4.33 
;;;

;;; SKIP



;;;
;;; EXERCISE  4.34
;;;

;;; SKIP





;;;
;;;
;;; SECTION 4.14  DEMORGAN'S THEOREM
;;;
;;;

;;; (and x y)  =  (not (or (not x) (not y)))

;;; (or x y)   =  (not (and (not x) (not y)))

(defun demorgan-and (x y)
  (not (or (not x) (not y))))


(defun demorgan-or (x y)
  (not (and (not x) (not y))))

#|

CL-USER> (demorgan-and T T)
T
CL-USER> (demorgan-and NIL T)
NIL
CL-USER> (demorgan-or t NIL)
T
CL-USER> (demorgan-or NIL NIL)
NIL

|#


(defun complicated-predicate (x y)
  (not (and (evenp x) (evenp y))))


(defun simplified-predicate (x y)
  (or (oddp x) (oddp y)))

#|

CL-USER> (complicated-predicate 3 4)
T
CL-USER> (complicated-predicate 2 4)
NIL
CL-USER> (simplified-predicate 3 4)
T
CL-USER> (simplified-predicate 2 4)
NIL

|#



;;;
;;; EXERCISE 4.35
;;;

;;; SKIP



;;;
;;; EXERCISE 4.36
;;;

;;; SKIP



(defun nand (x y)
  (not (and x y)))
;NAND

(defun not-using-nand (x)
  (nand x x))
;NOT-USING-NAND

#|

CL-USER> (not-using-nand NIL)
T
CL-USER> (not-using-nand T)
NIL

|#


(defun nor-using-nand (x y)
  "Takes two logical inputs (x) and (y), and implements the NOR function using
   only the NAND function.

   (nor (not (or x y)))
   (nor (not (nand (nand x x) (nand y y))))
   (nor (nand (nand (nand x x) (nand y y)) (nand (nand x x) (nand y y))))"
  
   (nand (nand (nand x x) (nand y y)) (nand (nand x x) (nand y y))))

#|

CL-USER> (nor-using-nand NIL NIL)
T
CL-USER> (nor-using-nand NIL T)
NIL
CL-USER> (nor-using-nand T NIL)
NIL
CL-USER> (nor-using-nand T T)
NIL

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



;;;
;;;
;;;
;;; CHAPTER 5  VARIABLES AND SIDE EFFECTS
;;;
;;;
;;;





;;;
;;;
;;; SECTION 5.4  SIDE EFFECTS
;;;
;;;

;;; 'form' is another word for 'expression'.






;;;
;;;
;;; SECTION 5.5  THE LET SPECIAL FUNCTION
;;;
;;;


;;; LET creates all the local variables at once.

(let ((p 2)) (defvar p (+ p 5)))
;7


(defun avrg-2 (x y)
  (let ((sum (+ x y)))
    (list 'Average 'of x 'and y 'is (/ sum 2))))

(avrg-2 3 7)
;(AVERAGE OF 3 AND 7 IS 5)


(let ((p 2))
  (let ((p (+ p 5)))
    (list 'Inner 'p 'is p))
  (list 'Outer 'p 'is p))


;;; Creating more than 1 variable at once.

(defun switch-billing (x)
  (let ((star (first x))
	(co-star (third x)))
    (list co-star 'accompanied 'by star)))

(switch-billing '(fred and ginger))
;(GINGER ACCOMPANIED BY FRED)

(let ((x '(fred and ginger)))
  (let ((star (first x))
	(co-star (third x)))
    (list co-star 'accompanied 'by star)))

;(GINGER ACCOMPANIED BY FRED)



;;; The value forms in a let form are first evaluated before any local
;;; variables are created.





;;;
;;;
;;; SECTION 5.6  THE LET* SPECIAL FUNCTION 
;;;
;;;

;;; LET* creates all local variables one at a time. Therefore each variable
;;; is evaluated within the lexical context of the previous variable.

(defun price-change (old new)
  (let* ((diff (- new old))
	 (proportion (/ diff old))
	 (percentage (* proportion 100.0)))
    (list 'Price 'of 'widgets 'changed 'by 'percentage percentage)))

(price-change 1.25 1.35)

;(PRICE OF WIDGETS CHANGED BY PERCENTAGE 8.000002)





;;;
;;;
;;; SECTION 5.7  SIDE EFFECTS CAN CAUSE BUGS
;;;
;;;

(defun coin-toss-with-bug ()
  "A buggy coin tossing algorithm."
  (let ((rndm (random 101)))
    (cond ((< rndm 50) 'heads)
	  ((> rndm 50) 'tails)
	  (t 'edge))))

(coin-toss-with-bug)
;TAILS
;HEADS
;TAILS





;;;
;;;
;;; SECTION 5.8  SYMBOLS AND VALUE CELLS
;;;
;;;

(let ((list (list 1 2 3 4)))
  (list list (list 111 222 333 444) list (list 'list)))





;;;
;;;
;;; SECTION 5.9  DISTINGUISHING LOCAL FROM GLOBAL VARIABLES
;;;
;;;

;;; Symbols are not variables. Symbols serve as names for variables (and for
;;; functions too). Which variable a symbol  depends on the
;;; context in which the symbol appears.

;;; Only variables can be bound to values, never symbols.

(defvar var-holding-symbol 'HELLO)

#|

CL-USER> (inspect var-holding-symbol)

The object is a SYMBOL.
0. Name: "HELLO"
1. Package: #<PACKAGE "COMMON-LISP-USER">
2. Value: "unbound"
3. Function: "unbound"
4. Plist: NIL
> q

; No value
CL-USER> (inspect 'var-holding-symbol)

The object is a SYMBOL.
0. Name: "VAR-HOLDING-SYMBOL"
1. Package: #<PACKAGE "COMMON-LISP-USER">
2. Value: HELLO
3. Function: "unbound"
4. Plist: NIL
> q

; No value

|#

(defvar var-holding-atom 57)

#|

CL-USER> (inspect var-holding-atom)

The object is an ATOM:
  57
> q

; No value
CL-USER> (inspect 'var-holding-atom)

The object is a SYMBOL.
0. Name: "VAR-HOLDING-ATOM"
1. Package: #<PACKAGE "COMMON-LISP-USER">
2. Value: 57
3. Function: "unbound"
4. Plist: NIL
> q

; No value

|#


(defvar var-holding-character #\A)

#|

CL-USER> (inspect var-holding-character)

The object is an ATOM:
  #\A
> q

; No value
CL-USER> (inspect 'var-holding-character)

The object is a SYMBOL.
0. Name: "VAR-HOLDING-CHARACTER"
1. Package: #<PACKAGE "COMMON-LISP-USER">
2. Value: #\A
3. Function: "unbound"
4. Plist: NIL
> q

; No value

|#


(defvar var-holding-string "HELLO!")

#|

CL-USER> (inspect var-holding-string)

The object is a VECTOR of length 6.
0. #\H
1. #\E
2. #\L
3. #\L
4. #\O
5. #\!
> q

; No value
CL-USER> (inspect 'var-holding-string)

The object is a SYMBOL.
0. Name: "VAR-HOLDING-STRING"
1. Package: #<PACKAGE "COMMON-LISP-USER">
2. Value: "HELLO!"
3. Function: "unbound"
4. Plist: NIL
> q

; No value

|#


(defun newvar (x)
  (list 'value 'of 'x 'is x))

#|

CL-USER> (newvar 'whoopie)
(VALUE OF X IS WHOOPIE)
CL-USER> (inspect 'newvar)

The object is a SYMBOL.
0. Name: "NEWVAR"
1. Package: #<PACKAGE "COMMON-LISP-USER">
2. Value: "unbound"
3. Function: #<FUNCTION NEWVAR>
4. Plist: NIL
> q

; No value

|#



;;;
;;;
;;; SECTION 5.10  BINDING, SCOPING, AND ASSIGNMENT
;;;
;;;

;;; Binding: The process of creating a variable and giving it a value.

;;; Lambda binding: Binding when the variable appears in an argument list.

;;; LET binding: Binding when the variable appears in the variable list of a
;;; LET or LET* form.


;;; Binding in Lexical scoping vs binding in Dynamic scoping

;;; In lexical scoping a variable is said to be bound if it exists and has
;;; a value.

;;; In dynamic scoping a variable is said to be bound if it just exists, with
;;; or without having a value.

;;; Only variables can be bound, never symbols.





;;;
;;;
;;;
;;; CHAPTER 6  LIST DATA STRUCTURES
;;;
;;;
;;;

;;; A list is a one-way chain of pointers.




;;;
;;;
;;; SECTION 6.2  PARENTHESIS NOTATION VS CONS CELL NOTATION
;;;
;;;

;;; A list is a one-way chain of pointers.

#|

CL-USER> (cons 'w '(x y z))
(W X Y Z)
CL-USER> (cons '(x y z) 'w)
((X Y Z) . W)

|#





;;;
;;;
;;; SECTION 6.3  THE APPEND FUNCTION
;;;
;;;

#|

CL-USER> (append '(friends romans) '(and countrymen))
(FRIENDS ROMANS AND COUNTRYMEN)
CL-USER> (append '(april showers) nil)
(APRIL SHOWERS)
CL-USER> (append nil nil)
NIL
CL-USER> (append '((a 1) (b 2) (c 3)) '((d 4)))
((A 1) (B 2) (C 3) (D 4))

|#


;;; APPEND is called a non-destructive function as it does not change the
;;; value of any variable or modify any existing cons cell.

;;; APPEND makes a copy of the first input, but not of the second. It makes a
;;; copy of the first input, then makes the cdr of the last cell of the copy
;;; point to the second input, and finally returns a pointer to the copy.

;;; The first argument to APPEND must be a list, whereas subsequent arguments
;;; need not be.

#|

(append 'a '(b c d))
The value
  A
is not of type
  LIST

CL-USER> (append '(a b c) 'd)
(A B C . D)

|#


(defun add-to-end (x y)
  (append x (list y)))

#|

CL-USER> (add-to-end '(a  b c) 'd)
(A B C D)

|#





;;;
;;;
;;; SECTION 6.4  COMPARING CONS, LIST AND APPEND
;;;
;;;

;;; CONS     Creates one new cons cell.
;;;          Used to add an element to the front of a list.

;;; LIST     Makes a new list by accepting an
;;;          arbitrary number of inputs and
;;;          building a chain of cons cells
;;;          ending in NIL.
;;;          In the new list the CAR of each cell
;;;          points to the corresponding input.        

;;; APPEND   Concatenates lists together, by
;;;          creating a copy of the first list
;;;          an extending it with the second.
;;;          The first input must be a list.


;;; When the first input is an atom (i.e. symbol):

#|

CL-USER> (cons 'rice '(and beans))
(RICE AND BEANS)
CL-USER> (list 'rice '(and beans))
(RICE (AND BEANS))
CL-USER> (append 'rice '(and beans))
; Evaluation aborted on #<TYPE-ERROR expected-type: LIST datum: RICE>.

|#


;;; When the first input is a list:

#|

CL-USER> (cons '(here today) '(gone tomorrow))
((HERE TODAY) GONE TOMORROW)
CL-USER> (list '(here today) '(gone tomorrow))
((HERE TODAY) (GONE TOMORROW))
CL-USER> (append '(here today) '(gone tomorrow))
(HERE TODAY GONE TOMORROW)

|#


;;; When the first input is a list, and the second input is a symbol:

#|

CL-USER> (cons '(eat at) 'joes)
((EAT AT) . JOES)
CL-USER> (list '(eat at) 'joes)
((EAT AT) JOES)
CL-USER> (append '(eat at) 'joes)
(EAT AT . JOES)

|#




;;;
;;;
;;; SECTION 6.5  MORE FUNCTIONS ON LISTS
;;;
;;;

#|

CL-USER> (nthcdr 0 '(a b c d))
(A B C D)
CL-USER> (nthcdr 2 '(a b c d))
(C D)
CL-USER> (nthcdr 3 '(a b c d))
(D)

|#


;;; Going too far with NTHCDR will not cause an error if the list if the list
;;; is terminated by NIL.

#|

CL-USER> (nthcdr 4 '(a b c d))
NIL
CL-USER> (nthcdr 5 '(a b c d))
NIL

|#


;;; Going too far with NTHCDR will cause an error if the list is terminated by
;;; an atom other than NIL.

#|

CL-USER> (nthcdr 2 '(a b c . d))
(C . D)
CL-USER> (nthcdr 4 '(a b c . d))
; Evaluation aborted on #<TYPE-ERROR expected-type: LIST datum: D>.

|#



;;;
;;; SUBSECTION 6.5.3  LAST
;;;

;;; LAST returns the last cons-cell of a list.

#|

CL-USER> (last '(all is forgiven))
(FORGIVEN)
CL-USER> (last '())
NIL
CL-USER> (last '(a b c d))
(D)
CL-USER> (last '(a b c . d))
(C . D)

|#



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
;;; SUBSECTION 6.5.4  REMOVE
;;;

;;; REMOVE removes some or all occurances of an item from a list. It is a
;;; non-destructive function as it returns a new list sans the removed items.

(remove 'a '(b a n a n a))
;(B N N)



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
;;;
;;; SECTION 6.6  LISTS AS SETS
;;;
;;;

;;; A set is an unordered collection of items in which each item appears only
;;; once.



;;;
;;; SUBSECTION 6.6.1  MEMBER
;;;

;;; MEMBER checks whether the given item is present in a list. If the item is
;;; it returns a sublist beginning with the item, or NIL if not.

;;; Since MEMBER returns non-NIL if the item is present, it is counted as a
;;; predicate.

(defvar ducks '(huey dewey louie))

#| 

CL-USER> (member 'huey ducks)
(HUEY DEWEY LOUIE)
CL-USER> (member 'louie ducks)
(LOUIE)
CL-USER> (member 'mickey ducks)
NIL

|#


(defun beforep (x y l)
  "Returns true if x appears before y in l."
  (member y (member x l)))

#|

CL-USER> (beforep 'not 'whom '(Ask not for whom the bell tolls))
(WHOM THE BELL TOLLS)
CL-USER> (beforep 'thee 'tolls '(It tolls for thee))
NIL

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
;;; SUBSECTION 6.6.2  INTERSECTION
;;;

;;; INTERSECTION takes two sets as input, and  returns a list containing items
;;; appearing in both sets.

;;; The exact order of the items in the output is upto the implementation, and
;;; is unimportant for sets anyway.

;;; INTERSECTION: No duplicates
(intersection '(fred john mary) '(sue mary fred))
;(MARY FRED)

;;; INTERSECTION: With duplicates
(intersection '(fred john mary john) '(sue mary fred))
;(MARY FRED)

;;; INTERSECTION: With duplicates
(intersection '(fred john mary 'fred) '(sue mary fred))
;(MARY FRED)

;;; INTERSECTION: With duplicates
(intersection '(sue mary fred) '(fred john mary 'fred))
;(FRED MARY)

;;; INTERSECTION: With duplicates
(intersection '(fred john mary) '(fred sue mary fred))
;(MARY FRED)

;;; INTERSECTION: With duplicates
(intersection '(fred john mary fred) '(fred sue mary fred))
;(FRED MARY FRED)

;;; INTERSECTION: With duplicates
(intersection '(fred fred fred fred fred) '(fred sue mary fred))
;(FRED FRED FRED FRED FRED)

;;; INTERSECTION: With duplicates
(intersection '(fred sue mary fred) '(fred fred fred fred fred))
;(FRED FRED)



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
;;; SUBSECTION 6.6.3  UNION
;;;

;;; UNION returns a list of items that appear in either set.

(union '(fred john mary) '(fred))
;(MARY JOHN FRED)

;;; UNION: With duplicates
(union '(fred fred fred fred fred) '(fred sue mary fred))
;(FRED SUE MARY FRED)

;;; UNION: With duplicates
(union '(fred sue mary fred) '(fred fred fred fred fred))
;(FRED SUE MARY FRED)



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
;;; SUBSECTION 6.6.4  SET-DIFFERENCE
;;;

(set-difference '(alpha bravo charlie delta) '(bravo charlie))
;(DELTA ALPHA)

(set-difference '(alpha bravo charlie delta) '(echo alpha foxtrot))
;(DELTA CHARLIE BRAVO)

(set-difference '(echo alpha foxtrot) '(alpha bravo charlie delta))
;(FOXTROT ECHO)

(set-difference '(alpha bravo charlie delta) '(alpha bravo charlie delta))
;NIL


;;; SET-DIFFERENCE is not a symmetric function.

(defvar line1 '(All things in moderation))

(defvar line2 '(Moderation in the defence of liberty is no virtue))

(set-difference line1 line2)
;(THINGS ALL)

(set-difference line2 line1)
;(VIRTUE NO IS LIBERTY OF DEFENCE THE)



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
;;; SUBSECTION 6.6.5  SUBSETP
;;;

;;; SUBSETP returns T if every element of the first set is an element of the
;;; second input.

(subsetp '(a i) '(a e i o u))
;T

(subsetp '(a x) '(a e i o u))
;NIL

(subsetp '(a e i o u) '(a e i o u))
;T



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
;;;
;;; SECTION 6.7  PROGRAMMING WITH SETS
;;;
;;;



;;;
;;;
;;; SECTION 6.8  LISTS AS TABLES
;;;
;;;

;;; An association-list (a-list for short) is a list of lists. Each list in an
;;; a-list is called an "entry". The car of each entry is called its "key".

(defvar words '((one un)
		(two deux)
		(three troix)
		(four quatre)
		(five cinq)))

;;; ASSOC

;;; ASSOC takes a key and an association-list (an a-list), and returns the
;;; association corresponding to the key) or NIL if no such key is present
;;; in the a-list.

;;; ASSOC searches for the given key in the CAR of each association (entry) in
;;; the a-list until it finds a matching association.

(assoc 'three words)
;(THREE TROIX)

(assoc 'six words)
;NIL


;;; RASSOC (reverse association)

;;; RASSOC takes a key and a table, and returns the element corresponding to
;;; the key or NIL if no such key is present in the table.

;;; RASSOC searches for the given key in the CDR of each entry in the table
;;; until it finds a matching entry. To use RASSOC with symbols as keys, the
;;; table must be a list of dotted-pairs.

(defvar animal-sounds '((cow . moo)
			(pig . oink)
			(cat . meow)
			(dog . woof)
			(bird . tweet)))

(rassoc 'woof animal-sounds)
;(DOG . WOOF)

(assoc 'woof animal-sounds)
;NIL

(rassoc 'un words)
;NIL





;;;
;;;
;;; SECTION 6.9  PROGRAMMING WITH TABLES
;;;
;;;

(defvar objts '((ob-1 large green shiny cube)
	      (ob-2 small red dull metal cube)
	      (ob-3 red small dull plastic cube)
	      (ob-4 small dull blue metal cube)
	      (ob-5 small shiny red four-sided pyramid)
	      (ob-6 large shiny green sphere)))

(defvar tbl-property-to-quality '((large       .  size)
				(small       .  size)
				(red         .  color)
				(green       .  color)
				(blue        .  color)
				(shiny       .  luster)
				(dull        .  luster)
				(metal       .  material)
				(plastic     .  material)
				(cube        .  shape)
				(sphere      .  shape)
				(pyramid     .  shape)
				(four-sided  .  shape)))

(defun dscrb-objt (o)
  "Takes an object and returns a list of its properties."
  (rest (assoc o objts)))

;;; Get differences in the properties of two objects using set-exclusive-or

(defun diff-in-props (x y)
  "Takes two lists of properties and returns their difference."
  (set-exclusive-or (dscrb-objt x) (dscrb-objt y)))


(defun quality-of (p)
  "Takes a property and returns its corresponding quality."
  (cdr (assoc p tbl-property-to-quality)))


(defun compare-qualities (x y)
  (remove-duplicates (sublis tbl-property-to-quality (diff-in-props x y))))

#|

CL-USER> (diff-in-props 'ob-2 'ob-3)
(PLASTIC METAL)
CL-USER> (quality-of 'red)
COLOR
CL-USER> (compare-qualities 'ob-3 'ob-4)
(MATERIAL COLOR)

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




;;;
;;;
;;; SECTION 6.10  TREES
;;;
;;;

;;; Trees are nested lists.

;;; SUBST substitutes one item for another everywhere it appears in a list.
;;; SUBST looks at the entire structure of the list including its inner
;;; elements.


;;; SUBLIS substitutes many lists simultaneously. It takes two inputs:
;;; a) a table whose entries are dotted-pairs, and b) a list in which the
;;; substitutions are to be made.

(defvar dotted-words '((one    . un)
		     (two    . duex)
		     (three  . troix)
		     (four   . quatre)
		     (five   . cinq)))

#|

CL-USER> (sublis '((roses . violets)
		   (red   . blue))
		 '(roses are red))

(VIOLETS ARE BLUE)
CL-USER> (sublis dotted-words '(three point one four one five two))
(TROIX POINT UN QUATRE UN CINQ DUEX)

|#





;;;
;;;
;;; SECTION 6.11 EFFICIENCY OF LIST OPERATIONS
;;;
;;;

;;; Extracting the first element of a list means merely querying the CAR of
;;; the first cons-cell of the list for the pointer to the first element of
;;; the list. Extracting the last element of a list amounts to "cdring down
;;; the list" and stopping at the cons-cell whose CDR is an atom.

;;; The amount of garbage-collection in a function is proportional to the
;;; amount of consing the function does.





;;;
;;;
;;; SECTION 6.12  SHARED STRUCTURE
;;;
;;;

;;; Two lists are said to share structure if they have cons-cells in common.
;;; Lists typed in via a keyboard will never share structure because READ
;;; always builds all required lists from new cons-cells.

#|

CL-USER> (defvar sharer-1 '(a b c))
SHARER-1
CL-USER> (defvar sharer-2 (cons 'd (cdr sharer-1)))
SHARER-2

CL-USER> (sdraw sharer-2)

[*|*]--->[*|*]--->[*|*]--->NIL
 |        |        |
 v        v        v
 D        B        C
; No value

|#




;;;
;;;
;;; SECTION 6.13  EQUALITY OF OBJECTS
;;;
;;;

;;; There can only be one symbol in the computer's memory with a given name,
;;; assuming that only the standard packages (i.e. namespaces) are present,
;;; and there are no uninterned symbols.

;;; Every object in memory has a numbered location, called its address. Each
;;; symbol has a unique address because a symbol exists in only one place in
;;; memory.

;;; E.g. in the list (TIME AFTER TIME) the two occurences of the symbol TIME
;;; refer to the same address. There cannot be two separate symbols named
;;; TIME.

;;; In Lisp, lists are not unique. The symbols the lists point to will be
;;; unique but the lists themselves will not be. Therefore we cannot use
;;; EQUAL to compare list by comparing their addresses.


;;; EQUAL compares two lists element-by-element.

#|

CL-USER> (defvar l-1 (list 'a 'b 'c))
L-1
CL-USER> (defvar l-2 (list 'a 'b 'c))
L-2

CL-USER> (equal l-1 l-2)
T

|#


;;; EQ

;;; EQ compares two lists by merely comparing their pointers. Thus, EQ is
;;; much more efficient than EQUAL, and is therefore preferred over EQUAL by
;;; programmers while comparing symbols. Conversely, they don't use EQ with
;;; lists unless they wish to tell whether two cons-cells are the same.

#|
CL-USER> (eq l-1 l-2)
NIL
|#

;;; Here the two lists l-1 and l-2 are EQUAL but not EQ.



;;; EQL, the standard predicate for comparison in Lisp.

;;; EQL is a more general version of EQ, and is used to compare numbers. EQL
;;; compares the addresses of objects as does EQ, but if the objects are
;;; numbers, then it will compare their values instead. The numbers being
;;; compared must be of the same type (e.g. both integers). EQL will return
;;; NIL if we use it to compare numbers that are of different types even if
;;; their values are the same. Such numbers are said to be not EQL.

#|

CL-USER> (eql 'foo 'foo)
T
CL-USER> (eql 3 3)
T
CL-USER> (eql 3 3.0)
NIL

|#


;;; For comparing numbers of the same or different types one can use = . It
;;; will be an error if we use = to compare objects other than numbers. = is
;;; the only way to compare numbers of different types.

#|

CL-USER> (= 3 3.0)
T
CL-USER> (= 'foo 'foo)
; Evaluation aborted on #<TYPE-ERROR expected-type: NUMBER datum: FOO>.

|#


;;; EQUALP

;;; EQUALP is similar to EQUAL but more liberal. E.g. EQUALP will ignore the
;;; case when comparing strings.

#|

CL-USER> (equal "foo" "FOO")
NIL
CL-USER> (equalp "foo" "FOO")
T

|#





;;;
;;;
;;; SECTION 6.14  KEYWORD ARGUMENTS
;;;
;;;

;;; A keyword is a special type of symbol whose name is always preceded by a
;;; symbol.

;;; Keywords always evaluate to themselves, and so need not be quoted.

(defvar text '(b a n a n a - p a n d a))

#|

CL-USER> (symbolp :count)
T
CL-USER> (keywordp :count)
T
CL-USER> (remove 'a text)
(B N N - P N D)
CL-USER> (remove 'a text :count 3)
(B N N - P A N D A)
CL-USER> (remove 'a text :count 3 :from-end t)
(B A N A N - P N D)
CL-USER> :count
:COUNT

|#


;;; The symbols COUNT and :COUNT are not EQ. They exist in different
;;; packages and so are distinct.



;;; MEMBER uses EQL (which will work correctly for numbers and symbols) to
;;; test whether an item appears in a set. We need to use EQUAL rather than
;;; EQL if our set contains lists.

(defvar set-of-cards '((3 clubs) (5 diamonds) (ace spades)))

#|

CL-USER> (member '(5 diamonds) set-of-cards)
NIL
CL-USER> (second set-of-cards)
(5 DIAMONDS)
CL-USER> (eql (second set-of-cards) '(5 diamonds))
NIL
CL-USER> (equal (second set-of-cards) '(5 diamonds))
T

|#


;;; With member we can use the :TEST keyword along with quoting the function
;;; EQUAL if our set contains lists. All functions on lists that include
;;; equality tests accept a :TEST keyword argument.

#|

CL-USER> (member '(5 diamonds) set-of-cards :test #'equal)
((5 DIAMONDS) (ACE SPADES))
CL-USER> (remove '(5 diamonds) set-of-cards :test #'equal)
((3 CLUBS) (ACE SPADES))

|#






;;;
;;;
;;;
;;; CHAPTER 7  APPLICATIVE PROGRAMMING
;;;
;;;
;;;





;;;
;;;
;;; SECTION 7.1  INTRODUCTION
;;;
;;;

;;; Applicative programming is based on the idea that functions are data, as
;;; are symbols and lists. Applicative operators are functions that take
;;; another function as input and apply it to a list in various ways. 
;;; These operators are all built from a primitive function called FUNCALL.





;;;
;;;
;;; SECTION 7.2  FUNCALL
;;;
;;;

;;; FUNCALL calls a function on some inputs.


(defvar fn-cons #'cons)
;#<FUNCTION CONS> 

(defvar fn-funcall #'funcall)
;#<FUNCTION FUNCALL>

#|

CL-USER> (funcall #'cons 'a 'b)
(A . B)
CL-USER> (type-of fn-cons)
FUNCTION
CL-USER> (type-of #'cons)
FUNCTION
CL-USER> (type-of #'funcall)
FUNCTION
CL-USER> (type-of fn-funcall)
FUNCTION

|#

;;; Sharp-quoting an undefined function, a special-function or a macro will
;;; result in an error.






;;;
;;;
;;; SECTION 7.3  THE MAPCAR OPERATOR
;;;
;;;

;;; MAPCAR applies a function to each element of a list, one at a time, and
;;; returns a list of the results.





;;;
;;;
;;; SECTION 7.4  MANIPULATING TABLES WITH MAPCAR
;;;
;;;

(defun en-to-fr (x)
  (second (assoc x words)))

#|

CL-USER> (mapcar #'first words)
(ONE TWO THREE FOUR FIVE)
CL-USER> (mapcar #'reverse words)
((UN ONE) (DEUX TWO) (TROIX THREE) (QUATRE FOUR) (CINQ FIVE))
CL-USER> (mapcar #'en-to-fr '(three one four one five))
(TROIX UN QUATRE UN CINQ)

|#




;;;
;;;
;;; SECTION 7.5  LAMBDA EXPRESSIONS
;;;
;;;

(mapcar #'(lambda (x) (list 'Hi 'there x)) '(joe fred wanda))
;((HI THERE JOE) (HI THERE FRED) (HI THERE WANDA))


;;; Lexical closures

#|

CL-USER> (lambda (n) (* n 10))
#<FUNCTION (LAMBDA (N)) {52DEF51B}>
CL-USER> #'(lambda (n) (* n 10))
#<FUNCTION (LAMBDA (N)) {52E1667B}>

|#





;;;
;;;
;;; SECTION 7.6  THE FIND-IF OPERATOR
;;;
;;;

;;; FIND-IF is an applicative operator takes a predicate and a list as inputs,
;;; and returns the first element in the list for which the predicate holds,
;;; or NIL if no such element exists.

#|

CL-USER> (find-if #'oddp '(2 4 6 7 8 9))
7
CL-USER> (find-if #'(lambda (n) (> n 3)) '(1 2 3 4 6 8))
4

|#





;;;
;;;
;;; SECTION 7.8  REMOVE-IF AND REMOVE-IF-NOT
;;;
;;;

;;; REMOVE-IF takes a predicate and a list and removes all the items from
;;; the list for which the predicate holds.

(remove-if #'numberp '(2 for 1 sale))
;(FOR SALE)


;;; REMOVE-IF-NOT takes a predicate and a list and removes all the items from
;;; the list for which the predicate does not hold.

(remove-if-not #'numberp '(2 for 1 sale))
;(2 1)

 



;;;
;;;
;;; SECTION 7.9  THE REDUCE OPERATOR
;;;
;;;

;;; REDUCE takes a function that accepts two inputs and a list and reduces
;;; the list to a single value by applying the function successively.

#|

CL-USER> (reduce #'+ '(1 2 3 4 5))
15
CL-USER> (reduce #'append words)
(ONE UN TWO DEUX THREE TROIX FOUR QUATRE FIVE CINQ)

|#


;;;
;;; EXERCISE 7.16
;;;

#|

CL-USER> (reduce #'union '((a b c) (c d a) (f e d) (g)))
(D E F B C A G)

|#





;;;
;;;
;;; SECTION 7.10  EVERY
;;;
;;;

;;; EVERY takes a predicate and a list as inputs and returns true if the
;;; predicate holds for every element of the list.

(every #'numberp '(1 2 A B 3 4 5))
;NIL

;;; EVERY will return T if the input list is empty because there will be no
;;; element for which the predicate will not hold.

(every #'oddp NIL)
;T


;;; EVERY can also operate on multiple lists when the predicate accepts
;;; multiple inputs.

(every #'> '(2 3 4 5 6 7) '(1 2 3 4 5 6))
;T



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
;;;
;;; SECTION 7.11  OPERATING ON MULTIPLE LISTS
;;;
;;;

#|

CL-USER> (mapcar #'(lambda (p j) (list p 'gets j))
	'(fred wilma george diane)
	'(carpentry plumbing car-repair masonry))
((FRED GETS CARPENTRY) (WILMA GETS PLUMBING) (GEORGE GETS CAR-REPAIR) (DIANE GETS MASONRY))

CL-USER> (mapcar #'cons '(a b c d e f) '(1 2 3 4 5 6))
((A . 1) (B . 2) (C . 3) (D . 4) (E . 5) (F . 6))

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
;;;
;;; SECTION 7.12  THE SPECIAL FUNCTION: FUNCTION
;;;
;;;

#|

|
CL-USER> 'cons
CONS
CL-USER> #'cons
#<FUNCTION CONS>
CL-USER> #' (lambda (n) (+ n 2))
#<FUNCTION (LAMBDA (N)) {52FC5C3B}>

|#


;;; The function or lexical closure returned by sharp-quoting may
;;; be stored in a variable. When this is done the value of
;;; variable is the function or lexical-closure. That is, the 

(defvar lex-clo #'(lambda (x) (* x 10)))
;LEX-CLO



;;;
;;; 7.13  KEYWORD ARGUMENTS TO APPLICATIVE OPERATORS
;;;

#|

CL-USER> (find-if #'oddp '(1 2 3 4 5 6) :from-end t)
5
CL-USER> (reduce #'cons '(a b c d e f))
(((((A . B) . C) . D) . E) . F)
CL-USER> (reduce #'cons '(a b c d e f) :from-end t)
(A B C D E . F)

|#



;;;
;;;
;;; SECTION 7.14  SCOPING AND LEXICAL CLOSURES
;;;
;;;

;;; Every lexical context has a parent context.





;;;
;;;
;;; SECTION 7.15  WRITING AN APPLICATIVE OPERATOR
;;;
;;;

(defun inalienable-rights (fn)
  (funcall fn '(life liberty and the pursuit of happiness)))

#|

CL-USER> (inalienable-rights #'length)
7
CL-USER> (inalienable-rights #'reverse)
(HAPPINESS OF PURSUIT THE AND LIBERTY LIFE)
CL-USER> (inalienable-rights #'(lambda (x) (cons 'high x)))
(HIGH LIFE LIBERTY AND THE PURSUIT OF HAPPINESS)

|#



;;;
;;;
;;; SECTION 7.16  FUNCTIONS THAT MAKE FUNCTIONS
;;;
;;;

(defun make-greater-than-predicate (n)
  "Creates a lexical closure which takes a number and returns true if
   another number is greater than this number."
  #'(lambda (x) (> x n)))

#|

CL-USER> (funcall (make-greater-than-predicate 3) 5)
T
CL-USER> (defvar pred (make-greater-than-predicate 3))
#<CLOSURE (LAMBDA (X) :IN MAKE-GREATER-THAN-PREDICATE) {1004236B8B}>
CL-USER> (funcall pred 2)
NIL
CL-USER> (funcall pred 5)
T
CL-USER> (remove-if-not pred '(1 2 3 4 5 6))
(4 5 6)

|#





;;;
;;;
;;;
;;; CHAPTER 8  RECURSION
;;;
;;;
;;;

;;; A recursive function is one which calls, i.e. applies, itself.

(defun anyoddp (x)
  "Takes a list of numbers and returns TRUE if any number in the list is
   odd"
  (cond ((null x) NIL)
	((oddp (first x)) t)
	(t (anyoddp (rest x)))))



;;;
;;; EXERCISE 8.1
;;;

#|

Answer:
The cond clause (oddp (first x) ...) is never true in this case.

|#






;;;
;;;
;;; SECTION 8.5  A LISP VERSION OF THE FACTORIAL FUNCTION
;;;
;;;

(defun fact (n)
  "Takes a number and returns its factorial."
  ;(break)
  (if (zerop n)
      1
      ;(/ 1 0)  ; div by 0 to trigger debugger
      (* n (fact (- n 1)))))

(fact 5)
;120



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
;;;
;;; SECTION 8.10  INFINITE RECURSION IN LISP
;;;
;;;

(defun collatz-conj (n)
  (cond ((or (zerop n) (< n 0)) NIL)
	((equal n 1) t)
	((evenp n) (collatz-conj (/ n 2)))
	(t (collatz-conj (+ (* 3 n) 1)))))

#|

CL-USER> (collatz-conj 10)
T

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
;;;
;;; SECTION 8.11  RECURSION TEMPLATES
;;;
;;;


;;;
;;; Tail Recursion

;;; In tail-recursion, in the last expression in the action part of the last
;;; COND clause the function calls itself recursively. The recursive call is
;;; the final action (tail) which the last COND clause performs. That is, the
;;; action part does not do any other work after the recursive call.

;;; In a tail-recursive function all calls return the same value as
;;; the terminal call.

;;; Augmentation of the result is not allowed in tail-recursive functions.


;;;
;;; Double-Test Tail Recursion
;;;

;;; In double-test tail recursion, there are two end-tests. The corresponding
;;; end-value is returned instead of proceeding with recursion, if either of
;;; the tests is true. If both tests are false, then the function reduces its
;;; input somehow and calls itself.

;;; The value returned by the COND is the same value returned by the parent
;;; call.

#|

Template:

(defun func (x)
  (COND (end-test-1 end-value-1)
        (end-test-2 end-value-2)
        (T (func reduced-x))))

|#

(defun anyoddp-rec (x)
  (cond ((null x) NIL)
	((oddp (first x)) T)
	(T (anyoddp-rec (rest x)))))

(anyoddp-rec '(0 2 4 6 8 1))
;T



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
;;; Single-Test Tail Recursion
;;;

;;; Single-test tail recursion is like double-test tail recursion, but with
;;; only one test. This type of recursion is used when it is certain that the
;;; input will eventually yield what the function is looking for.


#|

Template:

(defun func (x)
  (COND (end-test end-value)
        (T (func reduced-x))))

|#

(defun find-first-atom (x)
  (cond ((atom x) x)
	(t (find-first-atom (first x)))))

#|

CL-USER> (find-first-atom 'a)
A
CL-USER> (find-first-atom '(A))
A
CL-USER> (find-first-atom '((((a f)) i) r))
A
CL-USER> (find-first-atom NIL)
NIL
CL-USER> (find-first-atom '(((()))))
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
;;; Augmentation: Augmenting Recursion
;;;

;;; Plain tail-recursion entails an initial step followed by a smaller journey.
;;; Augmentation entails a smaller journey followed by a final step.
;;; The final step consists of choosing an augmentation value and applying it
;;; to the result of the previous recursive call.

#||

Template (augmentation for single-test tail recursion):

(defun func (x)
  (cond (end-test end-value)
        (T (aug-fun aug-val (func reduced-x)))))

||#

(defun count-slices-with-augmentation (x)
  (cond ((null x) 0)
	(t (+ 1 (count-slices-with-augmentation (rest x))))))

(count-slices-with-augmentation '(a b c d e f))
;6



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
;;;
;;; SECTION 8.12  VARIATIONS ON THE BASIC TEMPLATES
;;;
;;;
 

;;;
;;; SUBSECTION 8.12.1  List-Consing Recursion
;;;

;;; List-consing recursion is a special case of augmenting recursion where the
;;; augmenting function is CONS. The depth of the recursion is the length of
;;; resulting cons-cell chain, plus one (because the last call returns NIL
;;; instead of returning a cons-cell).

#||

Template:

(defun func (n)
  (cond (end-test NIL)
        (t (cons new-element (func reduced-n)))))

||#



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
;;; SUBSECTION 8.12.2  Simultaneous Recursion on Several Variables
;;;

;;; In this variation the function has multiple inputs and one or more of them
;;; is "reduced" with each recursive call.

#||

Template:

(DEFUN func (N X)
  (COND (end-test end-value)
        (T (func reduced-n reduced-x))))

||#

(defun nth-recursively (n x)
  (cond ((zerop n) (first x))
	(t (nth-recursively (- n 1) (rest x)))))

#|

CL-USER> (nth-recursively 0 '())
NIL
CL-USER> (nth-recursively 0 '(a))
A
CL-USER> (nth-recursively 3 '(a b c d))
D
CL-USER> (nth-recursively 60 '(a b c d))
NIL

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
;;; SUBSECTION 8.12.3  Conditional Augmentation
;;;

;;; In conditional augmentation we augment the recursion with only those values
;;; which satisfy a given condition, and not for all values as in regular
;;; augmentation.

#||

Template:

(DEFUN func (x)
  (COND (end-test end-value)
        (aug-test (aug-func aug-value (func reduced-x)))
        (T (func reduced-x))))

||#

(defun extract-symbols (x)
  "Takes a list (x) as input, and returns a list of only those elements of the
   list that are symbols."
  (cond ((null x) NIL)
	((symbolp (first x))
	 (cons (first x) (extract-symbols (rest x))))
	(t (extract-symbols (rest x)))))

(extract-symbols '(3 bears and 1 girl))
;(BEARS AND GIRL)


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
;;; SUBSECTION 8.12.4  Multiple Recursion
;;;

;;; A function is multiple-recursive if it makes more than one recursive call
;;; with each invocation. The multiple recursive-calls reside side-by-side
;;; within the parent.

;;; Multiple-recursion is not the same as simultaneous-recursion (where
;;; multiple variables are reduced simultaneously.)

;;; Multiple-recursive functions have several terminal-calls. (A terminal-call
;;; is a function call that does not recurse any further.)
#||

Template:

(DEFUN func (n)
  (COND (end-test-1 end-value-1)
        (end-test-2 end-value-2)
        (t (combiner (func reduced-n-1) (func reduced-n-2)))))

||#

(defun fib-mult-rec (n)
  (cond ((equal n 0) 1)
	((equal n 1) 1)
	(t (+ (fib-mult-rec (- n 1)) (fib-mult-rec (- n 2))))))

(fib-mult-rec 0)
;1

(fib-mult-rec 1)
;1

(fib-mult-rec 10)
;89





;;;
;;;
;;; SECTION 8.13  TREES AND CAR/CDR RECURSION
;;;
;;;

;;; CAR/CDR is a special case of multiple recursion and is used on binary
;;; trees.

;;; Binary trees are very irregular: each node is either an atom or a cons with
;;; two branches, the CAR;;; and the CDR (i.e a further binary tree).

;;; To process arbitrarily nested lists, consider them to be binary trees.
;;; Process the atoms, and use CAR/CDR recursion on each cons (i.e. call the
;;; function recursively on the CAR and CDR of each cons of the input.

#||

Template:

(DEFUN func (X)
  (COND (end-test-1 end-value-1)
        (end-test-2 end-value-2)
        (T (combiner (func (CAR X))
                     (func (CDR X))))))

||#

(defun find-number (x)
  "Takes an arbitrarily nested list (x), and returns T as soon as it finds a
   number in the list, or NIL otherwise."
  (cond ((numberp x) x)
	((atom x) NIL)
	(t (or (find-number (car x)) (find-number (cdr x))))))

(find-number NIL)
;NIL

(find-number '(a))
;NIL

(find-number '(2))
;2

(find-number '(((goldilocks . and)) (the . 3) bears))
;3


;;; Besides searching within trees, CAR/CDR recursion can be used for building
;;; trees by using CONS as the combiner.

(defun atoms-to-q (x)
  "Takes a binary-tree (x), and returns a new binary-tree in which every
   non-NIL atom in x is replaced by Q."
  (cond ((null x) NIL)
	((atom x) 'Q)
	(t (cons (atoms-to-q (car x)) (atoms-to-q (cdr x))))))


;(Q Q)

(atoms-to-q '(hark (harold the angel) sings))
;(Q (Q Q Q) Q)


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
;;;
;;;
;;; SECTION 8.14  USING HELPER FUNCTIONS
;;;
;;;
;;;

(defun count-up (n)
  "Takes a number (n), and displays all numbers from 1 upto and including n
   while making use of a helper function."
  (count-up-recursively 0 n))

(defun count-up-recursively (cnt n)
  "Takes a number (n), and conses all numbers from 1 upto and including n.
   It is a helper function for count-up."
  (cond ((> cnt n) NIL)
	(t (cons cnt (count-up-recursively (incf cnt) n)))))

#|

CL-USER> (count-up-recursively 0 0)
(0)
CL-USER> (count-up-recursively 0 1)
(0 1)
CL-USER> (count-up 5)
(0 1 2 3 4 5)
CL-USER> (count-up -5)
NIL

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
;;;
;;; SECTION 8.15  RECURSION IN ART AND LITERATURE
;;;
;;;


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
;;;
;;; SECTION 8.16  ADVANTAGES OF TAIL RECURSION
;;;
;;;

;;; Lisp can execute tail-recursive functions more efficiently than
;;; ordinary functions, by replacing the recursive call with a jump.

;;; It is better to write recursive-functions in tail-recursive form
;;; whenever possible.

;;; Introduce an extra variable to produce a tail-recursive version of an
;;; ordinary recursive function.

(defun my-reverse (x)
  "Takes a list (x), and returns its reverse by using augmentation rather 
   than tail-recursion."
  (cond ((null x) NIL)
	(t (append (reverse (rest x)) (list (first x))))))

#|

CL-USER> (my-reverse NIL)
NIL
CL-USER> (my-reverse '(1 2 3 4 5 6))
(6 5 4 3 2 1)

|#


(defun tr-reverse (x)
  (tr-reverse-helper x NIL))

(defun tr-reverse-helper (x result)
  (cond ((null x) NIL)
	(t (tr-reverse-helper (rest x)
			      (cons (first x) result)))))


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
;;;
;;; SECTION 8.17 WRITING NEW APPLICATIVE OPERATORS
;;;
;;;

;;; We can write custom applicative operators that can use FUNCALL.

(defun my-mapcar (f x)
  "A version of the function mapcar that applied a function to only a single
   list. Takes a function (f) that takes a single argument, and a list (x),
   and returns a list obtained by applying f to x."
  (cond ((null x) NIL)
	(t (cons (funcall f (first x)) (my-mapcar f (rest x))))))

#|

CL-USER> (my-mapcar #'evenp NIL)
NIL
CL-USER> (my-mapcar #'evenp '(1))
(NIL)
CL-USER> (my-mapcar #'evenp '(1 2 3 4 5 6))
(NIL T NIL T NIL T)

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
;;;
;;; SECTION 8.18  THE LABELS SPECIAL FUNCTION
;;;
;;;

;;; The LABELS special function allows us to define functions locally inside
;;; the body of the main function. 

;;; The syntax of a LABELS form is the same as that of the LET form.

#|

 (LABELS ((fn-1 args-1 body-1)
          ...
          (fn-n args-n body-n))
   body)
         
|#

;;; The body can call any of the local functions. The local functions can call
;;; each other. The local functions can reference their parent's variables.

;;; A disadvantage of using LABELS is that there is no way to trace functions
;;; that are defined inside a LABELS expression.

;;; QUESTION: can't the effect of labels be achieved by being able to defun
;;; functions inside the main function?

(defun count-up-using-labels (n)
  (labels ((count-up-recursively-2 (cnt)
	     (if (> cnt n) NIL
		 (cons cnt (count-up-recursively-2 (+ cnt 1))))))
    (count-up-recursively-2 1)))

(count-up-using-labels 6)
;(1 2 3 4 5 6)


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
;;;
;;; SECTION 8.19  RECURSIVE DATA STRUCTURES
;;;
;;;


;;;
;;; Arithmetic expressions
;;;

;;; Definition:

;;; An arithmetic expression is either a number, or a three-element list whose
;;; first and third elements are arithmetic expressions and whose middle
;;; element is one of +, -, * or /.



;;;
;;; Trees
;;; 

;;; Definition:

;;; A tree is either a single terminal-node, or a non-terminal-node whose
;;; branches are trees.

;;; Trees are naturally represented by lists.



;;;
;;; S-Expressions
;;;

;;; Definition:

;;; A S-expression is either an atom or a cons-cell whose CAR and CDR parts
;;; are s-expressions.

;;; S-expressions are an example of a tree data-structure, which is a
;;; recursive data-structure.

;;; The terminal-nodes of a tree are those that have no branches descending
;;; from them. The remaining nodes are called non-terminal-nodes.


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

 




;;; 
;;;
;;;
;;; CHAPTER 9  INPUT / OUTPUT
;;;
;;;
;;;



;;;
;;;
;;; SECTION 9.3  THE FORMAT FUNCTION
;;;
;;;

;;; It is good practice to begin each line with ~& so that the cursor is
;;; guraranteed to on a fresh line before printing each message.

(defun marys-bat ()
  (format t "~&Mary had a little bat.~%~&Its wings were brown and long."))

#|

CL-USER> (marys-bat)
Mary had a little bat.
Its wings were brown and long.
NIL

|#


;;; ~S inserts the printed representation of a Lisp s-expression into the
;;; message that format prints.

(defun square-talk (n)
  (format t "~&~S squared is ~S~%" n (* n n)))

#|

CL-USER> (square-talk 10)
10 squared is 100
NIL
CL-USER> (mapcar #'square-talk '(1 2 3 4 5 6))
1 squared is 1
2 squared is 4
3 squared is 9
4 squared is 16
5 squared is 25
6 squared is 36
(NIL NIL NIL NIL NIL NIL)

|#


;;; ~A prints an object without using escape characters, e.g. quotation marks.

#|

CL-USER> (format t "Without quotes:  ~A~%" "Hello, world!")
Without quotes:  Hello, world!
NIL

|#


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
;;;
;;; SECTION 9.4  THE READ FUNCTION
;;;
;;;

;;; The function read reads one object (a symbol, list, number, etc) from the
;;; kbd and returns that object as its value. The object will not be
;;; evaluated, and so need not be quoted.

(defun my-square ()
  (format t "Type in a number: ")
  (let ((x (read)))
    (format t "The number ~S squared is ~S.~%~&" x (* x x))))

#|

CL-USER> (my-square) 
Type in a number: 2
The number 2 squared is 4.
NIL

|#


;;;
;;; EXERCISE 9.6
;;;

;;; SKIP



;;;
;;; EXERCISE 9.7
;;;

;;; SKIP





;;;
;;;
;;; SECTION 9.5  THE YES-OR-NO-P FUNCTION
;;;
;;;

;;; The YES-OR-NO-P function takes a format string as input and asks the user
;;; a yes-or-no question. The function returns T if the user enters "yes", and
;;; NIL if the user enters "no".

(defun nirwana ()
  (if (yes-or-no-p "Do you seek nirwana? ")
      (format t "Then do not ask for it.")
      (format t "You are it.")))

#|

CL-USER> (nirwana)

Do you seek nirwana?  (yes or no) yes
Then do not ask for it.
NIL
CL-USER> (nirwana)

Do you seek nirwana?  (yes or no) no
You are it.
NIL

|#






;;;
;;;
;;; SECTION 9.6  READING FILES WITH WITH-OPEN-FILE MACRO
;;;
;;;

;;; WITH-OPEN-FILE creates a local variable (as does LET) and sets it to a
;;; stream object representing a connection to that file. When the function is
;;; exited the connection to the file is automatically closed.

;;; A stream object is a special Lisp datatype which describes connections
;;; to files.

#|

CL-USER> *terminal-io*
#<SYNONYM-STREAM :SYMBOL SWANK::*CURRENT-TERMINAL-IO* {1001871983}>

|#

(defun get-tree-data ()
  "Reads the file timber.dat"
  (with-open-file (fstream "~/Work/CommonLisp/Data/timber.dat")
    (let* ((tree-loc (read fstream))
	   (tree-table (read fstream))
	   (num-trees (read fstream)))
      (format t "~&There are ~S trees on ~S.~%" num-trees tree-loc)
      (format t "~&They are:  ~S~%" tree-table))))

#|

CL-USER> (get-tree-data)
There are 100 trees on "The North Slope".
They are:  ((45 REDWOOD) (12 OAK) (43 MAPLE))
NIL

|#
 




;;; 
;;;
;;; SECTION 9.7  WRITING FILES WITH WITH-OPEN-FILE MACRO
;;;
;;;

;;; We can use WITH-OPEN-FILE along with its keyword arguments :direction and
;;; :output to open files for writing, and then pass the stream created to
;;; format.

(defun save-tree-data (tree-loc tree-table num-trees)
  "Opens a file for writing and updates the tree data in it."
  (with-open-file (fstream "~/tmp/new-timber.dat"
			   :direction :output)
    (format fstream "~S~%" tree-loc)
    (format fstream "~S~%" tree-table)
    (format fstream "~S~%" num-trees)))

#|

CL-USER> (save-tree-data 
	  "The West Ridge"
	  '((45 redwood) (22 oak) (43 maple))
	  110)
	  
NIL

|#;



;;;
;;; EXERCISE 9.8
;;;

#|

SYMBOLS                                  STRINGS
-------------------------------------------------------------------------------
Unique data-structures having five       Vectors having character values as
fields: symbol-name, symbol-package,     their elements.
symbol-value, symbol-function and
symbol-plist

Used to name variables.                  Used as values of variables.

Evaluate to themselves.                  Evaluate to themselves.

(type-of 'symb)                          (type-of "symbol") 
=> SYMBOL                                => (SIMPLE-ARRAY CHARACTER (4)) 

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







;;;
;;;
;;; SECTION 9.8  PARAMETERS TO FORMAT DIRECTIVES
;;;
;;;

(defparameter glee-club '((john smith) (barbara wilson) (mustapha ali)))

(defun print-one-name (name)
  (format t "~&~10S ~S~%" (second name) (first name)))

(defun print-all-names (x)
  (mapcar #'print-one-name x)
  'done)

(print-all-names glee-club)
#|

SMITH      JOHN
WILSON     BARBARA
ALI        MUSTAPHA
DONE

|#





;;;
;;;
;;; SECTION 9.9  ADDITIONAL FORMAT DIRECTIVES
;;;
;;;

(defun sevenths (x)
  (mapcar #'(lambda (numerator)
	      (format t "~&~4,2F / 7 is ~7,5F~%" numerator (/ numerator 7.0)))
	  x)
  'done)

(sevenths '(1 3/2 2 2.5 3))

#|

1.00 / 7 is 0.14286
1.50 / 7 is 0.21429
2.00 / 7 is 0.28571
2.50 / 7 is 0.35714
3.00 / 7 is 0.42857
DONE

|#






;;;
;;;
;;; SECTION 9.10  LISP 1.5 OUTPUT PRIMITIVES
;;;
;;;

#|

CL-USER> (defparameter str-a "Wherefore art thou, Romeo?")
STR-A
CL-USER> (prin1 str-a)
"Wherefore art thou, Romeo?"
"Wherefore art thou, Romeo?"
CL-USER> (princ str-a)
Wherefore art thou, Romeo?
"Wherefore art thou, Romeo?"

|#


(defun my-print (x)
  (terpri)
  (prin1 x)
  (princ "~%")
  x)

#|

(mapcar #'my-print '(0 1 2 3 4 5))
0 
1 
2 
3 
4 
5 (0 1 2 3 4 5)

|#





;;;
;;;
;;; SECTION 9.11 HANDLING EOF CONDITIONS
;;;
;;;

;;; We tell READ to return a special value, called an EOF indicator, instead
;;; of generating an error on eof by supplying 1) a NIL, and 2) a fresh cons
;;; cell to indicate the eof. Any new cons-cell will do, as only the address
;;; of the cons-cell is important and not its contents.
 
(defun read-my-file ()
  (with-open-file (fstream "~/Work/Examples/CommonLisp/test/ex.dat")
    (let ((contents (read-all-objects fstream (list '$eof$))))
      ;(break "Before calling format.")
      (format t "~&TEST")
      ;(format t "~&Read ~S objects from the file." (length contents))
      contents)))

(defun read-all-objects (fstream eof-indicator)
  (let ((result (read fstream NIL eof-indicator)))
    (if (eq result eof-indicator)
	NIL
	(cons result (read-all-objects fstream eof-indicator)))))



 
;;;
;;;
;;; SECTION 9.12  PRINTING IN DOT NOTATION
;;;
;;;

;;; SKIP



;;;
;;; EXERCISE 9.11
;;;

;;; SKIP



;;;
;;; EXERCISE 9.12
;;;

;;; SKIP



;;;
;;; EXERCISE 9.13
;;;

;;; SKIP



;;;
;;; EXERCISE 9.14
;;;

;;; SKIP




;;;
;;;
;;; SECTION 9.13  HYBRID NOTATION
;;;
;;;

;;; SKIP



;;;
;;; EXERCISE 9.15
;;;

;;; TODO







;;;
;;;
;;;
;;; CHAPTER 10  ASSIGNMENT
;;;
;;;
;;;


;;;
;;;
;;; SECTION 10.3  STEREOTYPICAL UPDATING METHODS
;;;
;;;


;;;
;;; SUBSECTION 10.3.1  The INCF and DECF Macros
;;;

(defparameter my-stack NIL)


;;;
;;; SUBSECTION 10.3.2  The PUSH and POP Macros
;;;

#|

CL-USER> (push 'dish-1 my-stack)
(DISH-1)
CL-USER> (push 'dish-2 my-stack)
(DISH-2 DISH-1)
CL-USER> (push 'dish-3 my-stack)
(DISH-3 DISH-2 DISH-1)
CL-USER> (pop my-stack)
DISH-3
CL-USER> my-stack
(DISH-2 DISH-1)

my-stack is updated to point to the top cons-cell after each operation.

|#


(defun my-pop (x)
  "Takes a list (x), and removes the first elements of x and returns it."
  (let ((top-elem (first x)))
    (format t "x = ~s~%" x)
    (setf x (rest x))
    (format t "x = ~s~%" x)
    top-elem))

#|

CL-USER> (my-pop '(a b c d e f))
x = (A B C D E F)
x = (B C D E F)
A

|#


(defparameter *friends* NIL)

(defun meet (person)
  (cond ((equal person (first *friends*)) 'WE-JUST-MET)
	((member person *friends*) 'WE-KNOW-EACH-OTHER)
	(t (push person *friends*) 'PLEASED-TO-MEET-YOU)))

#|

CL-USER> (meet 'fred)
WE-JUST-MET
CL-USER> (meet 'cindy)
PLEASED-TO-MEET-YOU
CL-USER> (meet 'fred)
WE-KNOW-EACH-OTHER
CL-USER> (meet 'cindy)
WE-JUST-MET

|#


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
;;;
;;; SECTION 10.4  WHEN AND UNLESS
;;;
;;;


;;; Use WHEN and UNLESS conditional forms when there is need to evaluate
;;; more than one expression when the test is true. The value of the body which
;;; is returned is that of the last form. The forms prior to the last one are
;;; useful only for their side-effects such as I/O or assignment.

(defun picky-multiply (x y)
  "Computes X times Y. Input X must be odd: Y must be even."
  (unless (oddp x)
    (incf x)
     (format t "~&Changed X to ~S to make it odd." x))
  (when (oddp y)
    (decf y)
    (format t "~&Changed Y to ~S to make it even." y))
  (format t "~&")
  (* x y))

#|

CL-USER> (picky-multiply 2 9)
Changed X to 3 to make it odd.
Changed Y to 8 to make it even.
24

|#




;;;
;;;
;;; SECTION 10.5  GENERALIZED VARIABLES 
;;;
;;;

;;; A generalized variable is anywhere a pointer may be stored.

;;; Assignment means replacing one pointer with another. Evaluating the form
;;; (incf n 4) mean replacing the pointer to the number 3, held by N, with
;;; another pointer to the number 4.

;;; Assignment macros like DEFVAR, INCF, DECF, PUSH and POP can assign to
;;; generalized variables, which means that they store pointers in many
;;; different places.

;;; DEFVAR stands for "set field".

(defvar line '(Jack Benny was 39 for many years))

#|

CL-USER> (setf (sixth line) 'several)
SEVERAL
CL-USER> line
(JACK BENNY WAS 9 FOR SEVERAL YEARS)
CL-USER> (decf (fourth line) 30)
-21
CL-USER> line
(JACK BENNY WAS -21 FOR SEVERAL YEARS)

|#
 




;;;
;;;
;;; SECTION 10.6  CASE STUDY: A TIC-TAC-TOE PLAYER
;;;
;;;

;;; TODO 





;;;
;;;
;;; SECTION 10.7  DO IT YOURSELF LIST SURGERY
;;;
;;;

;;; List surgery is the process of modifying lists by directly changing the
;;; pointers in their cons-cells. List surgery is useful in large complex
;;; programs because it is much faster to change a few pointers than to build
;;; a whole new list. This also lowers the memory required for the program,
;;; and causes the program to garbage-collect less frequently.

(defun snip (x)
  (setf (cdr x) (cdr (cdr x))))

(defvar str '(No down payment))

(defvar str-2 (cdr str))

#|

CL-USER> (snip str)
NIL
CL-USER> str
(NO)
CL-USER> str-2
(DOWN PAYMENT)

|#


;;; Create a circular structure using surgery

#|

CL-USER> (defvar circ (list 'foo))
CIRC
CL-USER> (defvar (cdr circ) circ)
; Evaluation aborted on #<SIMPLE-ERROR "The NAME argument to DEFVAR, ~S, is not a symbol." {10037D0663}>.

|#




;;;
;;;
;;; SECTION 10.8  DESCRUCTIVE OPERATIONS ON LISTS
;;;
;;;

;;; Operations that change the contents of a cons-cell are called
;;; descructive operations.

;;; 'N' stands for non-copying or non-consing

(defvar tree '(I say (e i (e i) o)))


;;;
;;; SUBSECTION 10.8.1 NCONC
;;;


;;;
;;; SUBSECTION 10.8.2  NSUBST
;;;

(nsubst 'cheery '(e i) tree :test #'equal)


;;;
;;; SUBSECTION 10.8.3  NREVERSE
;;;


;;;
;;; SUBSECTION 10.8.4  NUNION
;;;


;;;
;;; SUBSECTION 10.8.5  NINTERSECTION
;;;


;;;
;;; SUBSECTION 10.8.6  NSET-DIFFERENCE
;;;





;;;
;;;
;;; 10.9  PROGRAMMING WITH DESCTRUCTIVE OPERATIONS
;;;
;;;

(defvar *objts* '((objt-1 large green shiny cube)
		(objt-2 small red dull metal cube)
		(objt-3 red small dull plastic cube)))

(defun my-rename (objt new-name)
  (setf (car (assoc objt *objts*)) new-name))

(defun add-prop (objt prop)
  (nconc (assoc objt *objts*) (list prop)))

#|

CL-USER> *objts*
((OBJT-1 LARGE GREEN SHINY CUBE) (OBJT-2 SMALL RED DULL METAL CUBE SHARP-EDGED)
 (OBJT-3 RED SMALL DULL PLASTIC CUBE))
CL-USER> (add-prop 'objt-2 'sharp-edged)
(OBJT-2 SMALL RED DULL METAL CUBE SHARP-EDGED SHARP-EDGED)
CL-USER> *objts*

((OBJT-1 LARGE GREEN SHINY CUBE)
 (OBJT-2 SMALL RED DULL METAL CUBE SHARP-EDGED SHARP-EDGED)
 (OBJT-3 RED SMALL DULL PLASTIC CUBE))

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




;;;
;;;
;;; SECTION 10.10  SETQ and SET
;;;
;;;

;;; SETQ

;;; SETQ predates DEFVAR and can be used to assign values to ordinary variables
;;; (like X), but not to generalized variables (like (second X). Internally,
;;; most Lisp implementations turn DEFVAR into SETQ if the assignment is to a
;;; local variable. 


;;; SET

;;; SET assigns a value to the global-variable, even if a local-variable exists
;;; with the same name.


;;; SYMBOL-VALUE

;;; SYMBOL-VALUE returns the contents of the value-cell of a given symbol. 

(defvar duck 'donald)

(defun test-func-1 (dk)
  (list dk (symbol-value 'duck)))

(defun test-func-2 (dk)
  (set 'duck 'daffy)
  (list dk (symbol-value 'duck)))

#|

CL-USER> (test-func-1 'huey)
(HUEY DAFFY)
CL-USER> (test-func-2 'huey)
(HUEY DAFFY)
CL-USER> duck
DAFFY

|#


;;; However, if the variable is dynamically-scoped, then
;;; SET assigns the value to the currently accessible dynamic-variable with
;;; that name.

;;; SET comes from Lisp 1.5, the earliest dialect of Lisp. 







;;;
;;;
;;;
;;; CHAPTER 11  ITERATION AND BLOCK STRUCTURE
;;;
;;;
;;;





;;;
;;;
;;; 11.2  DOTIMES AND DOLIST
;;;
;;;

;;; DOTIMES and DOLIST are macros which do not evaluate all
;;; their arguments. 





;;;
;;;
;;; 11.3  EXITING THE BODY OF A LOOP
;;;
;;;

(defun find-first-odd (numbers)
  (dolist (e numbers)
    (format t "~&Testing ~S ..." e)
    (when (oddp e)
      (format t "~&Found an odd number!~%")
      (return e))))

#|

CL-USER> (find-first-odd '(2 4 6 8 9 3 5 7))
Testing 2 ...
Testing 4 ...
Testing 6 ...
Testing 8 ...
Testing 9 ...
Found an odd number!
9

|#


(defun check-all-odd (numbers)
  (dolist (e numbers t)
    (format t "~&Checking if ~S is odd ...~%" e)
    (if (not (oddp e)) (return NIL))))

#|

CL-USER> (check-all-odd '(1 3 5))
Checking if 1 is odd ...
Checking if 3 is odd ...
Checking if 5 is odd ...
T
CL-USER> (check-all-odd '(1 2 3))
Checking if 1 is odd ...
Checking if 2 is odd ...
NIL

|#





;;;
;;;
;;; 11.4  COMPARING RECURSIVE AND ITERATIVE SEARCH
;;;
;;;

;;; For linear lists: iteration
;;; For nested lists, trees: recursion





;;;
;;;
;;; 11.6  COMPARING DOLIST WITH MAPCAR AND RECURSION
;;;
;;;

(defun proper-it-square-list (list-of-numbers)
  (let ((result nil))
    (dolist (e list-of-numbers)
      (push (* e e) result))
    (reverse result)))

#|

CL-USER> (proper-it-square-list '(1 2 3 4 5 6))
(1 4 9 16 25 36)

|#




;;;
;;;
;;; SECTION 11.7  THE DO MACRO 
;;;
;;;

;;; DO can bind any number of variables, it can iterate through any number
;;; of index variables, and allows for a custom test to decide when to exit
;;; the loop.

(defun launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt) (format t "BLAST OFF!!!~%"))
    (format t "~S... " cnt)))

#|


|#
;10... 9... 8... 7... 6... 5... 4... 3... 2... 1... BLAST OFF!!!
;NIL


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
;;;
;;; 11.8  ADVANTAGES OF IMPLICIT ASSIGNMENT
;;;
;;;
 
(defun fact-using-do (n)
  "The factorial function implemented using the DO macro."
  ;(break "inside DO")
  (do ((cnt (- n 1) (- cnt 1))
       (prod n (* prod cnt)))
      ((= cnt 1) prod)))

#|

CL-USER> (fact-using-do 5)
120

|#


(defun find-matching-elements (x y)
  "Takes two lists and returns those elements in both lists whose positions
   match."
  (do ((l x (rest l))
       (r y (rest r)))
      ((or (null l) (null r)) NIL)
    (if (equal (first l) (first r))
	(return (first l)))))

#|

CL-USER> (find-matching-elements '(b i r d) '(c a r p e t))
R

|#





;;;
;;;
;;; 11.9  THE DO* MACRO
;;;
;;;

;;; DO* has the same syntax as DO, but it creates and updates the variables
;;; sequentially like LET*, rather than all at once like LET.

(defun ffo-with-do* (l)
  (do* ((x l (rest x))
        (e (first x) (first x)))
       ((null x) NIL)
    (if (oddp e) (return e))))

#|

CL-USER> (ffo-with-do* '())
NIL
CL-USER> (ffo-with-do* '(2))
NIL
CL-USER> (ffo-with-do* '(1))
1
CL-USER> (ffo-with-do* '(2 4 6 8 9))
9

|#


(defun check-all-odd-with-do* (l)
  (do* ((cnt 0 (+ cnt 1))
	(x l (rest x))
	(e (first x) (first x)))
       ((null x) (if (zerop cnt) NIL T))
    (if (not (oddp e)) (return NIL))))

#|

CL-USER> (check-all-odd-with-do* '())
NIL
CL-USER> (check-all-odd-with-do* '(0))
NIL
CL-USER> (check-all-odd-with-do* '(-1))
T
CL-USER> (check-all-odd-with-do* '(2))
NIL
CL-USER> (check-all-odd-with-do* '(1))
T
CL-USER> (check-all-odd-with-do* '(1 3 5 7 9))
T
CL-USER> (check-all-odd-with-do* '(1 3 5 7 8))
NIL

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

#|

CL-USER> (let ((a 1))
           (let ((a (* 2 a))
             (b (+ 2 a)))
             (list a b)))
(2 3)
CL-USER> (let ((a 1))
	   (do ((a (* 2 a))
		(b (+ 2 a)))
	       (t (list a b))))
(2 3)
CL-USER> (let ((a 1))
	   (let* ((a (* 2 a))
		  (b (+ 2 a)))
	     (list a b)))
(2 4)
CL-USER> (let ((a 1))
	   (do* ((a (* 2 a))
		 (b (+ 2 a)))
		(t (list a b))))
(2 4)

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
;;;
;;; SECTION 11.10  INFINITE LOOPS WITH DO
;;;
;;;

;;; DO can be made to loop for ever by supplying NIL as the termination test.

(defun read-a-number ()
  (do ((answer NIL))
      (NIL)
    (format t "~&Type a number: ")
    (setf answer (read))
    (if (numberp answer)
	(return answer)
	(format t "~&Sorry, ~S is not a number. Try again." answer))))

#|

CL-USER> (read-a-number)
Type a number: 6

6
CL-USER> (read-a-number)
Type a number: %

Sorry, % is not a number. Try again.
Type a number: 5

5

|#




;;;
;;;
;;; SECTION 11.11  IMPLICIT BLOCKS
;;;
;;;

;;; A block is a sequence of expressions that can be exited from at any point
;;; via the special function RETURN-FROM. The bodies of looping forms such as
;;; DOTIMES, DOLIST, DO and DO* are enclosed in implicit blocks named NIL.
;;; The expression (RETURN x) is short for (RETURN-FROM NIL x).

;;; RETURN-FROM returns from the closest enclosing box having the specified
;;; name. The arguments to RETURN-FROM are a block-name and a result-
;;; expression. The block-name is not evaluated, and so should not be quoted.

;;; Function bodies are contained in implicit blocks, and function names serve
;;; as block names.

(defun square-list (l)
  (mapcar #'(lambda (e)
	      (if (numberp e)
		  (* e e)
		  (return-from square-list 'NOT-A-NUMBER)))
	  l))

#|

CL-USER> (square-list '(1 2 three four 5 6))
NOT-A-NUMBER

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



;;; Blocks may also be defined explicitly via the special function BLOCK.
;;; Together with RETURN-FROM blocks provide a means to exit local lexical
;;; context. An inner block shadows an outer one of the same name. A block may
;;; be exited only once.

#|

CL-USER> (block empty)
NIL

CL-USER> (block outer
           (block inner 1)
             2)

CL-USER> (block outer
           (block inner
             (return-from outer 1))
2)
1

CL-USER> (block outer
           (block inner
             (let ((a 0))
               (format t "~&~S" a)))
           2)
2

|#


;;; RETURN-FROM returns from the closest enclosing block with specified name.

#|


CL-USER> (block twin
           (block twin (return-from twin 1) 2))
1

CL-USER> (block dont-care
           (values 1 2) (values 3 4))
3
4

|#




;;;
;;;
;;; SECTION 11.12  PROG1, PROG2 AND PROGN
;;;
;;;

;;; PROG1, PROG2 and PROGN take an arbitrary number of expressions as input
;;; and evaluate the expressions one at a time. PROG1 returns the value of the
;;; first expression, PROG2 returns the value of the second expression, and
;;; PROGN returns the value of the last expression.

#|

CL-USER> (defparameter prog1-x 'foo)
PROG1-X
CL-USER> (prog1
	   (setf prog1-x 'foo)
           (setf prog1-x 'bar)
           (setf prog1-x 'baz)
           (format t "~&X is ~S~%" prog1-x))
X is BAZ
FOO
CL-USER> (defparameter prog2-x 'foo)
PROG2-X
CL-USER> (prog2
	   (setf prog2-x 'foo)
	   (setf prog2-x 'bar)
	   (setf prog2-x 'baz)
	   (format t "~&X is ~S~%" prog2-x))

X is BAZ
BAR
CL-USER> (defparameter progn-x 'foo)
PROGN-X
CL-USER> (progn 
	   (setf progn-x 'foo)
	   (setf progn-x 'bar)
	   (setf progn-x 'baz)
	   (format t "~&X is ~S~%" progn-x))

X is BAZ
NIL

|#

;;; To evaluate several expressions in the true-part or false-part of an IF,
;;; enclose them in PROGN, BLOCK or a LET.

;;; The effects of PROG1 and PROG2 can be easily achieved with LET.

(defun pop-using-prog1 (x)
  (prog1
      (first x)
    (defvar x (rest x))))

(defun pop-using-let (x)
  (let ((old-top (first x)))
    (defvar x (rest x))
    old-top))

#|

CL-USER> (defvar stck '(A B C D E F))
STCK
CL-USER> (pop-using-prog1 stck)
A
CL-USER> (pop-using-let stck)
A

|#





;;;
;;;
;;; SECTION 11.13  OPTIONAL ARGUMENTS
;;;
;;;

;;; Functions can be made to accept keyword or optional, or any number of
;;; arguments by putting special symbols called lambda-list keywords in the
;;; argument list.

;;; The lambda-list keyword &OPTIONAL marks the variables following it as
;;; optional arguments. An optional argument defaults to NIL if it is left
;;; un-specified.

(defun test-optional-args (x &optional y)
  (format t "~&X is ~S~%" x)
  (format t "~&Y is ~S~%" y)
  (list x y))

#|

CL-USER> (test-optional-args 1 2)
X is 1
Y is 2
(1 2)
CL-USER> (test-optional-args 1)
X is 1
Y is NIL
(1 NIL)

|#


;;; One can specify a default value to use in place of NIL by replacing the
;;; optional argument name with a list of form (name default-value).

(defun divide-check (dividend &optional (divisor 2))
  (format t "~&~S ~A divide evenly by ~S~%"
	  dividend
	  (if (zerop (rem dividend divisor))
	      "does"
	      "does not")
	  divisor))

#|

CL-USER> (divide-check 27 3)
27 does divide evenly by 3
NIL
CL-USER> (divide-check 27)
27 does not divide evenly by 2
NIL

|#




;;;
;;;
;;; SECTION 11.14  REST ARGUMENTS
;;;
;;;

;;; The &REST lambda-list keyword is used to bind the variable following it to
;;; a list of the remaining arguments to a function. This allows a function to
;;; accept an unlimited number of arguments.

(defun avrg-using-rest-args (&rest args)
  (/ (reduce #'+ args)
     (let ((l (length args)))
       (if (zerop l) 1.0 l))
     1.0))

#|

CL-USER> (avrg-using-rest-args 1 2 3 4 5 6)
3.5
CL-USER> (avrg-using-rest-args 1)
1.0
CL-USER> (avrg-using-rest-args)
0.0

|#


;;; When a function applied to a list of &rest arguments calls itself
;;; recursively on the cdr of that list, it will be processing a list of a list
;;; rather than the original list.

(defun faulty-square-all (&rest args)
  (if (null args)
      NIL
      (cons (* (first args) (first args))
	    (faulty-square-all (cdr args)))))

#|

CL-USER> (dtrace faulty-square-all)
WARNING: Function is not TRACEd: FAULTY-SQUARE-ALL
(FAULTY-SQUARE-ALL)
CL-USER> (faulty-square-all  1 2 3 4 5 6)
----Enter FAULTY-SQUARE-ALL
|     Arg-1 = 1
|     Arg-2 = 2
|     Arg-3 = 3
|     Arg-4 = 4
|     Arg-5 = 5
|     Arg-6 = 6
|   ----Enter FAULTY-SQUARE-ALL
|   |     Arg-1 = (2 3 4 5 6); Evaluation aborted on #<TYPE-ERROR expected-type: NUMBER datum: (2 3 4 5 6)>.
; Evaluation aborted on #<TYPE-ERROR expected-type: NUMBER datum: (2 3 4 5 6)>.

The value
  (2 3 4 5 6)
is not of type
  NUMBER
when binding SB-KERNEL::X
   [Condition of type TYPE-ERROR]

|#


(defun square-all (&rest args)
  (if (null args)
      NIL
      (cons (* (first args) (first args))
	    (apply #'square-all (cdr args)))))
;SQUARE-ALL

#|

CL-USER> (trace square-all)
(SQUARE-ALL)
CL-USER> (square-all  1 2 3 4 5 6)
  0: (SQUARE-ALL 1 2 3 4 5 6)
    1: (SQUARE-ALL 2 3 4 5 6)
      2: (SQUARE-ALL 3 4 5 6)
        3: (SQUARE-ALL 4 5 6)
          4: (SQUARE-ALL 5 6)
            5: (SQUARE-ALL 6)
              6: (SQUARE-ALL)
              6: SQUARE-ALL returned NIL
            5: SQUARE-ALL returned (36)
          4: SQUARE-ALL returned (25 36)
        3: SQUARE-ALL returned (16 25 36)
      2: SQUARE-ALL returned (9 16 25 36)
    1: SQUARE-ALL returned (4 9 16 25 36)
  0: SQUARE-ALL returned (1 4 9 16 25 36)
(1 4 9 16 25 36)

|#


;;; PROG1, PROG2 and PROGN functions using &REST

(defun my-prog1 (x &rest ignore)
  x)

(defun my-prog2 (x y &rest ignore)
  y)

(defun my-progn (&rest x)
  (format t "&REST args: ~s" x)
  (car (last x)))





;;;
;;;
;;; SECTION 11.15  KEYWORD ARGUMENTS
;;;
;;;

;;; Keyword arguments are useful when a function accepts a large number of
;;; optional arguments, in that we need to remember only the names of the many
;;; arguments, and not their order. Keywords evaluate to themselves, and so
;;; do not need to be quoted.

;;; &KEY

;;; The &KEY lambda-list keyword is used to create functions that accept
;;; keyword arguments. Default values may be supplied as with &OPTIONAL.

(defun make-sundae (name &key
			   (size 'regular)
			   (ice-cream 'vanilla)
			   (syrup 'hot-fudge)
			   nuts
			   cherries
			   whipped-cream)
  (list 'Sundae
	(list 'for name)
	(list ice-cream 'with syrup 'syrup)
	(list 'toppings '=
	      (remove NIL
		      (list (and nuts 'nuts)
			    (and cherries 'cherries)
			    (and whipped-cream 'whipped-cream))))))

#|

CL-USER> (make-sundae 'Cindy
		      :syrup 'strawberry
		      :nuts T
		      :cherries T)
(SUNDAE (FOR CINDY) (VANILLA WITH STRAWBERRY SYRUP)
 (TOPPINGS = (NUTS CHERRIES)))

|#




;;;
;;;
;;; SECTION 11.16  AUXILIARY VARIABLES
;;;
;;;

;;; &AUX

;;; The &AUX lambda-list keyword is used to define auxiliary local variables.
;;; The auxiliary variable is created with an initial value of NIL if only the
;;; name is specified. If a list of form (var expr) is specified, then expr is
;;; evaluated and the variable is initialized with the result of the expr.

;;; The &AUX keyword is akin to using LET*. Both create new local variables
;;; using sequential binding. Either &AUX or LET* may be used for the same
;;; effect.

(defun avrg-with-aux-vars (&rest args
			   &aux (len (length args)))
  (/ (reduce #'+ args) len 1.0))

#|

CL-USER> (avrg-with-aux-vars 1 2 3 4 5 6)
3.5
CL-USER> (avrg-with-aux-vars 1)
1.0
CL-USER> (avrg-with-aux-vars)
; Evaluation aborted on #<DIVISION-BY-ZERO {10043FF033}>.

|#








;;;
;;;
;;;
;;; CHAPTER 12  STRUCTURES AND THE TYPE SYSTEM
;;;
;;;
;;;


;;; Lisp's type-system is:
;;;   1) visible via data-structures like symbols and lists
;;;   2) extensible via new user-defined data-structures

;;; Structures are an example of a programmer-defined datatype.




;;;
;;;
;;; SECTION 12.2  TYPEP AND TYPE-OF
;;;
;;;

;;; The TYPEP predicate returns T if an object is of the specified type.

#|

CL-USER> (typep 3 'number)
T
CL-USER> (typep 3 'float)
NIL
CL-USER> (typep 'foo 'symbol)
T

|#





;;;
;;;
;;; SECTION 12.3  DEFINING STRUCTURES
;;;
;;;

;;; STRUCTURES

;;; A structure is a programmer-defined Lisp object with an arbitrary number
;;; of named components. Structure-types become part of the Lisp
;;; type-heirarchy. They can be referenced by functions like TYPEP and
;;; TYPE-OF.

;;; Structures are displayed with the #S notation. They are not lists.

;;; DEFSTRUCT

;;; The macro DEFSTRUCT is used to define new structures and specify the names
;;; and default values of their components.

(defstruct starship
  (name NIL)
  (speed 0)
  (condition 'green)
  (shields 'down))

(defvar ss1 (make-starship))

(defvar ss2 (make-starship :name "Ent"
			   :speed 10
			   :condition 'yellow
			   :shields 'down))




;;;
;;;
;;; SECTION 12.4  TYPE PREDICATES FOR STRUCTURES
;;;
;;;

#|

CL-USER> (typep 3 'number)
T
CL-USER> (typep 3 'float)
NIL
CL-USER> (typep 'foo 'symbol)
T
CL-USER> (typep ss1 'starship)
T
CL-USER> (type-of ss1)
STARSHIP
CL-USER> (starship-p ss2)
T

|#




;;;
;;;
;;; SECTION 12.5  ACCESSING AND MODIFYING STRUCTURES
;;;
;;;

;;; DEFSTRUCT also creates accessor-functions for each component in the new
;;; structure.

#|

CL-USER> (starship-speed ss2)
10
CL-USER> (starship-shields ss1)
UP
CL-USER> (setf (starship-name ss1) "Enterprise")
"Enterprise"
CL-USER> (incf (starship-speed ss1)) 
2
CL-USER> ss1
#S(STARSHIP :NAME "Enterprise" :SPEED 2 :CONDITION YELLOW :SHIELDS UP)

|#


;;; Accessor functions can be used to write new functions to manipulate the
;;; corresponding structures.

(defun alert (x)
  (setf (starship-shields x) 'up)
  (if (equal (starship-condition x) 'green)
      (setf (starship-condition x) 'yellow))
  'shields-raised)

#|

CL-USER> (alert ss1)
SHIELDS-RAISED
CL-USER> ss1
#S(STARSHIP :NAME "Enterprise" :SPEED 2 :CONDITION YELLOW :SHIELDS UP)

|#




;;;
;;;
;;; SECTION 12.6  KEYWORD ARGUMENTS TO CONSTRUCTOR FUNCTIONS
;;;
;;;

(defvar ss4 (make-starship :name "Reliant"
			 :shields 'damaged))




;;;
;;;
;;; SECTION 12.7  CHANGING STRUCTURE DEFINITIONS
;;;
;;;

;;; Discard any current instances of a structure before changing its structure
;;; using DEFSTRUCT. Then, make new instances of the changed structure using
;;; MAKE- .


#|

CL-USER> (defstruct starship
	   (captain NIL)
	   (name NIL)
	   (shields 'down)
	   (condition 'green)
	   (speed 0))

WARNING: change in instance length of class STARSHIP:
  current length: 4
  new length: 5
; Evaluation aborted on #<SIMPLE-ERROR "~@<attempt to redefine the ~S class ~S incompatibly with the current definition~:@>" {10046656B3}>.

CL-USER> (defvar ss4 (make-starship
		      :captain "Benson"
		      :name "Reliant"
		      :shields 'damaged
		      :condition 'green
		      :speed 0))

SS4

|#


;;; We can define another structure with the same fields, in the same order as
;;; an existing one.

(defstruct starcraft
  (captain "Kirk")
  (name NIL)
  (speed 0)
  (condition 'green)
  (shields 'down))



;;;
;;; EXERCISE 12.1
;;;

;;; SKIP


;;;
;;; EXERCISE 12.2
;;;

;;; SKIP


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
;;;
;;; SECTION 12.8  PRINT FUNCTIONS FOR STRUCTURES
;;;
;;;

(defun print-starship (s strm dpth)
  "Takes a structure-name (s) a stream (strm) and a value (dpth), and prints
   the name n to strm upto nesting-depth dpth."
  (format strm "#<STARSHIP ~a>~%" (starship-name s)))

#|

CL-USER> (defparameter *ss5* (make-starship :name "Enterprise"))
*SS5*
CL-USER> (print-starship *ss5* t 0)
#<STARSHIP Enterprise>
NIL

|#


(defun print-starcraft (s strm dpth)
  "Takes a structure (s) a stream (strm) and a value (dpth), and prints
   the name n to strm upto nesting-depth dpth."
  (format strm "#<STARCRAFT ~a>~%" (starcraft-name s)))


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




;;;
;;;
;;; SECTION 12.9  EQUALITY OF STRUCTURES
;;;
;;;


;;; The EQUAL function does not treat two distinct structures as equal even if
;;; they have the same components, but the EQUALP function does.

#|

CL-USER> (defparameter *ss5* (make-starship))
*SS5*
CL-USER> *ss5*
#S(STARSHIP :NAME NIL :SPEED 0 :CONDITION GREEN :SHIELDS DOWN)
CL-USER> (defparameter *ss6* (make-starship))
*SS6*
CL-USER> *ss6*
#S(STARSHIP :NAME NIL :SPEED 0 :CONDITION GREEN :SHIELDS DOWN)
CL-USER> (equal *ss5* *ss5*)
T
CL-USER> (equal *ss5* *ss6*)
NIL
CL-USER> (equalp *ss5* *ss6*)
T

|#


;;; Also EQUALP ignores case distinctions when comparing characters, but EQUAL
;;; does not.

#|

CL-USER> (equal "enterprise" "Enterprise")
NIL
CL-USER> (equalp "enterprise" "Enterprise")
T

|#





;;;
;;;
;;; SECTION 12.10  INHERITANCE FROM OTHER STRUCTURES
;;;
;;;

;;; A structure can be made a child of another structure by using the :INCLUDE
;;; option in the defstruct.

(defstruct ship
  (name NIL)
  (captain NIL)
  (crew-size NIL))

(defstruct (battleship (:include ship))
  (weapons NIL)
  (shields NIL))

(defstruct (supplyship (:include ship))
  (cargo NIL))

#|

CL-USER> (defparameter *bs*
	   (make-battleship :captain "James T Kirk"))
*BS*
CL-USER> *bs*
#S(BATTLESHIP
   :NAME NIL
   :CAPTAIN "James T Kirk"
   :CREW-SIZE NIL
   :WEAPONS NIL
   :SHIELDS NIL)
CL-USER> (defparameter *ss7* (make-supplyship :captain "Harry Mudd"))
*SS7*
CL-USER> *ss7*
#S(SUPPLYSHIP :NAME NIL :CAPTAIN "Harry Mudd" :CREW-SIZE NIL :CARGO NIL)
CL-USER> (ship-p *bs*)
T
CL-USER> (battleship-p *bs*)
T
CL-USER> (supplyship-p *bs*)
NIL
CL-USER> (supplyship-p *ss7*)
T

|#


;;; The accessor functions of parent structures also apply to child structures

#|

CL-USER> (battleship-captain *bs*)
"James T Kirk"
CL-USER> (supplyship-captain *ss7*)
"Harry Mudd"
CL-USER> (supplyship-captain *bs*)
; Evaluation aborted on #<TYPE-ERROR expected-type: SUPPLYSHIP datum: #S(BATTLESHIP ..)>.

|#






;;;
;;;
;;;
;;; CHAPTER 13  ARRAYS, HASH TABLES AND PROPERTY LISTS
;;;
;;;
;;;



;;;
;;;
;;; SECTION 13.2  CREATING AN ARRAY
;;;
;;;

;;; An array is a contiguous block of storage whose elements are named by
;;; numerical subscripts. One dimensional arrays are called vectors.

;;; CAR and CDR do not apply to an array which is not a sequence of cons-cells.
;;; The length of an array must be fixed in advanced. Elements can be added
;;; only at one end of an array. The time to access any element of an array is
;;; the same, unlike in the case of a list. The main advantage of array over
;;; lists is efficient access. Another advantage is that an array uses only
;;; half as much memory as a list of equal length. 

(defvar my-vec '#(tuning violin 440 a))




;;;
;;;
;;; SECTION 13.3  PRINTING ARRAYS
;;;
;;;

;;; To be able to see the elements of an array, we must set the global variable
;;; *PRINT-ARRAY* to T. This assures that vectors will be printed in the same
;;; notation we use to type them in.

#|

CL-USER> *PRINT-ARRAY*
T
CL-USER> my-vec
#(TUNING VIOLIN 440 A)
CL-USER> (setf *PRINT-ARRAY* NIL)
NIL
CL-USER> my-vec
#<(SIMPLE-VECTOR 4) {10062A538F}>
CL-USER> (setf *PRINT-ARRAY* T)
T

|#





;;;
;;;
;;; SECTION 13.4  ACCESSING AND MODIFYING ARRAY ELEMENTS
;;;
;;;

;;; AREF

;;; The AREF function is used to access the elements of an array by number, as
;;; is nth to access the elements of a list. The elements of an array of length
;;; N are numbered 0 through N-1.

#|

CL-USER> (aref my-vec 2)
440
CL-USER> (defvar vec-a '#(NIL NIL NIL NIL NIL))
VEC-A
CL-USER> (setf (aref vec-a 2) 'foo)
FOO
CL-USER> (setf (aref vec-a 3) '37)
37
CL-USER> vec-a
#(NIL NIL FOO 37 NIL)
CL-USER> (length vec-a)
5
CL-USER> (find-if #'numberp vec-a)
37

|#





;;;
;;;
;;; SECTION 13.5  CREATING ARRAYS WITH MAKE-ARRAY
;;;
;;;

;;; The function MAKE-ARRAY creates and returns a new array of the specified
;;; length whose initial contents are undefined.
;;; The initial-contents keyword specifies a list of values for initialization.

#|

CL-USER> (make-array 5)
#(0 0 0 0 0)
CL-USER> (make-array 5 :initial-element 1)
#(1 1 1 1 1)
CL-USER> (make-array 5 :initial-contents '(a e i o u))
#(A E I O U)

|#





;;;
;;;
;;; SECTION 13.6  STRINGS AS VECTORS
;;;
;;;

;;; Strings are a special type of vector.

#|

CL-USER> (defvar str-s "Cockatoo")
STR-S
CL-USER> (length vec-a)
5
CL-USER> (aref str-s 5)
#\p

|#


;;; The elements of a string are called character objects. E.g. #\k denotes
;;; the character object "k". Character objects evaluate to themselves and so
;;; need not be quoted.

#|

CL-USER> #\k
#\k
CL-USER> (defvar pet "Cockatoo")
PET
CL-USER> (setf (aref pet 5) #\p)
#\p
CL-USER> pet
"Cockapoo"

|#





;;;
;;;
;;; SECTION 13.7  HASH TABLES
;;;
;;;

;;; A hash-table is functionally the same as an association-list. The advantage
;;; a hash-table offers over association-lists is that it is implemented using
;;; special hashing-algorithms that allow Lisp to look things up much faster
;;; than it can in an association-list. Hash-tables are implemented using
;;; vectors rather than lists ( which make them fast.

;;; Elements in a hash-table are grouped into buckets. The more buckets there
;;; are, the fewer entries will be assigned to the same bucket, so retrievals
;;; will be faster, at the cost of more memory.

;;; Hash-tables are to vectors what association-lists are to lists.

;;; Hash-tables can only be created using the MAKE-HASH-TABLE function. In the
;;; default kind of hash-tables EQL is used to compare the keys of items that
;;; stored. It is also possible to create hash-tables that use EQ or EQUAL.


;;; GETHASH

;;; The function GETHASH looks up a key in a hash-table. The key can be any
;;; sort of object.

;;; GETHASH is to hash-tables what AREF is to vectors.

#|

CL-USER> (defvar ht-1 (make-hash-table))
HT-1
CL-USER> (setf (gethash 'john ht-1) '(Attorney (16 Maple Drive)))
(ATTORNEY (16 MAPLE DRIVE))
CL-USER> (setf (gethash 'mary ht-1) '(Physician (25 Cedar Court)))
(PHYSICIAN (25 CEDAR COURT))
CL-USER> (gethash 'john ht-1)
(ATTORNEY (16 MAPLE DRIVE))
T

|#



;;; The second return value (T if element is found, NIL otherwise) is returned
;;; in order to distinguish an item of T or NIL that appears in the table from
;;; an item that appears or does not appear at all, respectively.

;;; DESCRIBE

;;; The DESCRIBE function is used to obtain useful information on a table, such
;;; as how many buckets (groups of entries) it has.

#|

CL-USER> (describe ht-1)
#<HASH-TABLE :TEST EQL :COUNT 2 {10062A5113}>
  [hash-table]

Occupancy: 0.1
Rehash-threshold: 1.0
Rehash-size: 1.5
Size: 16
Synchronized: no
; No value

|#





;;;
;;;
;;; SECTION 13.8  PROPERTY LISTS
;;;
;;;

;;; A property-list is a list of alternating keys (called indicators) and their
;;; values (called properties).

;;; The GET function takes an indicator and returns the corresponding property.

;;; The function symbol-plist returns a symbol's property-list.

;;; If the symbol does not have the associated property, then GET returns NIL
;;; or a specified third argument.

;;; The function remprop removes a property from a property-list.

#|

CL-USER> (setf (get 'fred 'sex) 'male)
MALE
CL-USER> (setf (get 'fred 'age) '23)
23
CL-USER> (setf (get 'fred 'siblings) '(george wanda))
(GEORGE WANDA)
CL-USER> (describe 'fred)
COMMON-LISP-USER::FRED
  [symbol]

Symbol-plist:
  AGE -> 23
  CITY -> NIL
  SIBLINGS -> (GEORGE WANDA)
  SEX -> MALE
; No value
CL-USER> (symbol-plist 'fred)
(AGE 23 CITY NIL SIBLINGS (GEORGE WANDA) SEX MALE)
CL-USER> (get 'fred 'parents)
NIL
CL-USER> (get 'fred 'parents 'unknown)
UNKNOWN
CL-USER> (remprop 'fred 'age)
(AGE 23 CITY NIL SIBLINGS (GEORGE WANDA) SEX MALE)
CL-USER> (symbol-plist 'fred)
(CITY NIL SIBLINGS (GEORGE WANDA) SEX MALE)

|#





;;;
;;;
;;; SECTION 13.9  PROGRAMMING WITH PROPERTY LISTS
;;;
;;;

;;; PUSHNEW takes a value and a list, and adds the value to the list if it is
;;; not already present in the list.

(defun add-to-property (sym elem indctr)
  "Takes a symbol (sym), a value (elem) and an indicator of a property-list 
   (indctr), and adds the value to the property corresponding to the 
   indicator."
  (pushnew elem (get sym indctr)))

(defun record-meeting (x y)
  "Takes the names of two individuals and adds them to the appropriate 
   property."
  (add-to-property x y 'has-met)
  (add-to-property y x 'has-met)
  t)

#|

CL-USER> (symbol-plist 'fred)
(CITY NIL SIBLINGS (GEORGE WANDA) SEX MALE)
CL-USER> (symbol-plist 'little-red)
(HAS-MET (WOLFIE))
CL-USER> (record-meeting 'little-red 'wolfie)
T
CL-USER> (symbol-plist 'little-red)
(HAS-MET (WOLFIE))
CL-USER> (symbol-plist 'wolfie)
(HAS-MET (LITTLE-RED))
CL-USER> (record-meeting 'wolfie 'grandma)
T
CL-USER> (symbol-plist 'wolfie)
(HAS-MET (GRANDMA LITTLE-RED))
CL-USER> (symbol-plist 'grandma)
(HAS-MET (WOLFIE))

|#


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
;;;
;;; SECTION 13.10  PROPERTY-LIST CELLS
;;;
;;;

#|

CL-USER> (setf (get 'cat-in-hat 'bowtie) 'red)
RED
CL-USER> (setf (get 'cat-in-hat 'tail) 'long)
LONG
CL-USER> (symbol-plist 'cat-in-hat)
(TAIL LONG BOWTIE RED)
CL-USER> (symbol-name 'cat-in-hat)
"CAT-IN-HAT"
CL-USER> (type-of (symbol-name 'cat-in-hat))
(SIMPLE-BASE-STRING 10)

|#






;;;
;;;
;;; SECTION 13.11  MORE ON SEQUENCES
;;;
;;;



;;;
;;; COERCE
;;;

;;; The COERCE function can be used to convert a sequence from one type to
;;; another.

#|

CL-USER> (coerce "Cockatoo" 'list)
(#\C #\o #\c #\k #\a #\t #\o #\o)
CL-USER> (coerce "Cockatoo" 'vector)
"Cockatoo"
CL-USER> (coerce '(#\b #\i #\r #\d) 'string)
"bird"
CL-USER> (coerce '(#\b #\i #\r #\d) 'vector)
#(#\b #\i #\r #\d)

|#



;;;
;;; MAKE-ARRAY
;;;

;;; We can use MAKE-ARRAY with the keyword argument :ELEMENT-TYPE to create
;;; strings, which are vectors of character-strings, a subtype of CHARACTER.

#|

CL-USER> (make-array 3
		     :element-type 'string-char
		     :initial-contents '(#\M #\o #\m))
#(#\M #\o #\m)
CL-USER> (coerce #(#\M #\o #\m) 'string)
"Mom"
CL-USER> (coerce '#(#\M #\o #\m) 'string)
"Mom"

|#



;;;
;;; MAP
;;;

;;; The MAP function works on sequences of any type, unlike MAPCAR which works
;;; only on lists.

;;; MAP takes one additional argument: the type of result.

#|

CL-USER> (map 'list #'+ '(1 2 3 4) '#(10 20 30 40))
(11 22 33 44)
CL-USER> (map #'+ '(1 2 3 4) '#(10 20 30 40))
; Evaluation aborted on #<SIMPLE-ERROR #<SB-FORMAT::FMT-CONTROL "bad thing to be a type specifier: ~/SB-IMPL:PRINT-TYPE-SPECIFIER/"> {1005887A23}>.
CL-USER> (map 'vector #'+ '(1 2 3 4) '#(10 20 30 40))
#(11 22 33 44)
CL-USER> (map 'string #'+ '(1 2 3 4) '#(10 20 30 40))
; Evaluation aborted on #<TYPE-ERROR expected-type: CHARACTER datum: 11>.
CL-USER> (map 'string #'+ '(#\1 #\2 #\3 #\4) '#(10 20 30 40))
; Evaluation aborted on #<TYPE-ERROR expected-type: NUMBER datum: #\1>.
CL-USER> (map 'string #'+ '#(1 2 3 4) '#(10 20 30 40))
; Evaluation aborted on #<TYPE-ERROR expected-type: CHARACTER datum: 11>.

|#

;;; MAP can be used to apply a function to a sequence only for its side-effect
;;; by supplying NIL as the first argument to MAP.

#|

CL-USER> (map NIL #'print "a b")

#\a 
#\  
#\b 
NIL

|#







;;;
;;;
;;;
;;; CHAPTER 14  MACROS AND COMPILATION
;;;
;;;
;;;





;;;
;;;
;;; SECTION 14.2  MACROS AS SHORTHAND
;;;
;;;

;;; Macros or macro functions are a way to extend the syntax of Lisp. Writing
;;; a macro is analogous to writing in shorthand.

;;; Anything that can be done with a macro can also be done without a macro by
;;; using a combination of ordinary functions, special functions and in some
;;; cases, implementation-dependent functions.





;;;
;;;
;;; SECTION 14.3  MACRO EXPANSION
;;;
;;;

;;; A macro is a special shorthand-expanding function which does not evaluate
;;; its arguments. A macros job is to look at its arguments and produce an
;;; expression that Lisp can evaluate. The exact expression a macro produces
;;; can be implementation-dependent.

#|

CL-USER> (defvar arg-a 1)
ARG-A
CL-USER> (ppmx (incf arg-a))
Macro expansion:
(SETQ ARG-A (+ 1 ARG-A))

; No value

|#


;;; Gensyms are internal symbols that a macro may generate which are guaranteed
;;; to not conflict with any variable-names a user may come up with.


;;;
;;; EXERCISE 14.1
;;;

#|

CL-USER> (defvar arg-x 2)
ARG-X
CL-USER> (ppmx (pop arg-x))
First step of expansion:
(PROG1 (CAR ARG-X) (SETQ ARG-X (CDR ARG-X)))

Final expansion:
(LET ((#:G590 (CAR ARG-X)))
  (SETQ ARG-X (CDR ARG-X))
  #:G590)

; No value

|#


;;;
;;; EXERCISE 14.2
;;;

#|

CL-USER> (ppmx (defstruct starship-2 (name NIL)(condition 'green)))
Macro expansion:
(PROGN
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
   (SB-KERNEL::%DEFSTRUCT-PACKAGE-LOCKS
    '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>))
 (SB-KERNEL::%DEFSTRUCT
  '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>
  '#(#<SB-KERNEL:LAYOUT for T {50300003}>
     #<SB-KERNEL:LAYOUT for STRUCTURE-OBJECT {50300083}>)
  (SB-C:SOURCE-LOCATION))
 (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
   (SB-KERNEL::%COMPILER-DEFSTRUCT
    '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>
    '#(#<SB-KERNEL:LAYOUT for T {50300003}>
       #<SB-KERNEL:LAYOUT for STRUCTURE-OBJECT {50300083}>)))
 (SB-C:XDEFUN COPY-STARSHIP-2
     :COPIER
     (SB-KERNEL:INSTANCE)
   (COPY-STRUCTURE (THE STARSHIP-2 SB-KERNEL:INSTANCE)))
 (SB-C:XDEFUN STARSHIP-2-P
     :PREDICATE
     (SB-KERNEL::OBJECT)
   (TYPEP SB-KERNEL::OBJECT 'STARSHIP-2))
 (SB-C:XDEFUN (DEFVAR STARSHIP-2-NAME)
     :ACCESSOR
     (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
   (SB-KERNEL:%INSTANCE-SET (THE STARSHIP-2 SB-KERNEL:INSTANCE) 0
                            SB-KERNEL::VALUE))
 (SB-C:XDEFUN STARSHIP-2-NAME
     :ACCESSOR
     (SB-KERNEL:INSTANCE)
   (SB-KERNEL:%INSTANCE-REF (THE STARSHIP-2 SB-KERNEL:INSTANCE) 0))
 (SB-C:XDEFUN (DEFVAR STARSHIP-2-CONDITION)
     :ACCESSOR
     (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
   (SB-KERNEL:%INSTANCE-SET (THE STARSHIP-2 SB-KERNEL:INSTANCE) 1
                            SB-KERNEL::VALUE))
 (SB-C:XDEFUN STARSHIP-2-CONDITION
     :ACCESSOR
     (SB-KERNEL:INSTANCE)
   (SB-KERNEL:%INSTANCE-REF (THE STARSHIP-2 SB-KERNEL:INSTANCE) 1))
 (SB-C:XDEFUN MAKE-STARSHIP-2
     :CONSTRUCTOR
     (&KEY ((:NAME #:NAME) NIL) ((:CONDITION #:CONDITION) 'GREEN))
   (DECLARE (SB-INT:EXPLICIT-CHECK))
   (SB-KERNEL::%MAKE-STRUCTURE-INSTANCE-MACRO
    #<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>
    '((:SLOT T . 0) (:SLOT T . 1)) #:NAME #:CONDITION))
 (SB-KERNEL::%TARGET-DEFSTRUCT
  '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP-2 {100335F753}>))

|#





;;;
;;;
;;; SECTION 14.4  DEFINING A MACRO
;;;
;;;

;;; Macros are defined with DEFMACRO, whose syntax is similar to that of DEFUN.

(defmacro simple-incf (var)
  "Takes a variable-name (var) as input, and constructs an expression for Lisp
   to increment var by 1."
  (list 'setq var (list '+ var 1)))

#|

CL-USER> (defvar arg-a-2 4)
ARG-A-2
CL-USER> (simple-incf arg-a-2)
7
CL-USER> (ppmx (simple-incf arg-a-2))
Macro expansion:
(SETQ ARG-A-2 (+ ARG-A-2 1))

; No value
CL-USER> (dtrace simple-incf)
WARNING: Function is not TRACEd: SIMPLE-INCF
(SIMPLE-INCF)
CL-USER> (simple-incf arg-a-2)
----Enter SIMPLE-INCF macro
|     Arg-1 = SIMPLE-INCF
|     Arg-2 = ARG-A-2
 \--SIMPLE-INCF expanded to (SETQ ARG-A-2 (+ ARG-A-2 1))
8

|#


;;; Unlike DTRACE, PPMX only prints the result of the macro-expansion, and
;;; does not evaluate it. 

(defmacro simple-incf-2 (var &optional (amount 1))
  (list 'setq var (list '+ var amount)))

#|

CL-USER> (defvar arg-b 2)
ARG-B
CL-USER> (simple-incf-2 arg-b (* 3 arg-a-2))
44
CL-USER> (ppmx (simple-incf-2 arg-b (* 3 arg-a-2)))
Macro expansion:
(SETQ ARG-B (+ ARG-B (* 3 ARG-A-2)))

; No value

|#


(defun faulty-incf (var)
  "INCF implemented as a function rather than as a macro."
  (setq var (+ var 1)))

#|

CL-USER> (defvar arg-a-3 7)
ARG-A-3
CL-USER> (faulty-incf arg-a-3)
8
CL-USER> (faulty-incf arg-a-3)
8
CL-USER> arg-a-3
7

|#


;;;
;;; EXERCISE 14.3
;;;

(defmacro set-nil (v)
  "Takes a variable (v) as input, and generates Lisp code that sets v to NIL."
  (list 'setq v 'NIL))

#|

CL-USER> (defvar arg-c 2)
ARG-C
CL-USER> arg-c
NIL
CL-USER> (set-nil arg-c)
NIL
CL-USER> arg-c
NIL

|#




;;;
;;;
;;; SECTION 14.5  MACROS AS SYNTACTIC EXTENSIONS
;;;
;;;

;;; Three differences between ordinary function calls and macros:

#|

    Ordinary Functions                      Macro Functions

 1. Arguments always evaluated              Arguments never evaluated

 2. Result of function may be anything at   Result of macro must be a valid  
    all.                                    Lisp expression.
 
 3. Result returned by a function is never  Expression returned by a macro is
    evaluated at all.                       immediately evaluated. 
    
|#


;;; Special Functions

;;; SETQ, IF, LET, BLOCK

;;; Special functions are the lowest-level building blocks of Common Lisp, and
;;; are responsible for things like assignment, scoping and basic control
;;; structures like blocks and loops.


;;; Three differences between special functions and macros:

#|

    Special Functions                       Macros

 1. Special functions are language          Macros are functions.
    primitives

 2. Do very special things like scoping,    Macros extend Lisp with new 
    assignment, and basic control           functionality.
    structures like blocks and loops. 

 3. Nothing that can be done using special  Anything that can be done using 
    functions can be done without using     macros can be done without using
    special functions.                      macros.

 4. The number of special operators is      The number of macros is unlimited.
    fixed. Only an implementer of Lisp can  Any user of Lisp may write new
    add new special functions, or modify    macro at any time.
    or delete existing ones.
    
 5. Special functions never evaluate their  Macros never evaluate their 
                                            arguments.

 6. A special function may return anything  A macro must always return a valid
    at all.                                 Lisp expression.
 
 7. The result returned by a special        The Lisp expression returned by a
    function is not evaluated.              macro is immediately evaluated.

|#




;;;
;;;
;;; SECTION 14.6  THE BACKQUOTE CHARACTER
;;;
;;;

;;; The backquote character provides a facility to write a template for the
;;; expression which the macro is to return.

;;; When a backquote character is used, then the macro only has to fill in the
;;; blanks.

#|

CL-USER> (defvar arg-name 'fred)
ARG-NAME
CL-USER> `(This is ,arg-name from Pittsburgh)
(THIS IS FRED FROM PITTSBURGH)
CL-USER> `(I gave ,arg-name about ,(* 25 8) dollars)
(I GAVE FRED ABOUT 200 DOLLARS)

|#


(defmacro simple-incf-3 (var &optional (amount 1))
  `(setq ,var (+ ,var ,amount)))


#|

CL-USER> (defvar arg-a 3)
ARG-A
CL-USER> (simple-incf-3 arg-a)
3
CL-USER> (ppmx (simple-incf-3 fred-loan (* 25 8)))
Macro expansion:
(SETQ FRED-LOAN (+ FRED-LOAN (* 25 8)))

; No value

|#


;;;
;;; EXERCISE 14.4
;;;

(defmacro simple-rotatef (x y)
  "Takes two variables (x) and (y), and swaps their values."
  `(let ((tmp ,x))
     (setq ,x ,y)
     (setq ,y tmp)))

(defmacro simple-rotatef-2 (x y)
  `(let ((tmp ,x)
	 (tmp-2 ,y))
     (setq ,x tmp-2)
     (setq ,y tmp)))

#|

CL-USER> (defvar arg-x-2 4)
ARG-X-2
CL-USER> arg-x-2
4
CL-USER> (defvar arg-y-2 111)
ARG-Y-2
CL-USER> arg-y-2
111
CL-USER> (simple-rotatef arg-x-2 arg-y-2)
4
CL-USER> (simple-rotatef-2 arg-x-2 arg-y-2)
111
CL-USER> arg-x-2
4
CL-USER> arg-y-2
111

|#


;;; A common use of macros is to avoid having to quote arguments. The macro
;;; expands into an ordinary function call with the quoted versions of the
;;; arguments filled in where needed. Backquoting can generate expressions with
;;; quotes by including the quotes as part of the template.

#|

CL-USER> '(defvar foo 'bar)
(DEFVAR FOO 'BAR)

|#


(defmacro two-from-one (f o)
  "Takes a function (f) and an object (o), and applies the function to two
   copies of the object."
  `(,f ',o ',o))

(defmacro two-from-one-2 (f o)
  `(,f ,o ,o))

(defmacro two-from-one-3 (f o)
  `(,f 'o 'o))

#|

CL-USER> '(defvar foo 'bar)
(DEFVAR FOO 'BAR)
CL-USER> (two-from-one cons aardvaark)
(AARDVAARK . AARDVAARK)
CL-USER> (ppmx (two-from-one cons aardvaark))
Macro expansion:
(CONS 'AARDVAARK 'AARDVAARK)

; No value
CL-USER> (two-from-one-2 cons 'aardvaark)
(AARDVAARK . AARDVAARK)
CL-USER> (ppmx (two-from-one-2 cons 'aardvaark))
Macro expansion:
(CONS 'AARDVAARK 'AARDVAARK)

; No value
CL-USER> (two-from-one-3 cons aardvaark)
(O . O)
CL-USER> (two-from-one-3 cons 'aardvaark)
(O . O)
CL-USER> (ppmx (two-from-one-3 cons aardvaark))
Macro expansion:
(CONS 'O 'O)

; No value

|#


;;;
;;; EXERCISE 14.5
;;;

(defmacro set-mutual (x y)
  "Takes two variables (x) and (y), and sets each variable to the name of the
   other."
  `(let ((name-1 ',x)
	 (name-2 ',y))
     (format t "~&set-mutual:~%")
     (format t "~&Before: x=~S, y=~S~%" ,x ,y)
     ;;(format t "~&Before: x=~S, y=~S~%" ',x ',y)
     ;;`(format t "~&Before: x=~S, y=~S~%" ',,x ',,y)     
     (defvar ,x name-2)
     (defvar ,y name-1)
     (format t "~&After : x=~S, y=~S~%" ,x ,y)
     (format t "~%")))

(defmacro set-mutual-progn (x y)
  `(progn
     (format t "~&set-mutual-progn:~%")
     (format t "~&Before: x=~S, y=~S, ',x=~S, ',y=~S~%" ,x ,y ',x ',y)
     (defvar ,x ',y)
     (defvar ,y ',x)
     (format t "~&After : x=~S, y=~S, ',x=~S, ',y=~S~%" ,x ,y ',x ',y)
     (format t "~%")))

(defun set-mutual-func (x y)
  "Takes two variables (x) and (y), and sets each variable to the name of the
   other."
  (format t "~&set-mutual-func:~%")
  (format t "~&x=~S, y=~S~%" x y)
  (format t "~%"))

#|

CL-USER> (defvar x-3 3)
X-3
CL-USER> (defvar y-3 4)
Y-3
CL-USER> (set-mutual x-3 y-3)
set-mutual:
Before: x=3, y=4
After : x=3, y=4

NIL
CL-USER> (defvar x-3 3)
X-3
CL-USER> (defvar y-3 4)
Y-3
CL-USER> (set-mutual-progn x-3 y-3)
set-mutual-progn:
Before: x=3, y=4, ',x=X-3, ',y=Y-3
After : x=3, y=4, ',x=X-3, ',y=Y-3

NIL
CL-USER> (macroexpand-1 '(set-mutual-progn x-3 y-3))
(PROGN
 (FORMAT T "~&set-mutual-progn:~%")
 (FORMAT T "~&Before: x=~S, y=~S, ',x=~S, ',y=~S~%" X-3 Y-3 'X-3 'Y-3)
 (DEFVAR X-3 'Y-3)
 (DEFVAR Y-3 'X-3)
 (FORMAT T "~&After : x=~S, y=~S, ',x=~S, ',y=~S~%" X-3 Y-3 'X-3 'Y-3)
 (FORMAT T "~%"))
T
CL-USER> (defvar x-3 3)
X-3
CL-USER> (defvar y-3 4)
Y-3
CL-USER> (set-mutual-func x-3 y-3)
set-mutual-func:
x=3, y=4

NIL

|#



#|

<pankajgodbole> (defmacro set-mutual-progn (x y)  [20:47]
<pankajgodbole>   `(progn
<pankajgodbole>      (defvar ,x ',y)
<pankajgodbole>      (defvar ,y ',x)))
<pankajgodbole> 
<pankajgodbole> this macro swaps the names of the variables  [20:48]
<dlowe> so does (rotatef x y)
<pankajgodbole> it works but I don't understand how
<thrig> (defmacro showm (expr) `(pprint (macroexpand-1 ',expr)))  might be
	handy, or such  [20:49]
<dlowe> it generates code you can see with (macroexpand-1 '(set-mutual-progn
	my-x my-y))
<pankajgodbole> ok
<pfdietz> It's assigning the names of the variables to the other variables.
								        [20:50]
<pankajgodbole> yes
<pankajgodbole> but after (defvar ,x ',y)
<pankajgodbole> ,x would be the name of the second variable   [20:51]
<pfdietz> So after (set-mutual-progn foo bar),  the variable FOO has the value
	  BAR (the symbol, not the value that the variable BAR had), and BAR
	  has the value FOO.
<pankajgodbole> which is again assigned to y
<dlowe> (macroexpand-1 '(set-mutual-progn my-x my-y)) -> (PROGN (DEFVAR MY-X
	'MY-Y) (DEFVAR MY-Y 'MY-X))
<pankajgodbole> so it would seem that y retains its original value
<White_Flame> in a macro, "x" is the _source code_ from the usage of the macro
<pankajgodbole> but in reality it doesn't. how come?
<White_Flame> look at what dlowe pasted: (defvar x 'y)  [20:52]
<White_Flame> (defvar y 'x)
<White_Flame> none of those refer to the old values of x or y
<pfdietz> Macros are working on source code, not on the values those vars will
	  have when the code is actually run.
<pfdietz> Important concept there:  macros are code that creates code.  The
	  code they are creating is not being run yet.  [20:53]
<pankajgodbole> OK  [20:55]
<White_Flame> oh, did you mean that you thought that in `(progn (defvar ... )
	      ..) that the defvar was actually being _executed_ inside the
	      macro?  it's quoted; it's just a data list containing source
	      code  [20:57]
<pankajgodbole> No, I didn't mean that  [20:58]
<pankajgodbole> I know that the macro generates the code that is run
		afterwards
<pankajgodbole> but what I don't get is how does the generated code work, in
		the absence of a temp var to hold the initial value of ',x
								        [21:01]
<pankajgodbole> before it is changed by (defvar ,x ',y)
<dlowe> the value of ',x is always the name, as a symbol, passed to the macro
								        [21:02]
<White_Flame> (defvar ,x ',y) doesn't change any value of x or y
<White_Flame> that's part of template construction
<pankajgodbole> OK. let me process that  [21:03]
<White_Flame> if you have a question about teh runtime effects, then you
	      should be asking regarding what the expansion looks like
<White_Flame> ie, (progn (defvar my-x 'my-y) (defvar my-y 'my-x))
<White_Flame> as genereted from (set-mutual-progn my-x my-y) above
<pankajgodbole> ok  [21:04]
<White_Flame> there it's pretty clear that it's simply setting variables to
	      symbol values, regardless of their old values
<White_Flame> at macro time, `( .... ,x .... ,y) is just creating a list via
	      template.  the "defvar" is just a symbol inside that list.  No
	      setting happens at that time  [21:05]
<White_Flame> (or more clearly, no DEFVARing happens at that time)
<thrig> when and how often that code runs isn't really defined
<White_Flame> is this just exploratory coding to learn, or is this macro being
	      used in something specific?  [21:07]
<pankajgodbole> this is an example from Touretsky's book  [21:12]
<pankajgodbole> an exercise actually  [21:13]
<pankajgodbole> I solved it using let and a temp variable
<pankajgodbole> but the author used progn without any temp variable  [21:14]
<White_Flame> ok "Write a macro SET-MUTUAL that takes two variable names as
	      input and expands into an expression that sets each variable to
	      the name of the other. (SET-MUTUAL A B) should set A to ’B, and
	      B to ’A."
<pankajgodbole> yep, that one
<White_Flame> even from the last line of that exercise question, doesn't it
	      read like (defvar a 'b) (defvar b 'a) ?
<White_Flame> so you know that's what you want to generate
<pankajgodbole> true  [21:15]
<pankajgodbole> it does
<White_Flame> this is really an exercise on where to put quotes, to
	      distinguish variable usage from data symbols
<White_Flame> and showing that at the source code level (ie, what macros see),
	      the core element of the symbol is used for both purposes
								        [21:16]
<White_Flame> can you pastebin your solution  for comparison?  [21:17]
<pankajgodbole> https://pastebin.com/7ZE2aqBH  [21:20]
<pankajgodbole> I was playing around with quoting, hence the format
		expressions  [21:21]
<White_Flame> yep.  And since your LET variables are only used once, and
	      perform no side effects, you could place them inline in the
	      lower DEFVARs, which would make your solution identical to the
	      canon one  [21:22]
<pankajgodbole> OK
<pankajgodbole> yes
<White_Flame> macro writing usually starts from hand-writing what the output
	      should be, and then writing a template & code that will generate
	      that output  [21:23]
<pankajgodbole> yes, I was thinking about it that way  [21:24]
<White_Flame> so if your desired output was (let ((name-1 'foo) (name-2 'bar))
	      (defvar foo name-2) (defvar bar name-1)), you're good
<White_Flame> which of course could first be shorted to (progn (defvar foo 'bar)
	      (defvar bar 'foo)), and write a macro to generate that
								        [21:25]
<pankajgodbole> right  [21:26]
<pankajgodbole> thanks for explaining this
<White_Flame> no prob
<White_Flame> I learned CL before all these good introductory books exited.
	      It was much harder back then ;)
<White_Flame> (well, before I knew about these books at least, or before they
	      became popularly recommended)  [21:27]
<pankajgodbole> that must have been a long time ago :-)
<White_Flame> I noticed this one was 1990 which yes was well before I learned
	      CL ;)
<pankajgodbole> I'm learning CL full-time now  [21:28]
<White_Flame> cool
<pjb> pankajgodbole: so, first, defvar can do multiple assignments. Instead if
      your progn, you can expand to:  `(defvar ,x ',y   ,y ',x)  [21:44]
<pjb> pankajgodbole: now, in general, if you wanted to assign the values of
      the places, you couldn't do (defvar x y y x) since then both x and y would
      be bound to the previous value  bound to y.  Instead, you can use the
      parallel defvar:  (pdefvar x y y x) which will swap the values.  As
      mentionned, rotatef does the same. (rotatef x y).  [21:45]
<pjb> pankajgodbole: there's also a (let ((x 1) (y 2) (z 3)) (shiftf x y   y z
      z (1+ x)) (list x y z)) #| --> (2 3 2) |#  [21:46]
*** schweers` (~user@p200300ECA3C96400A1B691E67C535082.dip0.t-ipconnect.de)
    has quit: Ping timeout: 268 seconds  [21:52]
<pankajgodbole> Thanks pjb  [22:19]
<pankajgodbole> I'll take a look at what you suggest
[#clschool] 

|#


(defmacro showvar-mac (x)
  "Takes a variable (x), and returns Lisp code that displays the value of x."
  `(format t "~&The value of ~S is ~S~%" ',x ,x))

(defun showvar (x y)
  (format t "~&showvar:~%")
  (showvar-mac x)
  (showvar-mac y)
  (* x y)
  (format t "~%"))

#|

CL-USER> (defvar x-3 3)
X-3
CL-USER> (defvar y-3 4)
Y-3
CL-USER> (showvar x-3 y-3)
showvar:
The value of X is 3
The value of Y is 4

NIL

|#




;;;
;;;
;;; SECTION 14.7  SPLICING WITH BACKQUOTE
;;;
;;;

;;; If a template element is preceded by a comma and an at sign (,@), then the
;;; value (which must be a list) of that element is spliced into the result
;;; that the backquote constructs rather than being inserted.

#|

CL-USER> (defvar name-3 'fred)
NAME-3
CL-USER> (defvar address '(16 maple drive))
ADDRESS

;; Inserting the value
CL-USER> `(,name-3 lives at ,address now)
(FRED LIVES AT (16 MAPLE DRIVE) NOW)

;; Splicing the value
CL-USER> `(,name-3 lives at ,@address now)
(FRED LIVES AT 16 MAPLE DRIVE NOW)

|#


(defmacro set-zero (&rest vars)
  `(progn
     ,@(mapcar #'(lambda (var) (list 'defvar var 0)) vars)
     '(Zeroed ,@vars)))

#|

CL-USER> (macroexpand-1 (set-zero a b c))
(ZEROED A B C)
NIL
CL-USER> (ppmx (set-zero a b c))
Macro expansion:
(PROGN (DEFVAR A 0) (DEFVAR B 0) (DEFVAR C 0) '(ZEROED A B C))

; No value
CL-USER> (show-macexp (set-zero a b c))

(PROGN (DEFVAR A 0) (DEFVAR B 0) (DEFVAR C 0) '(ZEROED A B C))
; No value
CL-USER> (set-zero x-3 y-3)
(ZEROED X-3 Y-3)

|#


;;;
;;; EXERCISE 14.6
;;;

(defmacro variable-chain (&rest vars)
  "Takes a number of variables (vars) as input, and generates code that sets
   each var to the name of the next one."
  `(progn
     ,@(mapcar #'(lambda (var-1 var-2) `(defvar ,var-1 ',var-2))
	       vars
	       (rotate-left vars))))

#|

CL-USER> (show-macexp (variable-chain a b c d))

(PROGN (DEFVAR A 'B) (DEFVAR B 'C) (DEFVAR C 'D) (DEFVAR D 'A))
; No value
CL-USER> (set-zero x-3 y-3 z-3)
(ZEROED X-3 Y-3 Z-3)
CL-USER> (variable-chain x-3 y-3 z-3)
Z-3
CL-USER> x-3
3
CL-USER> y-3
4

|#




;;;
;;;
;;; SECTION 14.8 THE COMPILER
;;;
;;;

;;; The compiler translates Lisp programs into machine language, resulting in a
;;; speedup of 10 to 100, and reduction in memory usage.

;;; There are two ways to use the compiler:
;;; 1) COMPILE to compile a single function
;;; 2) COMPILE-FILE to compile an entire file.

(defun tedious-sqrt (n)
  (dotimes (i n)
    (if (> (* i i) n)
	(return i))))


#|

CL-USER> (time (tedious-sqrt 500000005000000))
Evaluation took:
  0.288 seconds of real time
  0.284000 seconds of total run time (0.284000 user, 0.000000 system)
  98.61% CPU
  633,636,267 processor cycles
  0 bytes consed
  
22360680
CL-USER> (compile 'tedious-sqrt)
TEDIOUS-SQRT
NIL
NIL
CL-USER> (time (tedious-sqrt 500000005000000))
Evaluation took:
  0.273 seconds of real time
  0.268000 seconds of total run time (0.268000 user, 0.000000 system)
  98.17% CPU
  597,922,589 processor cycles
  0 bytes consed
22360680

CL-USER> (disassemble #'tedious-sqrt)
; disassembly for TEDIOUS-SQRT
; Size: 194 bytes. Origin: #x52DEA5B5
; 5B5:       498B5D40         MOV RBX, [R13+64]               ; no-arg-parsing entry point
                                                              ; thread.binding-stack-pointer
; 5B9:       48895DF8         MOV [RBP-8], RBX
; 5BD:       488B45F0         MOV RAX, [RBP-16]
; 5C1:       488945E0         MOV [RBP-32], RAX
; 5C5:       488B45E0         MOV RAX, [RBP-32]
; 5C9:       8D58F1           LEA EBX, [RAX-15]
; 5CC:       F6C301           TEST BL, 1
; 5CF:       7513             JNE L0
; 5D1:       F6C30F           TEST BL, 15
; 5D4:       0F8594000000     JNE L7
; 5DA:       8078F111         CMP BYTE PTR [RAX-15], 17
; 5DE:       0F858A000000     JNE L7
; 5E4: L0:   488B45E0         MOV RAX, [RBP-32]
; 5E8:       488945D8         MOV [RBP-40], RAX
; 5EC:       488B7DD8         MOV RDI, [RBP-40]
; 5F0:       31F6             XOR ESI, ESI
; 5F2:       48897DE8         MOV [RBP-24], RDI
; 5F6:       EB50             JMP L4
; 5F8:       0F1F840000000000 NOP
; 600: L1:   488BD6           MOV RDX, RSI
; 603:       488BFE           MOV RDI, RSI
; 606:       41807D6800       CMP BYTE PTR [R13+104], 0       ; thread.stepping
; 60B:       7402             JEQ L2
; 60D:       CC0E             BREAK 14                        ; single-step trap (before)
; 60F: L2:   488975D0         MOV [RBP-48], RSI
; 613:       E8286631FF       CALL #x52100C40                 ; GENERIC-*
; 618:       488B75D0         MOV RSI, [RBP-48]
; 61C:       488975D0         MOV [RBP-48], RSI
; 620:       488B7DF0         MOV RDI, [RBP-16]
; 624:       E8676731FF       CALL #x52100D90                 ; GENERIC->
; 629:       488B75D0         MOV RSI, [RBP-48]
; 62D:       7F3A             JNLE L6
; 62F:       488BD6           MOV RDX, RSI
; 632:       41807D6800       CMP BYTE PTR [R13+104], 0       ; thread.stepping
; 637:       7402             JEQ L3
; 639:       CC0E             BREAK 14                        ; single-step trap (before)
; 63B: L3:   BF02000000       MOV EDI, 2
; 640:       E82B6531FF       CALL #x52100B70                 ; GENERIC-+
; 645:       488BF2           MOV RSI, RDX
; 648: L4:   488BD6           MOV RDX, RSI
; 64B:       488975D0         MOV [RBP-48], RSI
; 64F:       488B7DE8         MOV RDI, [RBP-24]
; 653:       E8086731FF       CALL #x52100D60                 ; GENERIC-<
; 658:       488B75D0         MOV RSI, [RBP-48]
; 65C:       7CA2             JL L1
; 65E:       BA17001050       MOV EDX, #x50100017             ; NIL
; 663: L5:   488BE5           MOV RSP, RBP
; 666:       F8               CLC
; 667:       5D               POP RBP
; 668:       C3               RET
; 669: L6:   488BD6           MOV RDX, RSI
; 66C:       EBF5             JMP L5
; 66E: L7:   488B45E0         MOV RAX, [RBP-32]
; 672:       CC53             BREAK 83                        ; OBJECT-NOT-INTEGER-ERROR
; 674:       00               BYTE #X00                       ; RAX
; 675:       CC0F             BREAK 15                        ; Invalid argument count trap
NIL

|#




;;;
;;;
;;; SECTION 14.9  COMPILATION AND MACRO EXPANSION
;;;
;;;

;;; The Common Lisp standard permits macro calls to be replaced by their
;;; expansions at any time.

;;; Because macro expansion can happen anytime, one must not write macro that
;;; produce side-effects, such as assignments or IO. But, it is fine for the
;;; macro to expand into an expression that produces side-effects.

(defmacro bad-announce-mcr ()
  (format t "bad-announce-mcr: Hi mom!~%"))

(defun bad-say-hi-fn ()
  (bad-announce-mcr))

(defmacro good-announce-mcr ()
  `(format t "~&Hi mom!~%"))

(defun good-say-hi-fn ()
  (good-announce-mcr))


#|

CL-USER> (bad-say-hi-fn)
NIL
CL-USER> (compile 'bad-say-hi-fn)
BAD-SAY-HI-FN
NIL
NIL
CL-USER> (bad-say-hi-fn)
NIL
CL-USER> (compile 'good-say-hi-fn)
GOOD-SAY-HI-FN
NIL
NIL
CL-USER> (good-say-hi-fn)
Hi mom!
NIL

|#




;;;
;;;
;;; SECTION 14.10  COMPILING ENTIRE PROGRAMS
;;;
;;;

;;; Macro definitions must be placed earlier in the source file than any
;;; functions that reference them.

;;; Be sure to use names that don't conflict with built-in functions.




;;;
;;;
;;; SECTION 14.11  CASE STUDY: FINITE STATE MACHINES  
;;;
;;;

;;; Finite State Machines (FSMs) are a technique from theoretical computer
;;; science for describing how simple devices like vending machines work.

(defstruct (node (:print-function print-node))
  (name    NIL)
  (inputs  NIL)
  (outputs NIL))

(defun print-node (node stream depth)
  (format stream "#<Node ~A>" (node-name node)))

(defstruct (arc (:print-function print-arc))
  (from   NIL)
  (to     NIL)
  (label  NIL)
  (action NIL))

(defun print-arc (arc stream depth)
  (format stream "#<ARC ~A / ~A / ~A>" (node-name (arc-from arc))
	                               (arc-label arc)
	                               (node-name (arc-to arc))))

(defvar *nodes* NIL)
(defvar *arcs*  NIL)
(defvar *current-node* NIL)

(defun initialize-fsm ()
  "Initializes all the global variables."
  (setf *nodes* NIL)
  (setf *arcs*  NIL)
  (setf *current-node* NIL))


(defmacro defnode (name)
  "Takes the name (name) as input, and calls a function to create a node of name
   name. It is syntactic sugar for defining new nodes."
  `(add-node ',name))

(defun add-node (name)
  "Takes a name (name) as input, and creates a node with name name and adds it
   to the list of nodes held in the global variable *NODES*.
   Add-node also returns the newly created node."
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(defun find-node (n)
  "Takes a name (n) of a node, and returns the corresponding node if it exists,
   or signals an error if no node exists named n."
  (or (find n *nodes* :key #'node-name)
      (format t "~&No node exists named ~A.~%" n)))

(defmacro defarc (frm lbl to &optional actn)
  "Takes a node (frm), a name (lbl), a node (to) and an optional action
   (actn), and returns a new arc. 
   DEFARC adds the new arc to the NODE-OUTPUTS list of the frm node, and
   to the NODE-INPUTS list of the to node.
   DEFARC also adds the new arc to the list of arcs kept in the global 
   variable *ARCS*."
  `(add-arc ',frm ',lbl ',to ',actn))

(defun add-arc (frm lbl to actn)
  (let* ((from-node (find-node frm))
	 (to-node   (find-node to))
	 (new-arc (make-arc :from from-node
			    :label lbl
			    :to to-node
			    :action actn)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from-node) (nconc (node-outputs from-node)
					  (list new-arc)))
    (setf (node-inputs to-node) (nconc (node-inputs to-node)
				       (list new-arc)))
    new-arc))

(defun fsm (&optional (starting-point 'start))
  "Takes an optional input (starting-point) specifying the initial state of the
   machine, and repeatedly calls the function ONE-TRANSITION to move to the
   next state. The machine stops when it reaches a state (e.g. END) with no
   output arcs."
  (setf *current-node* (find-node starting-point))
  (do ()
      ((null (node-outputs *current-node*)))
    (one-transition)))

(defun one-transition ()
  "Prompts for an input and makes the appropriate state transition by changing
   the value of *CURRENT-NODE*. If there is no legal transition from the 
   current state for a given input, ONE-TRANSITION prints an error message and
   prompts for input again."
  (format t "~%State ~A." (node-name *current-node*))
  (format t "~%Input: ")
  (let* ((ans (read))
	 (arc (find ans (node-outputs *current-node*) :key #'arc-label)))
    (unless arc
      (format t "No arc from ~A has label ~A~%."
	      (node-name *current-node*) ans)
      (return-from one-transition NIL))
    (let ((new-node (arc-to arc)))
      (format t "~A~%" (arc-action arc))
      (setf *current-node* new-node))))

#|

CL-USER> (initialize-fsm)
NIL
CL-USER> (defnode start)
#<Node START>
CL-USER> (defnode have-5)
#<Node HAVE-5>
CL-USER> (defnode have-10)
#<Node HAVE-10>
CL-USER> (defnode have-15)
#<Node HAVE-15>
CL-USER> (defnode have-20)
#<Node HAVE-20>
CL-USER> (defnode end)
#<Node END>
CL-USER> (defarc  start    coin-return  start    "Nothing to returnDDD.~%")
#<ARC START / COIN-RETURN / START>
CL-USER> (defarc  start    nickel       have-5   "Clunk!")
#<ARC START / NICKEL / HAVE-5>
CL-USER> (defarc  start    dime         have-10  "Clink!")
#<ARC START / DIME / HAVE-10>
CL-USER> (defarc  have-5   coin-return  start    "Returned five cents.")
#<ARC HAVE-5 / COIN-RETURN / START>
CL-USER> (defarc  have-5   nickel       have-10  "Clunk!")
#<ARC HAVE-5 / NICKEL / HAVE-10>
CL-USER> (defarc  have-5   dime         have-15  "Clink!")
#<ARC HAVE-5 / DIME / HAVE-15>
CL-USER> (defarc  have-10  coin-return  start    "Returned ten cents.")
#<ARC HAVE-10 / COIN-RETURN / START>
CL-USER> (defarc  have-10  nickel       have-15  "Clunk!")
#<ARC HAVE-10 / NICKEL / HAVE-15>
CL-USER> (defarc  have-10  dime         have-20  "Clink!")
#<ARC HAVE-10 / DIME / HAVE-20>
CL-USER> (defarc  have-15  coin-return  start    "Returned fifteen cents.")
#<ARC HAVE-15 / COIN-RETURN / START>
CL-USER> (defarc  have-15  nickel       have-20  "Clunk!")
#<ARC HAVE-15 / NICKEL / HAVE-20>
CL-USER> (defarc  have-15  dime         have-20  "Clink!")
#<ARC HAVE-15 / DIME / HAVE-20>
CL-USER> (defarc  have-15  gum-button   end      "Delivered gum.")
#<ARC HAVE-15 / GUM-BUTTON / END>
CL-USER> (defarc  have-20  coin-return  start    "Returned twenty cents.")
#<ARC HAVE-20 / COIN-RETURN / START>
CL-USER> (defarc  have-20  nickel       have-20  "Nickel returned.")
#<ARC HAVE-20 / NICKEL / HAVE-20>
CL-USER> (defarc  have-20  dime         have-20  "Dime returned.")
#<ARC HAVE-20 / DIME / HAVE-20>
CL-USER> (defarc  have-20  gum-button   end      "Delivered gum and nickel change.")
#<ARC HAVE-20 / GUM-BUTTON / END>
CL-USER> (defarc  have-20  mint-button  end      "Delivered mints")
#<ARC HAVE-20 / MINT-BUTTON / END>

|#


;;;
;;; EXERCISE 14.7
;;;

#|

CL-USER> (initialize-fsm)
NIL
CL-USER> (defnode start)
#<Node START>
CL-USER> (defnode have-5)
#<Node HAVE-5>
CL-USER> (defnode have-10)
#<Node HAVE-10>
CL-USER> (defnode have-15)
#<Node HAVE-15>
CL-USER> (defnode have-20)
#<Node HAVE-20>
CL-USER> (defnode HAVE-25)
#<Node HAVE-25>
CL-USER> (defnode end)
#<Node END>

CL-USER> *nodes*
(#<Node START> #<Node HAVE-5> #<Node HAVE-10> #<Node HAVE-15> #<Node HAVE-20>
 #<Node HAVE-25> #<Node END>)

CL-USER> (defarc  start    coin-return  start    "Nothing to returnDDD.~%")
#<ARC START / COIN-RETURN / START>
CL-USER> (defarc  start    nickel       have-5   "Clunk!")
#<ARC START / NICKEL / HAVE-5>
CL-USER> (defarc  start    dime         have-10  "Clink!")
#<ARC START / DIME / HAVE-10>
CL-USER> (defarc  start    quarter      have-25  "Ker-chunk!")
#<ARC START / QUARTER / HAVE-25>

CL-USER> (defarc  have-5   coin-return  start    "Returned five cents.")
#<ARC HAVE-5 / COIN-RETURN / START>
CL-USER> (defarc  have-5   nickel       have-10  "Clunk!")
#<ARC HAVE-5 / NICKEL / HAVE-10>
CL-USER> (defarc  have-5   dime         have-15  "Clink!")
#<ARC HAVE-5 / DIME / HAVE-15>
CL-USER> (defarc  have-5   quarter      have-25  "Nickel returned.")
#<ARC HAVE-5 / QUARTER / HAVE-25>

CL-USER> (defarc  have-10  coin-return  start    "Returned ten cents.")
#<ARC HAVE-10 / COIN-RETURN / START>
CL-USER> (defarc  have-10  nickel       have-15  "Clunk!")
#<ARC HAVE-10 / NICKEL / HAVE-15>
CL-USER> (defarc  have-10  dime         have-20  "Clink!")
#<ARC HAVE-10 / DIME / HAVE-20>
CL-USER> (defarc  have-10  quarter      have-25  "Dime returned.")
#<ARC HAVE-10 / QUARTER / HAVE-25>

CL-USER> (defarc  have-15  coin-return  start    "Returned fifteen cents.")
#<ARC HAVE-15 / COIN-RETURN / START>
CL-USER> (defarc  have-15  nickel       have-20  "Clunk!")
#<ARC HAVE-15 / NICKEL / HAVE-20>
CL-USER> (defarc  have-15  dime         have-20  "Clink!")
#<ARC HAVE-15 / DIME / HAVE-20>
CL-USER> (defarc  have-15  quarter      have-25  "Nickel and dime returned.")
#<ARC HAVE-15 / QUARTER / HAVE-25>
CL-USER> (defarc  have-15  gum-button   end      "Delivered gum.")
#<ARC HAVE-15 / GUM-BUTTON / END>

CL-USER> (defarc  have-20  coin-return  start    "Returned twenty cents.")
#<ARC HAVE-20 / COIN-RETURN / START>
CL-USER> (defarc  have-20  nickel       have-20  "Nickel returned.")
#<ARC HAVE-20 / NICKEL / HAVE-20>
CL-USER> (defarc  have-20  dime         have-20  "Dime returned.")
#<ARC HAVE-20 / DIME / HAVE-20>
CL-USER> (defarc  have-20  quarter      have-25  "2 dimes returned.")
#<ARC HAVE-20 / QUARTER / HAVE-25>
CL-USER> (defarc  have-20  gum-button   end      "Delivered gum and nickel change.")
#<ARC HAVE-20 / GUM-BUTTON / END>
CL-USER> (defarc  have-20  mint-button  end      "Delivered mints")
#<ARC HAVE-20 / MINT-BUTTON / END>

CL-USER> (defarc  have-25  coin-return  start    "Returned twenty-five cents.")
#<ARC HAVE-25 / COIN-RETURN / START>
CL-USER> (defarc  have-25  nickel       have-25  "Nickel returned.")
#<ARC HAVE-25 / NICKEL / HAVE-25>
CL-USER> (defarc  have-25  dime         have-25  "Dime returned.")
#<ARC HAVE-25 / DIME / HAVE-25>
CL-USER> (defarc  have-25  gum-button   end      "Delivered gum and dime change.")
#<ARC HAVE-25 / GUM-BUTTON / END>
CL-USER> (defarc  have-25  mint-button  end      "Delivered mints and nickel change.")
#<ARC HAVE-25 / MINT-BUTTON / END>
CL-USER> (defarc  have-25  choc-button  end      "Delivered chocolate bar.")
#<ARC HAVE-25 / CHOC-BUTTON / END>

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

CL-USER> (fsm)

State START.
Input: coin-return
Nothing to return.

State START.
Input: nickel
Clunk!

State HAVE-5.
Input: nickel
Clunk!

State HAVE-10.
Input: nickel
Clunk!

State HAVE-15.
Input: nickel
Clunk!

State HAVE-20.
Input: nickel
Clunk!

State HAVE-25.
Input: nickel
Nickel returned.

State HAVE-25.
Input: choc-button
Delivered chocolate bar.
NIL
CL-USER> *current-node*
#<Node END>

|#




;;;
;;;
;;; SECTION 14.12  THE &BODY LAMBDA-LIST KEYWORD
;;;
;;;

;;; One reason to use macros is to add new bits of syntax to Lisp.

;;; The &BODY special keyword is used to indicate that the remaining arguments
;;; to a macro form the body of some control structure. &BODY is also used to
;;; signify to human readers of the macro definition that the remaining
;;; arguments are a body of Lisp code.

(defmacro while (test &body body)
  "Takes a test condition (test) and zero or more body expressions (body), 
   and generates Lisp code that emulates the WHILE construct wherein if test
   evaluates to TRUE, then Lisp evaluates body."
  `(do ()
       ((not ,test))
     ,@body))

(defun next-power-of-two (n &aux (i 1))
  "Takes a number (n) and a counter (i), and repeatedly doubles the value of i,
   starting from 1, upto the first power of two that is greater than n."
  (while (< i n)
    (format t "~&Not ~S~%" i)
    (setf i (* i 2)))
  i)

#|

CL-USER> (next-power-of-two 11)
Not 1
Not 2
Not 4
Not 8
16

|#

;;; For best style, this particular problem should be solved with DO instead of
;;; WHILE, to avoid explicit DEFVARs.




;;;
;;;
;;; SECTION 14.13  DESTRUCTURING LAMBDA LISTS
;;;
;;;

;;; Like pattern-matching

(defmacro mix-and-match (p1 p2)
  (let ((x1 (first p1))
	(y1 (second p1))
	(x2 (first p2))
	(y2 (second p2)))
    `(list '(,x1 ,y1) '(,x1 ,y2) '(,x2 ,y1) '(,x2 ,y2))))
    ;`'('(,x1 ,y1) '(,x1 ,y2) '(,x2 ,y1) '(,x2 ,y2))))

#|

CL-USER> (mix-and-match (fred wilma) (barney betty))
((FRED WILMA) (FRED BETTY) (BARNEY WILMA) (BARNEY BETTY))
CL-USER> (mix-and-match ('fred 'wilma) ('barney 'betty))
(('FRED 'WILMA) ('FRED 'BETTY) ('BARNEY 'WILMA) ('BARNEY 'BETTY))
CL-USER> (mix-and-match ('fred 'wilma) ('barney 'betty))
(('FRED 'WILMA) ('FRED 'BETTY) ('BARNEY 'WILMA) ('BARNEY 'BETTY))

|#


(defmacro dovector ((var vector-expr &optional result-form)
		    &body body)
  "Takes a variable (var) a vector (vector-expr) an &optional argument 
   (result-form) and a &body argument (body), and returns Lisp code which 
   steps var through successive elements of vector-expr."
  `(do* ((vec-dovctr  ,vector-expr)
	 (len-dovctr  (length vec-dovctr))
	 (indx-dovctr 0 (+ indx-dovctr 1))
	 (,var        NIL))
	((equal indx-dovctr len-dovctr) ,result-form)
     (setf ,var (aref vec-dovctr indx-dovctr))
     ,@body))

#|

CL-USER> (dovector (x '#(foo bar baz))
	   (format t "X is ~S~%" x))
X is FOO
X is BAR
X is BAZ
NIL

|#




;;;
;;;
;;; SECTION 14.14  MACROS AND LEXICAL SCOPING
;;;
;;;

;;; A global variable lives in the value-cell of the symbol that names it.

(defun faulty-incf-2 (v)
  "Takes a variable (v) as a symbol, and implements INCF as a function rather
   than as a macro.
   Works only on global variables, and not on locals."
  (set v (+ (symbol-value v) 1)))

(defmacro simple-incf-4 (v)
  "Takes a variable-name (v) as input, and constructs an expression for Lisp
   to increment v by 1."
  (list 'setq v (list '+ v 1)))

(defun test-simple-incf-4 (x)
  (simple-incf-4 x))

(defun test-faulty-incf (x)
  (faulty-incf-2 'x))

#|

CL-USER> (defvar arg-faulty-incf-2 7)
ARG-FAULTY-INCF-2
CL-USER> (faulty-incf-2 'arg-faulty-incf-2)
9
CL-USER> (test-simple-incf-4 37)
38
CL-USER> (test-faulty-incf 37)
; Evaluation aborted on #<TYPE-ERROR expected-type: NUMBER datum: (B C D E F)>.

|#




;;;
;;;
;;; SECTION 14.16  DYNAMIC SCOPING
;;;
;;;


;;;
;;; Lexical Scoping
;;; 

;;; Lexical scoping means that for a function to access a variable, the function
;;; must be defined within the context where the variable is defined.

;;; E.g. if a function is defined at top-level with defun, then it can access
;;; only global variables, apart from any local variables it itself defines.

;;; If a function is defined by a lambda expression appearing inside the body of
;;; another function, then it can access the enclosing function's local
;;; variables, as well as its own.

;;; Functions defined outside of a particular function cannot access any of that
;;; function's variables.

(defvar fish '(salmon tuna))    ; lexically scoped variable

(defun reference-fish ()
  "Returns the value of the global variable FISH. 
   Its parent lexical context is the global context.
   Any occurence of the variable FISH in its body is taken as a reference to the   global variable FISH since it doesn't create a local variable named FISH."
  fish)

(defun test-lexical (fish)
  "Takes an input (FISH) having the same name as the variable FISH referenced 
   by the function REFERENCE-FISH.
   Here REFERENCE-FISH cannot access the local variable FISH. The symbol FISH
   in the body of REFERENCE-FISH continues to refer to the global variable 
   FISH."
  (list fish (reference-fish)))

#|

CL-USER> (reference-fish)
(SALMON TUNA)
CL-USER> (test-lexical '(guppy minnow))
((GUPPY MINNOW) (GUPPY MINNOW))
CL-USER> (reference-fish)
(SALMON TUNA)

|#

 
;;;
;;; Dynamic Scoping
;;; 

;;; Dynamically scoped variables are called "special variables". A special
;;; variable is not local to any function -- i.e. its value will be accessible
;;; anywhere, unlike lexically scoped variables which are accessible only within
;;; the body of the form that defines them.

;;; One way to declare a variable name to be special is with the DEFVAR macro.

(defvar birds)    ; dynamically scoped variable

(defvar birds '(eagle vulture))

(defun reference-birds ()
  "Returns the value of the global variable BIRDS.
   Its parent lexical context is the global context.
   Any occurence of the variable BIRDS in its body is taken as a reference to
   the global variable BIRDS since it doesn't create a local variable named
   BIRDS."
  birds)

(defun test-dynamic (birds)
  "Takes an input (BIRDS) having the same name as the special variable BIRDS
   referenced by the function REFERENCE-BIRDS.
   Here REFERENCE-BIRDS has access to the local variable BIRDS. The global
   variable BIRDS being a special (dynamic) variable takes the value of the
   local variable BIRDS."
  (list birds (reference-birds)))

#|

CL-USER> (reference-birds)
(EAGLE VULTURE)
CL-USER> (test-dynamic '(robin sparrow))
((ROBIN SPARROW) (ROBIN SPARROW))
CL-USER> (reference-birds)
(EAGLE VULTURE)

|#




;;;
;;;
;;; SECTION 14.16  DEFVAR, DEFPARAMETER, DEFCONSTANT
;;;
;;;

;;; DEFVAR, DEFPARAMETER, DEFCONSTANT all declare names to be special.


;;; DEFVAR is used to declare variables whose values will change during the
;;; normal operation of the program.

;;; DEFVAR will assign a value to a variable only if it does not already have
;;; a value. But if the variable already has a value, then DEFVAR will not
;;; change it.


;;; DEFPARAMETER is used to declare variables whose values will not change
;;; while the program runs.

;;; DEFPARAMETER will assign a value to a variable even if it already has one.


;;; DEFCONSTANT is used to define constants, which are guaranteed never to
;;; change during the running of the program.

;;; It is an error to try to change the value of a constant, or to create a new
;;; variable with the same name as an existing constant.

;;; Declaring a quantity to be constant sometimes allows the compiler to
;;; generate more efficient machine language than it would generate if the
;;; quantity were declared to be a variable.

;;; Names declared with DEFCONSTANT need not be enclosed by asterisks.

#|

CL-USER> (defparameter *max-glasses* 500)
*MAX-GLASSES*
CL-USER> *max-glasses*
500
CL-USER> (defparameter *max-glasses* 300)
*MAX-GLASSES*
CL-USER> *max-glasses*
300

CL-USER> (defconstant speed-of-light 299792500.0)
SPEED-OF-LIGHT
CL-USER> (defvar speed-of-light 'very-high)
; Evaluation aborted on #<SIMPLE-ERROR "Cannot proclaim a ~A variable ~A: ~S" {10033D2EE3}>.

|#




;;;
;;;
;;; SECTION 14.18  REBINDING SPECIAL VARIABLES
;;;
;;;

;;; Common Lisp contains many built-in special variables.

;;; E.g. *PRINT-BASE* is used by FORMAT and other functions to determine the
;;; base in which numbers are to be printed. We can dynamically rebind
;;; *PRINT-BASE* to print numbers in other bases.

;;; *PRINT-BASE* is already declared special by DEFVAR, and so we can rebind it
;;; by merely including it in the argument list of our function.

(defun print-in-base (*print-base* x)
  (format t "~&~D is written ~S in base ~D.~%" x x *print-base*))

#|

CL-USER> (print-in-base 10 205)
205 is written 205 in base 10.
NIL
CL-USER> (print-in-base 8 205)
205 is written 315 in base 8.
NIL
CL-USER> (print-in-base 2 205)
205 is written 11001101 in base 2.
NIL

|#


;;; When a special variable is rebound, any assignments will affect the new
;;; variable, not the old one.

(defun bump-foo ()
  (incf *foo*))

(defun rebind-foo ()
  (bump-foo)
  (showvar-mac *foo*)
  (let ((*foo* 100))
    (format t "~&Enter the LET ... ~%")
    (showvar-mac *foo*)
    (incf *foo*)
    (showvar-mac *foo*)
    (bump-foo)
    (showvar-mac *foo*)
    (format t "~&Leave the LET ... ~%"))
  (bump-foo)
  (showvar-mac *foo*))

#|

CL-USER> (defvar *foo* 2)
*FOO*
CL-USER> *foo*
4

CL-USER> (showvar-mac *foo*)
The value of *FOO* is 4
NIL

CL-USER> (rebind-foo)
The value of *FOO* is 5
Enter the LET ... 
The value of *FOO* is 100
The value of *FOO* is 101
The value of *FOO* is 102
Leave the LET ... 
The value of *FOO* is 6
NIL

|#


;;; Rebinding special variables is most useful when different parts of a large
;;; program need to communicate with each other, but passing information via
;;; extra arguments to functions is impractical.





;;;
;;;
;;; KEYBOARD EXERCISES
;;;
;;;



;;;
;;; EXERCISE 3.22
;;;
 
;;; SKIP



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





;;;
;;;
;;;
;;; Lisp Toolkit
;;;
;;;
;;;




;;;
;;; ED
;;;

;;; TODO


;;;
;;; STEP
;;;

;;; TODO




;;;
;;; DOCUMENTATION and APROPOS
;;;

;;; TODO



;;;
;;; SDRAW
;;;

;;; TODO



;;;
;;; TRACE and DTRACE
;;;

(defun half (n) (* n 0.5))

(defun avrg-3 (x y)
  (+ (half x) (half y)))

(trace)
(trace half avrg-3)

(untrace)



;;;
;;; The Debugger
;;;

(defun debug-fact (n)
  (cond ((zerop n) (break "N is zero."))
	(t (* n (debug-fact (- n 1))))))



;;;
;;; DRIBBLE
;;;

;;; TODO



;;;
;;; BREAK and ERROR
;;;

;;; TODO



;;;
;;; TIME
;;;

;;; The TIME macro function tells you how long it took to evaluate an
;;; expression, and provides other information such as the number of CPU cycles
;;; the operation took, and the number of bytes consed.

;;; TIME is useful for guaging the efficiency of programs.

(defun addup (n)
  "Takes a number (n), and adds up the first n integers."
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((> i n) sum)))

(time (addup 1000000))
;Evaluation took:
;  0.013 seconds of real time
;  0.016000 seconds of total run time (0.016000 user, 0.000000 system)
;  123.08% CPU
;  28,745,376 processor cycles
;  0 bytes consed
;500000500000

(time (addup 1000000000))
;; Evaluation took:
;;   8.854 seconds of real time
;;   8.852000 seconds of total run time (8.844000 user, 0.008000 system)
;;   99.98% CPU
;;   19,430,525,994 processor cycles
;;   0 bytes consed
;500000000500000000



;;;
;;; DESCRIBE and INSPECT
;;;

;;; DESCRIBE

;;; DESCRIBE takes any kind of Lisp object as input and prints an informative
;;; description of it.

(describe T)
#||
COMMON-LISP:T
  [symbol]

T names a constant variable:
  Value: T

T names the system-class #<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>:
  Class precedence-list: T
  Direct subclasses: ARRAY, SIMD-PACK-256, SIMD-PACK, NUMBER,
                     SB-KERNEL::RANDOM-CLASS, SB-KERNEL:FDEFN,
                     SB-KERNEL:CODE-COMPONENT, WEAK-POINTER,
                     SYSTEM-AREA-POINTER, SYMBOL, CHARACTER,
                     SB-PCL::SLOT-OBJECT, SEQUENCE, STREAM, FUNCTION
  No direct slots.
|#

(describe 'cons)
#|
COMMON-LISP:CONS
  [symbol]

CONS names a compiled function:
  Lambda-list: (SB-IMPL::SE1 SB-IMPL::SE2)
  Declared type: (FUNCTION (T T) (VALUES CONS &OPTIONAL))
  Documentation:
    Return a list with SE1 as the CAR and SE2 as the CDR.
  Known attributes: flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;LIST.LISP

CONS names the built-in-class #<BUILT-IN-CLASS COMMON-LISP:CONS>:
  Class precedence-list: CONS, LIST, SEQUENCE, T
  Direct superclasses: LIST
  No subclasses.
  No direct slots.

CONS names a primitive type-specifier:
  Lambda-list: (&OPTIONAL (SB-KERNEL::CAR-TYPE-SPEC '*)
                (SB-KERNEL::CDR-TYPE-SPEC '*))
|#

(describe 7)
;7
;  [fixnum]

(describe 'defun)
#||
COMMON-LISP:DEFUN
  [symbol]

DEFUN names a macro:
  Lambda-list: (SB-IMPL::NAME SB-IMPL::LAMBDA-LIST &BODY SB-IMPL::BODY)
  Documentation:
    Define a function at top level.
  Source file: SYS:SRC;CODE;DEFBOOT.LISP
|#

(describe 'fred)
#|
COMMON-LISP-USER::FRED
  [symbol]

Symbol-plist:
  CITY -> NIL
  SIBLINGS -> (GEORGE WANDA)
  SEX -> MALE
|#

(describe ss4)
#||
#S(STARSHIP..
  [structure-object]

Slots with :INSTANCE allocation:
  CAPTAIN                        = "Benson"
  NAME                           = "Reliant"
  SHIELDS                        = DAMAGED
  CONDITION                      = GREEN
  SPEED                          = 0
|#


;;; INSPECT

;(inspect 'half)



;;;
;;;
;;; ROOM
;;;
;;;

(room)

#||

Dynamic space usage is:   66,768,128 bytes.
Immobile space usage is:  15,339,648 bytes (57,504 bytes overhead).
Read-only space usage is:          0 bytes.
Static space usage is:           432 bytes.
Control stack usage is:        8,232 bytes.
Binding stack usage is:          640 bytes.
Control and binding stack usage is for the current thread only.
Garbage collection is currently enabled.

Breakdown for dynamic space:
  15,038,672 bytes for   217,246 instance objects
  12,761,184 bytes for    99,801 simple-vector objects
  11,011,600 bytes for   688,225 cons objects
   8,971,872 bytes for    24,681 simple-character-string objects
   3,257,392 bytes for    45,928 simple-array-unsigned-byte-8 objects
   3,164,544 bytes for     4,492 simple-array-unsigned-byte-64 objects
   8,179,632 bytes for   185,776 other objects

  62,384,896 bytes for 1,266,149 dynamic objects (space total)

Breakdown for immobile space:
  13,229,376 bytes for 22,092 code objects
   1,221,840 bytes for 25,442 symbol objects
     830,160 bytes for 22,377 other objects

  15,281,376 bytes for 69,911 immobile objects (space total)

||#


(list-all-symbols-in-package 'COMMON-LISP-USER)



;;;
;;;
;;; END
;;;
;;;
