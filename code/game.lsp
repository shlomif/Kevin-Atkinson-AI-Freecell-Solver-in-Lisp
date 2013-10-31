;; game.lsp -
;; Copyright (C) 2001 by Kevin Atkinson <kevin_fc@atkinson.dhs.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; version 2, as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA or go to www.gnu.org.

;; This file contains the Representation of the Freecell game
;; A "single" move is just that, the moving of a single card
;; from one location to another as oppose to a "multi" move
;; which moves several cards at once provided that there are
;; enough empty positions to support the move.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Structures and Types used in the representation of the game
;;;

(provide 'game)
(require 'util)
(require 'card)

;;
;; Constants and types
;;

(defconstant num-freecells 4)
(defconstant num-columns   8)
(defconstant num-homecells num-suits)

(deftype freecell () `card)
(deftype homecell () `(or rank (integer 0 0)))
(deftype column   () `(list-of card))

(deftype freecells () `(unsigned-byte 32))
(deftype homecells () `(unsigned-byte 16))
(deftype columns   () `(svector column   #.num-columns))

(deftype freecell-pos () `(integer 0 #.(- num-freecells 1)))
(deftype homecell-pos () `(integer 0 #.(- num-homecells 1)))
(deftype column-pos   () `(integer 0 #.(- num-columns 1)))

(deftype num-spots   () `(integer 0 #.(+ num-freecells num-homecells)))
(deftype num-in-seq  () `(integer 0 #.num-ranks))

(declaim (type freecell-pos num-freecells))
(declaim (type homecell-pos num-homecells))
(declaim (type column-pos   num-column))

(deftype num-moves () `(unsigned-byte 16))
(defconstant max-moves #xFFF0)
(declaim (type num-moves max-moves))

;;
;; Variables to control how moves and games are printed
;;   (printing also controled my *print-pretty*)
;;   (also see *card-format* in card.lsp)
;;

(defvar *move-format* :normal
  "The format to print moves and positions in.  One of :normal :compact.")
(declaim (type (member :normal :compact) *move-format*))

(defvar *column-start* 0
  "The number to start counting columns with in compact mode.")
(declaim (type (integer 0 1) *column-start*))

(defvar *game-format* :terse
  "The fromat to print the game in.  One of :terse :expanded.")
(declaim (type (member :terse :expanded) *game-format*))

;;
;; Pos struct
;;

(deftype what () `(integer #x10 #x30))
(deftype which () `(unsigned-byte 4))

(deftype pos () `(unsigned-byte 8))

(declaim (inline pos what which which-null))

(declaim (type what freecell homecell column))
(defconstant freecell #x10)
(defconstant homecell #x20)
(defconstant column #x30)

(declaim (type which which-nil))
(defconstant which-nil #x0F)

(defun pos (what &optional (which which-nil))
  #!(decl-type (what what) (which which))
  (the pos (logior what which)))
	    
(defun what (pos)
  #!(decl-type (pos pos))
  (the what (logand pos #xF0)))

(defun which (pos)
  #!(decl-type (pos pos))
  (the which (logand pos #x0F)))

(defun which-null (pos)
  #!(decl-type (pos pos))
  (= (logand pos #x0F) #x0F))

(defun print-pos (pos &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (pos pos) (stream stream))
  (ecase *move-format*
    (:normal  (print-pos-normal  pos stream))
    (:compact (print-pos-compact pos stream))))

(defun print-pos-normal (pos &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (pos pos) (stream stream))
  (write-char (ecase (what pos)
		(#.freecell #\f)
		(#.homecell #\h)
		(#.column   #\c))
	      stream)
  (format stream "~A" (if (which-null pos) #\Space (which pos)))
  nil)

(defun print-pos-compact (pos &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (pos pos) (stream stream))
  (ecase (what pos)
    (#.column (format stream "~A" (+ (which pos) *column-start*)))
    (#.freecell (if (which-null pos)
		 (write-char #\F stream)
		 (write-char (code-char (+ (which pos) 
					   (char-code #\a)))
			     stream)))
    (#.homecell (write-char #\H stream)))
  nil)

;;
;; Move Struct
;;

(deftype move () `(unsigned-byte 16))

(declaim (inline make-move move-from move-to))

(defun make-move (from to)
  #!(decl-type (pos from to))
  (the move (dpb to (byte 8 8) from)))

(defun move-from (move)
  #!(decl-type (move move))
  (the pos (ldb (byte 8 0) move)))

(defun move-to (move)
  #!(decl-type (move move))
  (the pos (ldb (byte 8 8) move)))

(defun print-move (move &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (move move) (stream stream))
  (ecase *move-format*
    (:normal  (print-move-normal  move stream))
    (:compact (print-move-compact move stream))))

(defun print-move-normal (move &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (move move) (stream stream))
  (print-pos-normal (move-from move) stream)
  (write-string "->" stream)
  (print-pos-normal (move-to move)   stream)
  t)

(defun print-move-compact (move &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (move move) (stream stream))
  (print-pos-compact (move-from move) stream)
  (print-pos-compact (move-to move)   stream)
  t)

(defun print-moves (moves &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type ((or (list-of move) game) moves) (stream stream))
  (when (typep moves 'game)
    (setf moves (moves moves)))
  (setf moves (reverse moves))
  (do ((i 1 (+ 1 i)))
      ((null moves)
       (when (= (mod i 10) 0) (write-char #\Newline stream)))
    (print-move (pop moves) stream)
    (write-char (if (= (mod i 10) 0) #\Newline #\Space) stream))
  t)

;;
;; Game Struct
;;

(defvar *game-indp-mode* :disallow)
(declaim (type (member :ignore :disallow) *game-indp-mode*))

(defstruct (game
	     (:conc-name nil)
             (:constructor make-game ())
             (:copier nil)
             (:constructor copy-game
                           (old-game
                            &aux (freecells  (freecells old-game))
                                 (homecells  (homecells old-game))
                                 (columns    (copy-seq (columns   old-game)))
                                 (cards-left (cards-left old-game))
                                 (num-moves  (num-moves  old-game))
				 (track-moves (track-moves old-game))
                                 (moves      (moves      old-game))
				 (indp-mode  (indp-mode  old-game))
				 (indp-moves (indp-moves old-game))))
	     (:print-function (lambda (game stream d 
				       &aux (*game-format* *game-format*))
				(when (not *print-pretty*)
				  (setf *game-format* :terse))
				(print-unreadable game #'print-game 
						  stream d))))
  "Standard Freecell Game"
  (freecells   #x3f3f3f3f
               :type freecells)
  (homecells   #x0000
               :type homecells)
  (columns     (make-array num-columns
			   :element-type 'column
			   :initial-element (list card-nil))
               :type columns)
  (cards-left  52
               :type (integer 0 52))
  (num-moves   0 
               :type num-moves)
  (track-moves t
	       :type boolean)
  (moves       nil 
               :type (list-of move))
  (indp-mode   *game-indp-mode*
	       :type (member :ignore :disallow))
  (indp-moves  nil
	       :type (list-of move)))

(declaim (inline freecell (setf freecell)))
(defun freecell (game n)
  #!(decl-type (game game) (freecell-pos n))
  (the freecell
    (ldb (byte 6 (* 8 n)) (freecells game))))
(defun (setf freecell) (val game n)
  #!(decl-type (freecell val) (game game) (freecell-pos n))
  (setf (freecells game)
	(dpb val (byte 6 (* 8 n)) (freecells game)))
  val)

(declaim (inline homecell (setf homecell)))
(defun homecell (game n)
  #!(decl-type (game game) (homecell-pos n))
  (the homecell
    (ldb (byte 4 (* 4 n)) (homecells game))))
(defun (setf homecell) (val game n)
  #!(decl-type (homecell val) (game game) (homecell-pos n))
  (setf (homecells game)
	(dpb val (byte 4 (* 4 n)) (homecells game)))
  val)

(defmacro column   (game n)
  `(aref (columns ,game) ,n))

(declaim (inline freecell-empty-d))
(defun freecell-empty-d (fc)
  #!(decl-type (freecell fc))
  (card-null fc))

(declaim (inline freecell-empty))
(defun freecell-empty (game i)
  #!(decl-type (game game) (freecell-pos i))
  (freecell-empty-d (freecell game i)))

(declaim (inline column-empty-d))
(defun column-empty-d (col)
  #!(decl-type (column col))
  (card-null (car col)))

(declaim (inline column-empty))
(defun column-empty (game i)
  #!(decl-type (game game) (column-pos i))
  (column-empty-d (column game i)))

(declaim (inline column-1?))
(defun column-1? (game i)
  #!(decl-type (game game) (column-pos i))
  (column-empty-d (cdr (column game i))))

(defun print-game (game &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (game game) (stream stream))
  (ecase *game-format*
    (:terse    (print-game-terse    game stream))
    (:expanded (print-game-expanded game stream))))

(defun print-game-terse (game &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (game game) (stream stream))
  (format stream "Game:~A,~A"
	  (cards-left game)
	  (num-moves game)))

(defun print-game-expanded (game &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (game game) (stream stream))
  (format stream "#M ~A~%" (num-moves game))
  (format stream "F: ")
  (dotimes (i num-freecells)
    (print-card (freecell game i) stream)
    (write-char #\Space stream))
  (write-char #\Newline stream)
  (format stream "H: ")
  (dotimes (i num-homecells)
    (print-homecell-pos game i stream)
    (write-char #\Space stream))
  (write-char #\Newline stream)
  (dotimes (i num-columns)
    (format stream "~A: " i)
    (dolist (card (cdr (reverse (column game i))))
      (print-card card stream)
      (write-char #\Space stream))
    (write-char #\Newline stream)))

(defun print-homecell-pos (game pos 
                           &optional (stream *standard-output*)
			   &aux (rank (homecell game pos)))
  (declare (optimize (speed 0)))
  #!(decl-type (game game) 
	       (homecell-pos pos) 
	       (stream stream)
	       ((or (eql 0) rank) rank))
  (print-card (if (= rank 0) card-nil (make-card rank pos)) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility Functions for Game Structures
;;;

(defun empty-freecell? (game)
  #!(decl-type (game game))
  (not-null (find-empty-freecell game)))

(defun find-empty-freecell (game)
  "Return a freecells position number that is free"
  #!(decl-type (game game))
  (the (or freecell-pos null)
    (block outer
      (unless (eql (indp-mode game) :ignore)
	(dotimes (i num-freecells)
	  (declare (type fixnum i))
	  (when (and (freecell-empty game i)
		     (not (member-if (lambda (m &aux (p (move-to m)))
				       (and (eql (what p) #.freecell)
					    (eql (which p) i)))
				     (indp-moves game))))
	    (return-from outer i))))
      (dotimes (i num-freecells)
	(declare (type fixnum i))
	(when (freecell-empty game i)
	  (return-from outer i)))
      (return-from outer nil))))

(declaim (inline card-at-pos))
(defun card-at-pos (game pos)
  "Return the card located on the top of the pile at pos"
  #!(decl-type (game game) (pos pos))
  (ecase (what pos)
    (#.freecell (freecell game (which pos)))
    (#.homecell (make-card (homecell game (which pos)) (which pos)))
    (#.column   (first (column game (which pos))))))

(declaim (inline alt-colors))
(defun alt-colors (card-1 card-2)
  #!(decl-type (card card-1 card-2))
  (not (eq (color card-1) (color card-2))))

(declaim (inline in-seq))
(defun in-seq (p1 p2)
  #!(decl-type ((or homecell card) p1 p2))
  (= (+ (gen-rank p1) 1) (gen-rank p2)))

(declaim (inline in-seq-alt))
(defun in-seq-alt (p1 p2)
  "Returns true of p1 and p2 are in seq. and of alt. colors"
  #!(decl-type (card p1 p2))
  (or (card-null p1)
      (card-null p2)
      (and (alt-colors p1 p2)
	   (in-seq     p1 p2))))

(defun num-in-seq-alt (game col)
  "Returns the number of cards in seq for a column"
  #!(decl-type (game game) (column-pos col))
  (do ((lst (column game col)) 
       (num 0 (+ 1 num)) 
       (prev nil (pop lst)))
      ((or (column-empty-d lst) 
	   (not (in-seq-alt prev (car lst))))
       num)
    (declare (type (integer 0 13) num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to deal with keeping track of independent moves
;;;

(defun remove-affected-indp (game from to)
  #!(decl-type (game game) (pos from to))
  (setf (indp-moves game)
	(remove-if (lambda (move)
		     (or (equalp from (move-from move))
			 (equalp from (move-to move))
			 (equalp to   (move-from move))
			 (equalp to   (move-to move))))
		   (indp-moves game))))

(defun duplicate-effort (game from to)
  "Return the previous move that the current move is duplicating effort for"
  (declare (ignore to))
  #!(decl-type (game game) (pos from))
  (find-if (lambda (move)
	     (equalp (move-to move) from))
	   (indp-moves game)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Funtions to move a single card
;;;

(defun make-single-move (game from to)
  #!(decl-type (game game) (pos from to))
  (cond
    ((not (valid-single-move game from to))
     (error "Invalid Move"))
    ((not (makes-progress game from to))
     (error "Useless Move"))
    (t
     (unsafe-make-single-move game from to))))

(defun unsafe-make-single-move (game from to)
  "Make a move without first checking if it valid."
  #!(decl-type (game game) (pos from to))
  (prog (move)
     (setf to (push-card (pop-card game from) game to))
     (setf move (make-move from to))
     (when (track-moves game)
       (push move (moves game))
       (incf (num-moves game)))
     (unless (eql (indp-mode game) :ignore)
       (remove-affected-indp game from to)
       (push move (indp-moves game)))
     (when (eq (what to) #.homecell)
       (decf (cards-left game)))
     (return move)))

(defun push-card (card game pos)
  "Push a card onto a pile at pos."
  ;; It is assumed that the move is legal.  I.e. it does no checking
  ;; of such.  It will modify pos so it accuretly reflects where the
  ;; card was plased.  Will also return that pos for convenience
  #!(decl-type (game game) (pos pos) (card card)) 
  (ecase (what pos)
    (#.freecell (when (which-null pos)
		  (setf pos (pos #.freecell (find-empty-freecell game))))
		(setf (freecell game (which pos)) card))
    (#.homecell (setf (homecell game (suit card)) (rank card)))
    (#.column   (push card (column game (which pos)))))
  pos)

(defun pop-card  (game pos)
  "Pop the card on the top of the pile at pos."
  #!(decl-type (game game) (pos pos))
  (ecase (what pos)
    (#.freecell (prog (card) 
		  (setf card (freecell game (which pos)))
                  (setf (freecell game (which pos)) card-nil)
                  (return card)))
    (#.column   (pop (column game (which pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to determine if a particular single move is valid
;;;

(defun valid-single-move (game from to)
  #!(decl-type (game game) (pos from to))
  (prog ((card-to-move (card-at-pos game from)))
     (when (card-null (card-at-pos game from))
       (return nil))
     (ecase (what to)
       (#.freecell (return (empty-freecell? game)))
       (#.homecell (return (in-seq (homecell game (suit card-to-move))
				  card-to-move)))
       (#.column   (let ((card-to-cover (card-at-pos game to)))
		     (return (in-seq-alt card-to-move card-to-cover)))))))

(defun makes-progress (game from to)
  "Returns true if the move actually does something useful."
  #!(decl-type (game game) (pos from to))

  (cond ((and (eq #.freecell (what from)) (eq #.freecell (what to))) 
	 nil)

	((and (eq #.column (what from)) (eq #.column (what to))
	      (column-1? game (which from)) (column-empty game (which to)))
	 nil)
	
	((eq #.homecell (what to))
	 T)
	
	((eq nil (moves game))
	 T)
	
	((let* ((last-move (car (moves game)))
		(last-from (move-from last-move))
		(last-to   (move-to   last-move)))
	   (and (= from last-to) (= to last-from)))
	 nil)
	
	((and (eql (indp-mode game) :disallow)
	      (duplicate-effort game from to))
	 nil)
	
	(t
	 T)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to determine possible single moves
;;;

(defun valid-single-moves (game &aux moves)
  #!(decl-type (game game))
  (flet ((try-move (from to)
	   #!(decl-type (pos from to))
	   (if (and (valid-single-move game from to)
		    (makes-progress game from to))
	       (push (make-move from to) moves))))
    (dotimes (i num-freecells)
      (try-move (pos #.freecell i) (pos #.homecell))
      (dotimes (j num-columns)
	(try-move (pos #.freecell i) (pos #.column j))))
    (dotimes (i num-columns)
      (try-move (pos #.column i) (pos #.homecell))
      (try-move (pos #.column i) (pos #.freecell))
      (dotimes (j num-columns)
	(try-move (pos #.column i) (pos #.column j))))
    (sort moves (lambda (x y) (better-move game x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to compare if one move is better than the other
;;;

(defun better-move (game lhs rhs)
  #!(decl-type (game game) (move rhs lhs))
  (block outer
    (flet ((prefer (test)
             #!(decl-type ((function (move) boolean) test))
             (let ((lhs-t (funcall test lhs))
                   (rhs-t (funcall test rhs)))
               (cond
                 ((and lhs-t (not rhs-t)) (return-from outer t))
                 ((and rhs-t (not lhs-t)) (return-from outer nil))
                 (T                         nil))))
           (col-better (p1 p2)
             #!(decl-type (pos p1 p2))
             (unless (and (eq (what p1) #.column)
                          (eq (what p2) #.column))
               (return-from col-better))
             (multiple-value-bind (c1h c1l)
                 (closest-homecell game (column game (which p1)))
               (multiple-value-bind (c2h c2l)
                   (closest-homecell game (column game (which p2)))
                 (when (< c1h c2h) (return-from outer t))
                 (when (> c1h c2h) (return-from outer nil))
                 (when (< c1l c2l) (return-from outer t))
                 (when (> c1l c2l) (return-from outer nil))))))
      (declare (inline prefer))

      ;; move to homecell
      (prefer (lambda (m) (eq (what (move-to m)) #.homecell)))
      ;; king to empty column
      (prefer (lambda (m) (and (eq (what (move-to m)) #.column)
                               (column-empty game (which (move-to m)))
                               (= (rank (card-at-pos game (move-from m)))
                                  13))))
      ;; from column with closest homecell or shortest length
      (col-better (move-from lhs) (move-from rhs))
      ;; to empty column if another card ready to move on top
      (prefer (lambda (m)
		(and (eq (what (move-to m)) #.column)
		     (column-empty game (which (move-to m)))
		     (find-next-in-seq game (move-from m)))))
      ;; to column with the reverse of prev column test
      (col-better (move-to rhs) (move-to lhs))
      ;; from freecell
      (prefer (lambda (m) (eq (what (move-from m)) #.freecell)))
      ;; from column of length 1
      (prefer (lambda (m)
		(and (eq (what (move-from m)) #.column)
		     (column-1? game (which (move-from m)))
		     (not (find-next-in-seq game (move-from m))))))
      ;; to column
      (prefer (lambda (m) (and (eq (what (move-to m)) #.column)
			       (not (column-empty game (which (move-to m)))))))
      ;; to freecell
      (prefer (lambda (m) (eq (what (move-to m)) #.freecell)))
      ;; to empty column (all that is left)
      (return-from outer nil))))

(defun find-next-in-seq (game pos)
  #!(decl-type (game game) (pos pos))
  (let ((c (card-at-pos game pos)))
    (dotimes (i num-freecells)
      (unless (freecell-empty game i)
	(when (in-seq-alt (freecell game i) c)
	  (return-from find-next-in-seq (pos #.freecell i)))))
    (dotimes (i num-columns)
      (unless (column-empty game i)
	(let ((col (column game i)))
	  (when (and (eq (what pos) #.column)
		     (= (which pos) i))
	    (pop col))
	  (unless (column-empty-d col)
	    (when (in-seq-alt (car col) c)
	      (return-from find-next-in-seq (pos #.column i)))))))))

(defun closest-homecell (game col)
  "Returns the number of cards up the homecell card is or 20 otherwise"
  #!(decl-type (game game) (column col))
  (do ((c nil)
       (i 0 (+ 1 i))
       (h 19))
      ((column-empty-d col) (values h i))
    (declare (type (integer 0 20) i h))
    (setf c (car col))
    (pop col)
    (when (and (= h 19) (in-seq (homecell game (suit c)) c))
      (setf h (if (= i 0) 20 i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to compare two games for equality
;;;

(defstruct game-key
  (columns   (abort) :type columns)
  (homecells (abort) :type homecells))

(defun game-key (game)
  (make-game-key 
   :columns (sort (copy-seq (columns game)) #'card-lt :key #'car)
   ;;:columns (columns game)
   :homecells (homecells game)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to start an actual game
;;;

(defun deal (&optional (deck (make-deck)) (game (make-game)))
  "Deal a new Random Freecell Game"
  (declare (optimize (speed 0)))
  #!(decl-type (game game) (deck deck))
  (do ((i 0 (mod (+ i 1) num-columns)))
      ((= (deck-size deck) 0))
    (push (take-rand deck) (column game i)))
  game)

(defun print-deal (game &optional (stream *standard-input*))
  (declare (optimize (speed 0)))
  #!(decl-type (game game) (stream stream))
  (when (moves game)
    (error "Can't print the beginning deal of a game if moves have been made."))
  (let ((*card-format* :compact))
    (dotimes (i num-columns)
      (dolist (card (cdr (reverse (column game i))))
	(print-card card stream)
	(write-char #\Space stream))
      (write-char #\Newline stream))))

(defun read-deal (&optional (stream *standard-input*) (game (make-game)))
  (declare (optimize (speed 0)))
  #!(decl-type (stream stream))
  (dotimes (i num-columns)
    (let ((line-in (make-string-input-stream (read-line stream))))
      (ignore-error end-of-file
	(do (card) (nil)
	  (setf card (read-card line-in))
	  (push card (column game i))))))
  game)

(defun microsoft (num)
  "Gets a microsoft deal"
  (declare (optimize (speed 0)))
  #!(decl-type (fixnum num))
  (let* ((p (run-program "/home/grad5/kevina1/ai-proj/bin/microsoft"
			(list (format nil "~A" num))
			:output :stream))
	 (g (read-deal (process-output p))))
    (process-close p)
    g))

;;
;;
;;

(defun fc-solve (game &optional (search :df-opt))
  "Solves a deal using fc-solve"
  (declare (optimize (speed 0)))
  (let ((p (run-program "/home/grad5/kevina1/ai-proj/bin/fc-solve"
			(append '("-m" "-sn")
				(ecase search
				  (:df-opt '("-me" "dfs" "-opt"))
				  (:df     '("-me" "dfs"))
				  (:a-star '("-me" "a-star"))))
			:input :stream :output :stream :wait nil))
	(c 0))
    (print-deal game (process-input p))
    (close (process-input p))
    (when (eql (peek-char t (process-output p)) #\-)
      (read-line (process-output p))
      (read-line (process-output p))
      (do ((line nil))
	  ((equalp line ""))
	(setf line (read-line (process-output p) nil nil))
	(setf c (+ c (count #\space line) 1))
	(format t "~A~%" line))
      (format t "~A Moves~%~%" c))
    (do ((line t))
	((null line))
      (setf line (read-line (process-output p) nil nil))
      (when line
	(format t "~A~%" line)))
    (process-close p))
  nil)

