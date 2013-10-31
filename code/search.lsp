;; search.lsp - the actual code to play a game.
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


(provide 'search)
(require 'game)

(declaim (inline est))
(defun est (game)
  #!(decl-type (game game))
  (+ (cards-left game)))

;;
;; df-search
;;

(defstruct result
  (num-moves     max-moves		; num moves till this one
		 :type num-moves)
  (lp-moves-left 0			; least possible moves left
		 :type num-moves)
  (est           0
		 :type num-moves)
  (old           nil
		 :type boolean))

(deftype depth () `num-moves)

(defun df-search (start-game

		  &key

		  (limit 150)
		  (jump-when 20000)
		  (jump-scale 0.90)
		  (give-up-after nil)
		  (after-sol (* jump-when
				(+ (expt 2 5) 2)))
		  ;; set give-up-after amd after-sol to nil
		  ;; to let the search run to completion

		  &aux

		  (visited (make-hash-table :test #'equalp))
		  (count 0)
		  
		  (key nil)
		  (moves nil)
		  
		  #|(last-jump-depth 0)
		  (tally-old (make-array limit
					 :element-type 'fixnum
					 :initial-element 0))
		  (tally-new (make-array limit
					 :element-type 'fixnum
					 :initial-element 0))|#
		  
		  (best-sol nil)
		  (best-sol-num-moves limit)
		
		  (since-last 0) ; count since last solution or jump
		  (prev-jumps '(0))
		  (jump-to    nil))
  
  #!(decl-type (game start-game)
	       (depth limit)
	       (fixnum jump-when)
	       (single-float jump-scale)
	       ((or null fixnum) give-up-after after-sol)
	       ((list-of fixnum) prev-jumps)
	       (fixnum count)
	       #|(depth last-jump-depth)|#
	       ((or null game) best-sol)
	       (num-moves best-sol-num-moves)
	       (fixnum since-last)
	       
	       ((or null depth) jump-to))
  (labels
      ((search0 (game depth &aux result)
	 #!(decl-type (game game) (depth depth))

	 (incf count)
	 (incf since-last)

	 (setf key (game-key game))
	 
	 (let* ((prev (gethash key visited)))
	   (cond 
	     ((null prev)
	      (setf result (make-result :num-moves (num-moves game)
					:lp-moves-left (est game))))
	      ((< (num-moves game) (result-num-moves prev))
	       (setf (result-num-moves prev) (num-moves game))
	       (setf result prev)
	       (remhash key visited))
	      (t
	       #|(let ((i (- depth last-jump-depth)))
		 (when (< i 0)
		   (setf i 0))
		 (if (result-old prev)
		     (incf (aref tally-old i))
		   (incf (aref tally-new i)))
		 (setf (result-old prev) nil))|#
	       (setf result prev)
	       (return-from search0 max-moves))))
	 
	 ;; found a solution
	 (when (= (cards-left game) 0)
	   (setf (result-lp-moves-left result) 0)
	   
	   (setf best-sol game)
	   (setf best-sol-num-moves (num-moves game))

	   (setf since-last 0)
	   (setf prev-jumps '(0))

	   (when after-sol
	     (setf give-up-after after-sol)
	     (setf count 0))
	   
	   (format t "*** Found a sol with ~A moves. *** " (num-moves game))
	   (let ((c 0))
	     (declare (type fixnum c))
	     (maphash (lambda (k v)
			(when (<= (num-moves game)
				  (+ (result-num-moves v) (result-est v)))
			  (remhash k visited)
			  (incf c)))
		      visited)
	     (format t "Purged ~A states.~%" c))
	   (return-from search0 (result-lp-moves-left result)))

	 ;; give up
	 (when (and give-up-after (> count give-up-after))
	   (return-from search0 (result-lp-moves-left result)))

	 ;; if this solution is not going to possible be better
	 ;; there is no point is constantly trying
	 (when (>= (+ (num-moves game) (result-lp-moves-left result))
		   best-sol-num-moves)
	   (return-from search0 (result-lp-moves-left result)))

	 ;; If a solution has not been found in x number of moves
	 ;; jump up some in the stack
	 (when (> since-last jump-when)
	   (let ((fac 1) (scale 0.0))
	     (declare (type fixnum fac) (type single-float scale))

	     (do ((f 1 (* f 2)))
		 ((/= (the fixnum (car prev-jumps)) f)
		  (setf fac f))
	       (declare (type fixnum f))
	       (pop prev-jumps))
	     (setf scale (expt jump-scale fac))
	     (setf jump-to (truncate (* depth scale)))
	     (unless (= jump-to 0)
	       (push fac prev-jumps))
	     
	     (setf since-last 0)
	     #|(setf last-jump-depth jump-to)|#
	     (maphash (lambda (k v)
			(declare (ignore k))
			(setf (result-old v) t))
		      visited)

	     (format t ">> Count: ~A, Hash Size: ~A~%"
		     count (hash-table-count visited))
	     (format t "Jumping To ~A from ~A, Scale: ~A, ~A ... "
		     jump-to depth scale prev-jumps)
	     (return-from search0 (result-lp-moves-left result))))
	 
	 (setf moves (valid-single-moves game))

	 (when moves
	   (setf (result-est result) (est game))
	   (setf (gethash key visited) result))

	 ;; try possibe moves
	 (let ((local-min max-moves))
	   (dolist (move moves)
	     (let ((new-game (copy-game game)))
	       (unsafe-make-single-move new-game
					(move-from move) (move-to move))
	       (let ((min (search0 new-game (+ 1 depth))))
		 (declare (type num-moves min))
		 (incf min)
		 (when (< min local-min)
		   (setf local-min min))
		 (when jump-to
		   (cond
		     ((<= depth jump-to)
		      (format t " Done~%" depth)
		      (setf jump-to nil))
		     (t
		      (return)))))))
	   (setf (result-lp-moves-left result) local-min))

	 (return-from search0 (result-lp-moves-left result))))
    
    (search0 start-game 0)
    #|(dotimes (x limit)
      (format t "~3@A: ~6@A ~6@A~%" x (aref tally-old x) (aref tally-new x)))|#
    (values best-sol #|tally-old tally-new|#)))

