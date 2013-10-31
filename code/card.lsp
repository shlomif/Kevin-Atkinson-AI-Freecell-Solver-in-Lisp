;; card.lsp -
;; Copyright (C) 2001 by Kevin Atkinson <kevin_fc@atkinson.dhs.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
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

;;; This file contains various structures and utilies for dealing
;;; with playing cards

(provide 'card)
(require 'util)

;;
;; Constants and types
;;

(deftype card-num ()
  `(integer 0 51))

(defconstant num-suits 4)
(declaim (type (unsigned-byte 3) num-suits))
(defconstant num-ranks 13)
(declaim (type (unsigned-byte 5) num-ranks))
(defconstant num-cards 52)
(declaim (type (unsigned-byte 8) num-cards))

(defconstant ranks (num-list num-ranks :start 1)
  "Valid ranks of a card.")
(deftype rank ()
  "A valid rank of a card"
  `(integer 1 #.num-ranks))

(defconstant suits (num-list num-suits)
  "Valid suits of a card")
(deftype suit ()
  "A valid suit of a card"
  '(integer 0 #.(- #.num-suits 1)))

;;
;; Variable(s) to control how cards are printed
;;   (printing also controled my *print-pretty*)
;;

(defvar *card-format* :numeric
  "Format to print cards in.  One of :numeric :compact :standard.")
(declaim (type (member :numeric :compact :standard) *card-format*))

;;
;; Card Structure
;;

(deftype card () `(unsigned-byte 6))

(declaim (inline make-card rank gen-rank suit card-null))

(defun make-card (rank suit)
  #!(decl-type (rank rank) (suit suit))
  (the card (dpb suit (byte 2 4) rank)))

(defun rank (card)
  #!(decl-type (card card))
  (the rank (ldb (byte 4 0) card)))

(defun gen-rank (card)
  #!(decl-type ((unsigned-byte 6) card))
  (ldb (byte 4 0) card))

(defun suit (card)
  #!(decl-type (card card))
  (the suit (ldb (byte 2 4) card)))

(declaim (type card card-nil))
(defconstant card-nil #x3F)

(defun card-null (card)
  #!(decl-type (card card))
  (= card card-nil))

(declaim (inline card-lt))
(defun card-lt (c1 c2)
  #!(decl-type (card c1 c2))
  (< c1 c2))

(defun print-card (card &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (card card) (stream stream))
  (ecase *card-format*
    ((:compact :standard) (print-card-standard card stream))
    (:numeric             (print-card-numeric card stream))))

(defun print-card-standard (card &optional (stream *standard-output*))
  (declare (optimize (speed 0)))
  #!(decl-type (card card) (stream stream))
  (if (card-null card)
      (write-string (if (eq *card-format* :standard) "---" "--") stream)
    (progn
      (when (eq *card-format* :standard)
	(write-char (if (= (rank card) 10) #\1 #\Space) stream))
      (format stream "~A" (case (rank card)
			    (1  #\A) 
			    (10 #\0) 
			    (11 #\J) 
			    (12 #\Q)
			    (13 #\K)
			    (t (rank card))))
      (write-char (ecase (suit card)
		    (0 #\D)
		    (1 #\C)
		    (2 #\H)
		    (3 #\S))
		  stream))))

(defun print-card-numeric (card &optional (stream *standard-output*) d)
  (declare (ignore d))
  (declare (optimize (speed 0)))
  #!(decl-type (card card) (stream stream))
  (if (card-null card)
    (format stream "----")
    (format stream "~2D-~D" (rank card) (suit card))))

(defun read-card (stream) 
  (declare (optimize (speed 0)))
  #!(decl-type (stream stream))
  (prog (card (with-< nil))
    (skip-whitespace stream)
    (when (eq #\< (peek-char nil stream))
      (setf with-< t)
      (read-char stream))
    (setf card (make-card (read-rank stream) (read-suit stream)))
    (when with-<
      (skip-whitespace stream)
      (when (not (eq #\> (read-char stream)))
	(error "expecting >")))
    (return card)))

(defun read-rank (stream)
  (declare (optimize (speed 0)))
  #!(decl-type (stream stream))
  (skip-whitespace stream)
  (the rank (prog (c)
    (setf c (read-char stream))
    (case (char-downcase c)
      (#\k (return 13))
      (#\q (return 12))
      (#\j (return 11))
      (#\0 (return 10))
      (#\a (return 1)))
    (setf c (to-digit c))
    (when (or (/= c 1) (not (digit-char-p (peek-char nil stream))))
      (return c))
    ;; the first digit *is* 1
    (setf c (read-char stream))
    (setf c (to-digit c))
    (return (+ 10 c)))))

(defun read-suit (stream)
  (declare (optimize (speed 0)))
  #!(decl-type (stream stream))
  (skip-whitespace stream)
  (the suit (prog (c)
    (setf c (read-char stream))
    (case (char-downcase c)
      (#\d (return 0))
      (#\c (return 1))
      (#\h (return 2))
      (#\s (return 3)))
    (setf c (to-digit c))
    (return c))))

(defconstant colors '(:black :red)
  "Valid colors of a Card.")

(declaim (inline color))
(defun color (card)
  "Returns the color of a card"
  #!(decl-type (card card))
  (if (logbitp 4 card)
      :red
    :black))

;;
;; Deck Structure
;;

(defstruct (deck
             (:constructor make-deck ())
             (:print-function (lambda (o s d)
				(print-unreadable o #'print-deck s d))))
  "A deck of cards."
  (size  52
         :type (integer 0 52))
  (cards (prog* ((i 0)
                 (v (make-array 52 :element-type 'card)))
            (dolist (r ranks)
              (dolist (s suits)
                (setf (aref v i) (make-card r s))
                (incf i)))
            (return v))
         :type (svector card 52)))

(defun print-deck (deck stream)
  (declare (optimize (speed 0)))
  #!(decl-type (deck deck) (stream stream))
  (format stream "Deck ~A" (deck-size deck)))

(defun take-rand (deck)
  "Extracts a random card from a deck of cards."
  (declare (optimize (speed 0)))
  #!(decl-type (deck deck))
  (prog* ((r (random (deck-size deck)))
          (c (deck-cards deck))
          (v (aref c r)))
     (decf (deck-size deck))
     (setf (aref c r) (aref c (deck-size deck)))
     (return v)))
