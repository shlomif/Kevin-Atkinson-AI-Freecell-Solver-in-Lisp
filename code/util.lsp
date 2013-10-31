;; util.lsp - various utility functions
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

(provide 'util)

;; The read-macro #! and macro decl-type is here because clisp
;; completly ignored type declarations.  This is a problem becuase I
;; use the type declaration as a form as error checking.  I also use
;; the type declaration to obtain faster code when compiling with
;; cmucl.  Thus decl-type macro will expand to either a declare
;; or a type check code or nothing depending on which compiler
;; is used and if the code is compiled or interpreted
;; The read macro #! is needed because macro are not allowed to
;; expand into declare statements in common lisp so the macro
;; has to be expanded at read-time.
;;
;; The proper way to use the two is to use them where ever a declare
;; is used as follows
;; #!(decl-type ((TYPE PARM+)+) where '+' means 1 or more times

(defmacro eval-always (&body body)
  `(eval-when (compile load eval) ,.body))

(eval-always
 (set-dispatch-macro-character #\# #\!
    #'(lambda (stream char1 char2)
        (declare (ignore char1 char2))
        (multiple-value-bind (x) (macroexpand-1 (read stream t nil t))
          x))))

(defmacro decl-type (&rest args)
  "Fancy type declare.  See util.lisp"
  (if (and (member :clisp *features*) (eval-when (:load-toplevel :execute) t))
    `(progn
       ,@(mapcan #'(lambda (tl)
                     (mapcar #'(lambda (var)
                                 (list 'check-type var (car tl)))
                             (cdr tl)))
                 args))
    `(declare
      ,@(mapcar #'(lambda (tl) (cons 'type tl))
                args))))

;; Curry is a standard concept used in many functional programming
;; languages.  It takes in a function and returns a new function 
;; with the provided args supplied.  For example to add 2 to each 
;; number in a list one would use:
;;  (mapcar (curry #'+ 1) lst)
;; Which is the same as
;;  (mapcar (lambda (i) (+ 1 i)) lst)

(declaim (inline curry))
(defun curry (fn &rest args)
  #!(decl-type (function fn))
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

;;(declaim (ext:constant-function num-list))
(defun num-list (count &key (start 0))
  "Returns a list of COUNT numbers in sequence starting at START."
  #!(decl-type (fixnum count start))
  (let ((end (- (+ start count) 1))
	(brk (- start 1))
        (lst nil))
    (do ((i end (- i 1)))
	((= i brk) lst)
      (declare (type fixnum i))
      (push i lst))))

(declaim (inline not-null))
(defun not-null (lst)
  (not (null lst)))

(deftype svector (type num)
  `(simple-array ,type (,num)))

(defun to-digit (char)
  "convert CHAR to an integer or error"
  #!(decl-type (character char))
  (let ((digit (digit-char-p char)))
    (when (null digit)
      (error "'~A' is not a digit" char))
    digit))

(defun skip-whitespace (stream)
  "skip over whitspace for STREAM"
  #!(decl-type (stream stream))
  (peek-char t stream)
  nil)

(defmacro ignore-error (error &body body)
  "like ignore-errors but only ignores a specific error"
  `(block error-block 
     (handler-bind ((,error (lambda (e) 
			      (declare (ignore e)) 
			      (return-from error-block))))
       ,.body)))

;;
;;
;;

(defun print-unreadable (obj fun stream d)
  (declare (optimize (speed 0)))
  (declare (ignore d))
  (cond
    (*print-pretty* (apply fun obj stream nil))
    (T              (write-string "#<" stream)
		    (apply fun obj stream nil)
		    (write-string ">" stream))))

(deftype list-of (what)
  ;; recursive deftype or not actually supported in lisp but
  ;; clisp seasms to support it
  `(or null (cons ,what list)))

