;;;; cards.lisp

(proclaim '(optimize (debug 3)))

(in-package #:cards)

(defclass hand ()
  ((cards :accessor cards
	  :type '(array card *)
	  :initarg :cards)))

(defun make-hand (cards)
  (make-instance 'hand :cards cards))