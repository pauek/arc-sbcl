
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

(defun %car (x)
  (and (consp x) (car x)))

(defun %literal? (x)
  (or (eq x t) (eq x nil)
      (characterp x)
      (numberp x)))

(defun cpt (form env cc)
  (let ((head (%car form)))
    (cond ((stringp head) (copy-seq s))
	  ((literal? 