
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

(defun %car (x)
  (and (consp x) (car x)))

(defun %ecar (x)
  (if (consp x) (car x)
      (error "ecar: not a cons")))

(defun %cdr (x)
  (and (consp x) (cdr x)))

(defun %ecdr (x)
  (if (consp x) (car x)
      (error "ecdr: not a cons")))

;; Symbols

(defun %sym (sym)
  (intern (format nil "$~a" sym)
	  (find-package :arc)))

(defun arcsym (s) (%sym s))

(defun rmsym (s) 
  (unintern (%sym s) 
	    (find-package :arc)))

(defun %boundp (sym)
  (boundp (%sym sym)))

(defun %symval (sym)
  (and (%boundp sym)
       (symbol-value (%sym sym))))

(defun %set (sym val)
  (set (%sym sym) val))

(defstruct tagged type rep)

(defun %mk-tagged (type rep)
  (make-tagged :type type :rep rep))

(defun %tagged? (x) (typep x 'tagged))
(defun %type (x)    (tagged-type x))
(defun %rep (x)     (tagged-rep x))

(defun %tag? (tag x)
  (and x (%tagged? x) (eq tag (%type x))))
