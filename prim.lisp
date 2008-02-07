
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

;;; Primitives

(defmacro defprim (name args &body body)
  (let ((_name (%sym name)))
    `(progn
       (defun ,_name ,args ,@body)
       (defparameter ,_name #',_name))))


(macrolet ((_ (name &rest args)
	     `(defprim ,name (fn ,@args)
		(if (functionp fn)
		    (funcall fn ,@args)
		    ($apply fn (list ,@args))))))
  (_ funcall0)
  (_ funcall1 a1)
  (_ funcall2 a1 a2)
  (_ funcall3 a1 a2 a3)
  (_ funcall4 a1 a2 a3 a4))

(defprim apply (fn args)
  (cond ((functionp fn) (apply fn args))
	((consp fn) (nth (car args) fn))
	((stringp fn) (char fn (car args)))
	((hash-table-p fn) (gethash (car args) fn))
	(t (error "Call to inappropriate object"))))

(macrolet ((_arith (op)
	     `(defprim ,op (&rest args)
		(apply #',op args))))
  (_arith -)
  (_arith *)
  (_arith /)
  (_arith mod)
  (_arith expt)
  (_arith sqrt))

(defprim + (&rest args)
  (cond ((null args) 0)
	((every #'stringp args)
	 (apply #'concatenate 'string args))
	((every #'listp args)
	 (apply #'append args))
	(t (apply #'+ args))))

(defprim cons (a b)
  (cons a b))

(defprim car (x)
  (cond ((null x) nil)
	((consp x) (car x))
	(t (error "Error can't take car of ~a" x))))

(defprim cdr (x)
  (cond ((null x) nil)
	((consp x) (cdr x))
	(t (error "Error can't take cdr of ~a" x))))

(defprim err (msg &rest args)
  (let ((msg (format nil "~a~{~a~^ ~}" msg args)))
    (error msg)))

