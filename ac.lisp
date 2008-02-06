
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

;;; Base arc walker

(defwalker arc)

(defwalk/sp arc quote (rest) `(quote ,@rest))

(defwalk/sp arc if (rest)
  (labels ((_if (args)
	     (cond ((null args) nil)
		   ((null (cdr args)) (walk (car args)))
		   (t `(if ,(walk (car args))
			   ,(walk (cadr args))
			   ,(_if (cddr args)))))))
    (_if rest)))

(defwalk/sp arc fn (rest)
  (labels ((_warg (a)
	     (if (and (consp a) (eq (car a) 'o))
		 `(o ,(cadr a) ,(walk (caddr a)))
		 a))
	   (_wargs (args)
	     (cond ((null args) nil)
		   ((consp args) (mapcar #'_warg args))
		   (t args))))
    `(fn ,(_wargs (car rest)) ,@(mapcar #'walk (cdr rest)))))

(defwalk/sp arc set (rest)
  `(set ,@(mapcar #'walk rest))) ; ?


;;; Macro expansion

(defwalker macex (arc))

(defun macro? (x)
  (when (symbolp x)
    (let ((fn (%symval x)))
      (when (%tag? 'mac fn)
	(%rep fn)))))

(defwalk macex list (head rest)
  (let ((m (macro? head)))
    (if m
	(walk (apply m rest))
	(cons head (mapcar #'walk rest)))))

;;; Continuation passing style



;;; Compilation

(defwalker c (arc))

(defwalk c list (head rest)
  ;;assume funcall
  `(funcall (%symval ',head) ,@(mapcar #'walk rest)))

;;; arcc & arcev

(defun arcc (form &optional env)
  (declare (ignore env))
  (walk 'c form))

(defun arcev (form &optional env)
  (eval (arcc form env)))

(defmacro defprim (name args &body body)
  `(defparameter ,(intern (format nil "@~a" name)) 
     (lambda ,args ,@body)))

(macrolet ((_arith (op)
	     `(defprim ,op (&rest args)
		(apply #',op args))))
  (_arith +)
  (_arith -)
  (_arith *)
  (_arith /))

#|
(defparameter @do 
  (%mk-tagged 'mac #'(lambda (&rest body) `(progn ,@body))))

(defparameter @progn
  (%mk-tagged 'mac #'(lambda (&rest body) `(fistro ,@body))))
|#
