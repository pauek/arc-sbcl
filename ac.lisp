
(declaim (optimize (debug 3)))

(in-package :arc)

;;; Macro expansion

(defwalker aexp (arc))

(defun macro? (x)
  (when (symbolp x)
    (let ((fn (%symval x)))
      (when (%tag? 'mac fn)
	(%rep fn)))))

(defwalk aexp list (head rest)
  (let ((m (macro? head)))
    (if m
	(walk (apply m rest))
	(cons head (mapcar #'walk rest)))))

;;; Continuation passing style

;;; Compilation

(defun arcc (form env)
  (declare (ignore form env))
  nil)

(defun arcev (form &optional env)
  (eval (arcc form env)))

#|
(defparameter @do 
  (%mk-tagged 'mac #'(lambda (&rest body) `(progn ,@body))))

(defparameter @progn
  (%mk-tagged 'mac #'(lambda (&rest body) `(fistro ,@body))))
|#
