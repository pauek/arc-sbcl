
(declaim (optimize (debug 3)))

(in-package :arc)

(defun macro? (x)
  (when (symbolp x)
    (let ((fn (%symval x)))
      (when (%tag? 'mac fn)
	(%rep fn)))))

(defwalk aexp call (head rest)
  (let ((m (macro? head)))
    (if m
	(walk (apply m rest))
	(cons head (mapcar #'walk rest)))))

#|
(defparameter @do 
  (%mk-tagged 'mac #'(lambda (&rest body) `(progn ,@body))))

(defparameter @progn
  (%mk-tagged 'mac #'(lambda (&rest body) `(fistro ,@body))))
|#
