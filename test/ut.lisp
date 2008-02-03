
(declaim (optimize (debug 3)))

(in-package :arc/test)

;;; Test utils

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tests* nil))

(defvar *curr* '(nil . 0))
(defvar *failed* nil)

(defmacro deftest (name &body body)
  (pushnew name *tests*)
  `(defun ,name ()
     (setf *curr* (cons ',name 1))
     (format t "~a~15t " ',name)
     (macrolet ((_e (x) `(arc-eval ',x))) ,@body)
     (terpri)))
     
(defun chk (res)
  (princ (if res #\. #\+))
  (when (not res) (push (copy-list *curr*) *failed*))
  (incf (cdr *curr*)))

(defun arc-read-form (str)
  (with-input-from-string (_s str)
    (w/no-colon-2 (s _s) (read s))))

(defun chkcmp (res str)
  (chk (== res (ac (arc-read-form str) nil))))

(defun chkev (res str)
  (chk (== res (arc-eval (arc-read-form str)))))

(defun test ()
  (setf *failed* nil)
  (loop for _t in (reverse *tests*)
     do (funcall (symbol-function _t)))
  (when *failed*
    (format t "~%Failed:~%")
    (let (prev)
      (dolist (f (reverse *failed*))
	(if (eq prev (car f))
	    (format t " [~a]" (cdr f))
	    (format t "~&  ~a [~a]" (car f) (cdr f)))
	(setf prev (car f)))))
  (values))

(defgeneric == (a b)
  (:method (a b) (equal a b)))
