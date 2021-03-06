
(in-package :arc/test)

;;; Test utils

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tests* nil))

(defvar *curr* '(nil . 0))
(defvar *failed* nil)

(defgeneric == (a b)
  (:method (a b) (equal a b)))

(defmacro deftest (name &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (pushnew ',name *tests*))
     (defun ,name ()
       (setf *curr* (cons ',name 1))
       (format t "~a~15t " ',name)
       (macrolet ((_e (x) `(arcev ',x))) ,@body)
       (terpri))))
     
(defun chk (res)
  (princ (if res #\. #\+))
  (when (not res) (push (copy-list *curr*) *failed*))
  (incf (cdr *curr*)))

(defun arc-read-form (str)
  (with-input-from-string (s str)
    (w/no-colon (read s))))

(defun %equal/cps (x y)
  (let (binds)
    (labels ((_equ (n m)
	       (and (symbolp n) (symbolp m)
		    (string= (symbol-name n)
			     (symbol-name m))))
	     (_uninterned? (x)
	       (and (symbolp x) 
		    (null (symbol-package x))))
	     (_bind (x fn)
	       (find x binds :key fn :test #'_equ))
	     (_match? (i j)
	       (let ((a (_bind i #'car))
		     (b (_bind j #'cdr)))
		 (and (_equ (cdr a) j)
		      (_equ (car b) i))))
	     (_eq/cps (a b)
	       (cond ((and (null a) (null b)) t)
		     ((_uninterned? a)
		      (cond ((and (not (_bind a #'car))
				  (not (_bind b #'cdr)))
			     (setf binds (cons (cons a b) binds)))
			    ((_match? a b) t)))
		     ((and (atom a) (atom b)) (equal a b))
		     ((and (consp a) (consp b))
		      (and (_eq/cps (car a) (car b))
			   (_eq/cps (cdr a) (cdr b)))))))
      (_eq/cps x y))))

(macrolet ((_chk (name (fn &rest args) &optional (cmp '==))
	     `(defun ,name (str res)
		(let ((ret (ignore-errors
			     (,fn (arc-read-form str) ,@args))))
		  (chk (,cmp res ret))))))
  (_chk chkmac (arcmac))
  (_chk chkc   (arcc nil))
  (_chk chkcps (arccps) %equal/cps)
  (_chk chkev  (arcev)))

(defun chkerr (str)
  (chk (handler-case (arcev (arc-read-form str))
	 (error () t)
	 (:no-error (res) 
	   (declare (ignore res)) 
	   nil))))

(defun run (&rest which)
  (flet ((in-arc (sym)
	   (intern (symbol-name sym))))
    (let ((tests (if which 
		     (mapcar #'in-arc which)
		     (reverse *tests*))))
      (setf *failed* nil)
      (loop for _t in tests
	 do (funcall (symbol-function _t)))
      (when *failed*
	(format t "~%Failed:~%")
	(let (prev)
	  (dolist (f (reverse *failed*))
	    (if (eq prev (car f))
		(format t " [~a]" (cdr f))
		(format t "~&  ~a [~a]" (car f) (cdr f)))
	    (setf prev (car f))))
	(format t "~%"))
      (format t "~%")
      (values))))
