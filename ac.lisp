
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

;;; Base arc walker

(defwalker arc)

(defwalk/sp arc quote (rest) `(quote ,@rest))

(defwalk/sp arc if (rest)
  `(if ,@(mapcar #'walk rest))) ; ?

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

(defvar *env* nil)

(defwalk c list (head rest)
  (let ((wrest (mapcar #'walk rest)))
    (cond ((symbolp head)
	   `(funcall (%symval ',head) ,@wrest))
	  (t 
	   `(funcall ,(walk head) ,@wrest)))))

(defwalk/sp c if (rest)
  (labels ((_if (args)
	     (cond ((null args) nil)
		   ((null (cdr args)) (walk (car args)))
		   (t `(if ,(walk (car args))
			   ,(walk (cadr args))
			   ,(_if (cddr args)))))))
    (_if rest)))

(defwalk/sp c fn (rest)
  (let (syms)
    (labels ((_err ()
	       (error "Error parsing lambda list"))
	     (_opt? (a)
	       (and (consp a) (eq (car a) 'o)))
	     (_arg (o? a)
	       (cond (o? (unless (eq (car a) 'o) (_err))
			 (push (cadr a) syms)
			 (if (cddr a)
			     `((,(cadr a) ,(walk (caddr a))))
			     (list (cadr a))))
		     (t (push a syms)
			(list a))))
	     (_argl (o? x)
	       (unless (null x)
		 (let ((a (car x)))
		   (cond ((null x) nil)
			 ((_opt? a)
			  (append (if o? nil '(&optional))
				  (_arg t a) (_argl t (cdr x))))
			 (o? (_err))
			 ((null (cdr x))
			  (_arg (_opt? a) a))
			 ((consp (cdr x))
			  (append (_arg nil a) (_argl nil (cdr x))))
			 (t 
			  (push (cdr x) syms)
			  (append (_arg nil a) (list '&rest (cdr x))))))))
	     (_args (a) (cond ((null a) nil)
			      ((symbolp a) `(&rest ,a))
			      ((consp a) (_argl nil a))
			      (t (_err)))))
      (let ((args (_args (car rest)))
	    (body (let ((*env* (append syms *env*)))
		    (mapcar #'walk (cdr rest)))))
	`(lambda ,args
	   (declare (ignorable ,@syms)) ;; Avoid SBCL warning
	   ,@body)))))

(defwalk/sp c set (rest)
  (labels ((_pair (place val)
	     (if (member place *env*)
		 `(setf ,place ,val)
		 `(setf ,(%sym place) ,val)))
	   (_pairs (args)
	     (cond ((null args) nil)
		   ((consp (cdr args)) 
		    (cons (_pair (car args) (walk (cadr args)))
			  (_pairs (cddr args))))
		   (t (error "Odd number of arguments to set")))))
    `(progn ,@(_pairs rest))))

;;; SBCL backquote

(macrolet ((_pass (what)
	     `(defwalk/sp c ,what (rest)
		`(,',what ,@(mapcar #'walk rest)))))
  (_pass sb-impl::backq-list)
  (_pass sb-impl::backq-list*)
  (_pass sb-impl::backq-cons)
  (_pass sb-impl::backq-append))

;;; arcc & arcev

(defun arcc (form &optional env)
  (declare (ignore env))
  (walk 'c form))

(defun arcev (form &optional env)
  (eval (arcc form env)))

;;; Primitives

(defmacro defprim (name args &body body)
  (let ((_name (intern (format nil "@~a" name))))
    `(progn
       (defun ,_name ,args ,@body)
       (defparameter ,_name #',_name))))

(macrolet ((_arith (op)
	     `(defprim ,op (&rest args)
		(apply #',op args))))
  (_arith -)
  (_arith *)
  (_arith /))

(defprim + (&rest args)
  (if (stringp (car args))
      (apply #'concatenate 'string args)
      (apply #'+ args)))

(defprim cons (a b)
  (cons a b))

#|
(defparameter @do 
  (%mk-tagged 'mac #'(lambda (&rest body) `(progn ,@body))))

(defparameter @progn
  (%mk-tagged 'mac #'(lambda (&rest body) `(fistro ,@body))))
|#
