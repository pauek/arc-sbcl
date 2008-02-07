
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

;;; Base arc walker

(defgeneric wif (wkr rest)
  (:method (wkr rest) `(if ,@rest)))

(defgeneric wfn (wkr nrm opt rest syms body)
  (:method (wkr nrm opt rest syms body)
    (declare (ignore syms))
    (flet ((_opt (a) `(o ,@a)))
    `(fn (,@nrm ,@(mapcar #'_opt opt) . ,(car rest)) ,@body))))

(defgeneric wset (wkr pairs)
  (:method (wkr pairs) `(set ,@pairs)))

(defgeneric wfuncall (wkr head rest)
  (:method (wkr head rest)
    `(,head ,@rest)))

(defvar *env* nil)

(defun env? (x)
  (member x *env*))

(defwalker arc)

(defwalk arc list (head rest)
  (let ((wrest (mapcar #'walk rest)))
    (wfuncall (wkr) (walk head) wrest)))

(defwalk/sp arc quote (rest) 
  `(quote ,@rest))

(defwalk/sp arc set (rest)
  (unless (= 0 (mod (length rest) 2))
    (error "Odd number of arguments to set"))
  (wset (wkr) (mapcar #'walk rest)))

(defwalk/sp arc if (rest) 
  (wif (wkr) (mapcar #'walk rest)))

(defwalk/sp arc fn (fn-rest)
  (let (nrm opt rest syms)
    (labels ((_err ()
	       (error "Error parsing lambda list"))
	     (_opt? (a)
	       (and (consp a) (eq (car a) 'o)))
	     (_arg (o? a)
	       (cond (o? (unless (eq (car a) 'o) (_err))
			 (push (cadr a) syms)
			 (cond ((cddr a)
				(push (list (cadr a)
					    (walk (caddr a)))
				      opt))
			       (t (push (cadr a) opt))))
		     (t (push a nrm)
			(push a syms))))
	     (_argl (o? x)
	       (unless (null x)
		 (let ((a (car x)))
		   (cond ((null x) nil)
			 ((_opt? a) (_arg t a) (_argl t (cdr x)))
			 (o? (_err))
			 ((null (cdr x)) (_arg nil a))
			 ((consp (cdr x)) (_arg nil a) (_argl nil (cdr x)))
			 (t (push (cdr x) rest)
			    (push (cdr x) syms)
			    (_arg nil a))))))
	     (_args (a) 
	       (cond ((null a) nil)
		     ((symbolp a) 
		      (push a syms)
		      (push a rest))
		     ((consp a) (_argl nil a))
		     (t (_err)))))
      (_args (car fn-rest))
      (let ((body (let ((*env* (append syms *env*)))
		    (mapcar #'walk (cdr fn-rest)))))
	(wfn (wkr)
	     (nreverse nrm) 
	     (nreverse opt) 
	     (nreverse rest) 
	     (nreverse syms) 
	     body)))))

;;; SBCL backquote
(macrolet ((_pass (what)
	     `(defwalk/sp arc ,what (rest)
		`(,',what ,@(mapcar #'walk rest)))))
  (_pass sb-impl::backq-list)
  (_pass sb-impl::backq-list*)
  (_pass sb-impl::backq-cons)
  (_pass sb-impl::backq-append))


;;; Macro expansion

(defwalker mac (arc))

(defwalk mac list (head rest)
  (flet ((_macro? (x)
	   (when (symbolp x)
	     (let ((fn (%symval x)))
	       (when (%tag? 'mac fn)
		 (%rep fn))))))
    (let ((m (_macro? head)))
      (if m
	  (walk (apply m rest))
	  (cons head (mapcar #'walk rest))))))

;;; Continuation passing style



;;; Compilation

(defwalker c (arc))

(defwalk arc atom (x)
  (flet ((_literal? (a)
	   (or (null a) (eq a t)
	       (numberp a)
	       (stringp a)
	       (characterp a))))
    (cond ((_literal? x) x)
	  ((env? x) x)
	  (t `(%symval ',x)))))

(defmethod wif ((wkr c) rest)
  (labels ((_if (args)
	     (cond ((null args) nil)
		   ((null (cdr args)) (car args))
		   (t `(if ,(car args)
			   ,(cadr args)
			   ,(_if (cddr args)))))))
    (_if rest)))

(defmethod wfn ((wkr c) nrm opt rest syms body)
  `(lambda (,@nrm
	    ,@(when opt  `(&optional ,@opt))
	    ,@(when rest `(&rest ,@rest)))
     (declare (ignorable ,@syms)) ; Avoid SBCL warning
     ,@body))

(defmethod wset ((wkr c) pairs)
  (labels ((_pair (place val)
	     (if (env? place)
		 `(setf ,place ,val)
		 `(setf ,(%sym place) ,val)))
	   (_pairs (args)
	     (cond ((null args) nil)
		   ((consp (cdr args)) 
		    (cons (_pair (car args) (cadr args))
			  (_pairs (cddr args)))))))
    `(progn ,@(_pairs pairs))))

(defmethod wfuncall ((wkr c) head rest)
  (let ((len (length rest)))
    (cond ((<= 0 len 4) 
	   `(,(%sym (format nil "FUNCALL~a" len)) ,head ,@rest))
	  (t 
	   `($apply ,head (list ,@rest))))))

;;; arcme & arcc & arcev

(defun arcmac (form)
  (walk 'mac form))

(defun arcc (form)
  (walk 'c form))

(defun arcev (form)
  (eval (arcc form)))

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
  (if (stringp (car args))
      (apply #'concatenate 'string args)
      (apply #'+ args)))

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
