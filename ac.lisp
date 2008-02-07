
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

;;; Base arc walker

(defgeneric wif (wkr rest)
  (:method (wkr rest) `(if ,@rest)))

(defgeneric wfn (wkr arg-nrm arg-opt arg-rest syms body)
  (:method (wkr arg-nrm arg-opt arg-rest syms body)
    (declare (ignore syms))
    (flet ((_opt (a) `(o ,@a)))
    `(fn (,@arg-nrm 
	  ,@(mapcar #'_opt arg-opt)
	  .
	  ,(car arg-rest))
	  ,@body))))

(defgeneric wset (wkr rest)
  (:method (wkr rest) `(set ,@rest)))

(defgeneric wfuncall (wkr head rest)
  (:method (wkr head rest)
    `(,head ,@rest)))

(defvar *env* nil)

(defwalker arc)

(defwalk arc list (head rest)
  (let ((wrest (mapcar #'walk rest)))
    (cond ((symbolp head)
	   (wfuncall (wkr) `(%symval ',head) rest))
	  (t 
	   (wfuncall (wkr) (walk head) wrest)))))

(defwalk/sp arc quote (rest) 
  `(quote ,@rest))

(defwalk/sp arc set (rest) 
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

(defmethod wif ((wkr c) rest)
  (labels ((_if (args)
	     (cond ((null args) nil)
		   ((null (cdr args)) (car args))
		   (t `(if ,(car args)
			   ,(cadr args)
			   ,(_if (cddr args)))))))
    (_if rest)))

(defmethod wfn ((wkr c) nrm arg-opt arg-rest syms body)
  `(lambda (,@nrm
	    ,@(when arg-opt  `(&optional ,@arg-opt))
	    ,@(when arg-rest `(&rest ,@arg-rest)))
     (declare (ignorable ,@syms)) ; Avoid SBCL warning
     ,@body))

(defmethod wset ((wkr c) rest)
  (labels ((_pair (place val)
	     (if (member place *env*)
		 `(setf ,place ,val)
		 `(setf ,(%sym place) ,val)))
	   (_pairs (args)
	     (cond ((null args) nil)
		   ((consp (cdr args)) 
		    (cons (_pair (car args) (cadr args))
			  (_pairs (cddr args))))
		   (t (error "Odd number of arguments to set")))))
    `(progn ,@(_pairs rest))))

(defmethod wfuncall ((wkr c) head rest)
  `(funcall ,head ,@rest))

;;; arcme & arcc & arcev

(defun arcmac (form)
  (walk 'mac form))

(defun arcc (form)
  (walk 'c form))

(defun arcev (form)
  (eval (arcc form)))

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

