
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(in-package :arc)

;;; Base arc walker

(defgeneric wif (wkr rest)
  (:method (wkr rest) `(if ,@rest)))

(defgeneric wfn (wkr args body)
  (:method (wkr args body)
    `(fn ,args ,@body)))

(defgeneric wfn/cmplx (wkr nrm opt rest syms body)
  (:method (wkr nrm opt rest syms body)
    (declare (ignore syms))
    (labels ((_mklist (x) (if (atom x) (list x) x))
	     (_opt (a) `(o ,@(_mklist a))))
      `(fn (,@nrm ,@(mapcar #'_opt opt) . ,rest) ,@body))))

(defgeneric wset (wkr pairs)
  (:method (wkr pairs) 
    (flet ((_2list (p)
	     (list (car p) (cdr p))))
      `(set ,@(mapcan #'_2list pairs)))))

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
  (labels ((_pairs (lst)
	     (cond ((null lst) nil)
		   ((not (symbolp (car lst)))
		    (error "First argument to set must be a symbol [~a]" 
			   (car lst)))
		   ((consp (cdr lst))
		    (cons (cons (car lst) (walk (cadr lst)))
			  (_pairs (cddr lst))))
		   (t 
		    (error "Odd number of arguments to set")))))
    (wset (wkr) (_pairs rest))))

(defwalk/sp arc if (rest) 
  (wif (wkr) (mapcar #'walk rest)))

(defun parse-args (wkr args)
  (let (nrm opt rest syms)
    (labels ((_err ()
	       (error "Error parsing lambda list"))
	     (_walk (f)
	       (wform wkr f))
	     (_opt? (a)
	       (and (consp a) (eq (car a) 'o)))
	     (_parg (o? a)
	       (cond (o? (unless (eq (car a) 'o) (_err))
			 (push (cadr a) syms)
			 (cond ((cddr a)
				(push (list (cadr a)
					    (_walk (caddr a)))
				      opt))
			       (t (push (cadr a) opt))))
		     (t (push a nrm)
			(push a syms))))
	     (_pargl (o? x)
	       (unless (null x)
		 (let ((a (car x)))
		   (cond ((null x) nil)
			 ((_opt? a) (_parg t a) (_pargl t (cdr x)))
			 (o? (_err))
			 ((null (cdr x)) (_parg nil a))
			 ((consp (cdr x)) (_parg nil a) (_pargl nil (cdr x)))
			 (t (push (cdr x) rest)
			    (push (cdr x) syms)
			    (_parg nil a))))))
	     (_pargs (a) 
	       (cond ((null a) nil)
		     ((symbolp a) (push a syms) (push a rest))
		     ((consp a) (_pargl nil a))
		     (t (_err)))))
      (_pargs args)
      (values (nreverse nrm) 
	      (nreverse opt) 
	      (car rest)
	      (nreverse syms)))))

(defwalk/sp arc fn (rest)
  (labels ((_mklist (x) 
	     (if (atom x) (list x) x))
	   (_cmplx? (args)
	     (cond ((null args) nil)
		   ((symbolp args) t)
		   ((and (consp args) (symbolp (car args)))
		    (_cmplx? (cdr args)))
		   (t t)))
	   (_wbody (env+)
	     (let ((*env* (append env+ *env*)))
	       (mapcar #'walk (cdr rest))))
	   (_wfnc ()
	     (multiple-value-bind (nrm opt rest syms)
		 (parse-args (wkr) (car rest))
	       (let ((body (_wbody syms)))
		 (wfn/cmplx (wkr) nrm opt rest syms body))))
	   (_wfn ()
	     (wfn (wkr) (car rest) 
		  (_wbody (_mklist (car rest))))))
    (if (_cmplx? (car rest))
	(_wfnc)
	(_wfn))))

;;; SBCL backquote
(macrolet ((_pass (what)
	     `(defwalk/sp arc ,what (rest)
		`(,',what ,@(mapcar #'walk rest)))))
  (_pass sb-impl::backq-list)
  (_pass sb-impl::backq-list*)
  (_pass sb-impl::backq-cons)
  (_pass sb-impl::backq-comma)
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
	  (cons (walk head) (mapcar #'walk rest))))))

;;; Continuation passing style



;;; Compilation

(defwalker c (arc))

(defwalk c atom (x)
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

(defmethod wfn ((wkr c) _args body)
  (let ((args (if (listp _args) 
		  _args 
		  `(&rest ,_args))))
    `(lambda ,args ,@body)))

(defmethod wfn/cmplx ((wkr c) nrm opt rest syms body)
  `(lambda (,@nrm
	    ,@(when opt  `(&optional ,@opt))
	    ,@(when rest `(&rest ,rest)))
     (declare (ignorable ,@syms)) ; Avoid SBCL warning
     ,@body))

(defmethod wset ((wkr c) pairs)
  (labels ((_pair (p)
	     (destructuring-bind (place . val) p
	       (if (env? place)
		   `(setf ,place ,val)
		   `(setf ,(%sym place) ,val)))))
    `(progn ,@(mapcar #'_pair pairs))))

(defmethod wfuncall ((wkr c) head rest)
  (let ((len (length rest)))
    (cond ((<= 0 len 4) 
	   `(,(%sym (format nil "FUNCALL~a" len)) ,head ,@rest))
	  (t 
	   `($apply ,head (list ,@rest))))))

(defwalker mac+c (arc)
  (wmac (new-walker 'mac))
  (wc   (new-walker 'c)))

(defmethod wform ((w mac+c) form)
  (wform (wc w) (wform (wmac w) form)))

;;; arcme & arcc & arcev

(defun arcmac (form)
  (walk 'mac form))

(defun arcc (form)
  (walk 'c (arcmac form)))

(defun %arcev (form)
  (flet ((_ign-warn (condition)
	   ;; Avoid SBCL warnings for global symbols
	   (declare (ignore condition))
	   (muffle-warning))) 
    (handler-bind ((warning #'_ign-warn))
      (eval form))))

(defun arcev (form)
  (%arcev (arcc form)))
