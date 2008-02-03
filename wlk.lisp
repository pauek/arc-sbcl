
(declaim (optimize (debug 3)))

(defparameter *special-forms* '(quote fn if set))

(macrolet ((_gen (name args form)
	     `(defgeneric ,name (wlk ,@args)
		(:method (wlk ,@args) 
		  (declare (ignore wlk)) ,form))))
  (_gen wliteral (x) x)
  (_gen wsyntax (form) form)
  (_gen wsymbol (s) s)
  (_gen wcall (head rest) (cons head rest)))

(defgeneric wspecial (wlk head rest))

(defmacro %defwalk (type name args &body body)
  (let* ((wlk (gensym))
	 (w (if type `(,wlk (eql ',type)) wlk)))
    `(defmethod ,name (,w ,@args)
       (flet ((walk (f) (walk ,wlk f)))
	 ,@body))))

(defmacro defwalk (type class args &body body)
  (let ((name (intern (format nil "W~a" class) 
		      (find-package :arc))))
    `(%defwalk ,type ,name ,args ,@body)))

(defmacro defwalk/sp (type spform (rest) &body body)
  `(defwalk ,type special ((head (eql ',spform)) ,rest)
     ,@body))

;; Default defwalk/sp

(defmacro %defwalk/sp (spform (rest) &body body)
  `(defwalk/sp nil ,spform (,rest) ,@body))

(%defwalk/sp quote (rest) `(quote ,@rest))

(%defwalk/sp if (rest)
  (labels ((_if (args)
	     (cond ((null args) nil)
		   ((null (cdr args)) (walk (car args)))
		   (t `(if ,(walk (car args))
			   ,(walk (cadr args))
			   ,(_if (cddr args)))))))
    (_if rest)))

(%defwalk/sp fn (rest)
  (labels ((_warg (a)
	     (if (and (consp a) (eq (car a) 'o))
		 `(o ,(cadr a) ,(walk (caddr a)))
		 a))
	   (_wargs (args)
	     (cond ((null args) nil)
		   ((consp args) (mapcar #'_warg args))
		   (t args))))
    `(fn ,(_wargs (car rest)) ,@(mapcar #'walk (cdr rest)))))

(%defwalk/sp set (rest)
  `(set ,@(mapcar #'walk rest))) ; ?

;; Walk function

(defun walk (wlk form)
  (flet ((_literal? (x)
	   (or (eq x t) (eq x nil)
	       (numberp x)
	       (characterp x)
	       (stringp x)))
	 (_syntax? (s) 
	   (declare (ignore s)))
	 (_symbol? (s) 
	   (symbolp s))
	 (_special? (s) 
	   (member s *special-forms*)))
    (if (atom form)
	(cond ((_literal? form) (wliteral wlk form))
	      ((_syntax? form)  (wsyntax wlk form))
	      ((_symbol? form)  (wsymbol wlk form))
	      (t (error "Bad object in expression [~a]" form)))
	(destructuring-bind (head . rest) form
	  (cond ((_syntax? head) 
		 (walk wlk (cons (wsyntax wlk head) rest)))
		((_special? head)
		 (wspecial wlk head rest))
		(t 
		 (wcall wlk head rest)))))))
