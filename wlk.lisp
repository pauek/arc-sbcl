
(declaim (optimize (debug 3)))

(macrolet ((_dg (name)
	     `(defgeneric ,name (wlk form)
		(:method (wlk form) 
		  (declare (ignore wlk form))))))
  (_dg wliteral)
  (_dg wsyntax)
  (_dg wsymbol)
  (_dg wcall))

(defgeneric wspecial (wlk head rest))

(defmacro %defwalk (class name args &body body)
  (let ((wlk (gensym)))
    `(defmethod ,name ((,wlk ,class) ,@args)
       (flet ((walk (f) (walk ,wlk f)))
	 ,@body))))

(defgeneric special-form? (wlk form)
  (:method (w f) nil))

(defmacro defwalker (class &optional super slots)
  (flet ((_slot (s) 
	   (if (consp s)
	       `(,(car s) :accessor ,(car s) :initform ,(cdr s))
	       `(,s :accessor ,s :initform nil))))
    `(defclass ,class ,super 
       (,@(mapcar #'_slot slots)
	(spforms :accessor special-forms :allocation :class)))))

(defmacro defwalk (class form args &body body)
  (let ((name (intern (format nil "W~a" form) 
		      (find-package :arc))))
    `(%defwalk ,class ,name ,args ,@body)))

(defmacro defwalk/sp (class spform (rest) &body body)
  `(progn
     (defmethod special-form? ((wlk ,class) (f (eql ',spform))) t)
     (defwalk ,class special ((head (eql ',spform)) ,rest)
       ,@body)))

;; Default walker: identity.

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
	   (special-form? wlk s)))
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
