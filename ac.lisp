
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(in-package :arc)

;;; Base arc walker

(defwgeneric arc-if (rest)
  (:wmethod t (rest) `(if ,@rest)))

(defwgeneric arc-fn (arg-list body)
  (:wmethod t (arg-list body)
    (labels ((_rbld (x &optional acum)
	       (flet ((_rcr (a)
			(_rbld (cdr x) a)))
		 (if (null x) acum
		     (_rbld (cdr x)
			    (ecase (caar x)
			      (:rst (cadar x))
			      (:nrm (cons (cadar x) acum))
			      (:opt (cons `(o ,@(cdar x)) acum))
			      (:des (cons (_rbld (reverse (cadar x)))
					  acum))))))))
      `(fn ,(_rbld (reverse arg-list)) ,@body))))

(defwgeneric arc-set (pairs)
  (:wmethod t (pairs) 
    (flet ((_2list (p)
	     (list (car p) (cdr p))))
      `(set ,@(mapcan #'_2list pairs)))))

(defwgeneric arc-call (head rest)
  (:wmethod t (head rest)
    `(,(walk head) ,@(mapcar #'walk rest))))

(defvar *env* nil)
(defun env? (x) (member x *env*))
(defmacro w/env+ (val &body body)
  `(let ((*env* (append ,val *env*))) ,@body))

(defwalker arc)

(defwlist quote arc (rest)
  `(quote ,@rest))

(defwlist set arc (rest)
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
    (arc-set (_pairs rest))))

(defwlist if arc (rest) 
  (arc-if (mapcar #'walk rest)))

(defun sym-list (pargs &optional acum)
  (if (null pargs)
      (nreverse acum)
      (ecase (caar pargs)
	((:nrm :opt)
	 (sym-list (cdr pargs) (cons (cadar pargs) acum)))
	(:rst (cons (cadar pargs) acum))
	(:des (sym-list (cdr pargs) 
			(append (sym-list (cadar pargs))
				acum))))))

(defwfun parse-args (args &optional acum o?)
  (flet ((_opt? (x)
	   (and (consp x) (eq (car x) 'o)))
	 (_recur (ac+ o?)
	   (parse-args (cdr args) (cons ac+ acum) o?)))
    (cond ((null args) (nreverse acum))
	  ((symbolp args) 
	   (nreverse (cons `(:rst ,args) acum)))
	  ((consp args)
	   (let ((a (car args)))
	     (cond ((_opt? a) 
		    (w/env+ (sym-list acum)
		      (let ((val (when (caddr a) 
				   (walk (caddr a))))) ;; <--
			(_recur `(:opt ,(cadr a) ,val) t))))
		   (o? (error "Optional argument followed by non-optional"))
		   ((consp a) 
		    (_recur `(:des ,(parse-args a)) nil))
		   ((symbolp a)
		    (_recur `(:nrm ,a) nil))
		   (t (error "Error parsing args"))))))))

(defwlist fn arc (rest)
  (let* ((argl (parse-args (car rest)))
	 (body (w/env+ (sym-list argl)
		 (mapcar #'walk (cdr rest)))))
    (arc-fn argl body)))

(defwmethod list arc (head rest)
  (arc-call head rest))


;;; SBCL backquote
(macrolet ((_pass (what)
	     `(defwlist ,what arc (rest)
		`(,',what ,@(mapcar #'walk rest)))))
  (_pass sb-impl::backq-list)
  (_pass sb-impl::backq-list*)
  (_pass sb-impl::backq-cons)
  (_pass sb-impl::backq-comma)
  (_pass sb-impl::backq-append)
  (_pass sb-impl::backq-comma-at)
  (_pass sb-impl::backq-comma-dot))

;;; Macro expansion + special syntax

(defwalker mac (arc))

(defun tokens (sepr source tok acc)
  (cond ((null source)
         (reverse (cons (reverse tok) acc)))
        ((eq (car source) sepr)
         (tokens sepr (cdr source) nil
		 (cons (reverse tok) acc)))
        (t (tokens sepr (cdr source) (cons (car source) tok)
		   acc))))

(defparameter *cmp-char* #\:)
(defparameter *neg-char* #\~)

(defun %ssyntax? (f)
  (labels ((_has-char? (str i)
	     (and (>= i 0)
		  (or (let ((c (char str i)))
			(or (char= c *cmp-char*) 
			    (char= c *neg-char*)))
		      (_has-char? str (1- i))))))
    (and (symbolp f)
	 (not (or (eq f '+) (eq f '++)))
	 (let ((nm (symbol-name f)))
	   (_has-char? nm (1- (length nm)))))))

(defun %expand-syntax (sym)
  (labels ((_chars (sym)
	     (map 'list #'identity (symbol-name sym)))
	   (_charsval (chs)
	     (read-from-string (map 'string #'identity chs)))
	   (_exp1 (tok)
	     (if (eq (car tok) *neg-char*)
		 `(complement ,(_charsval (cdr tok)))
		 (_charsval tok))))
    (let ((elts (mapcar #'_exp1 
			(tokens *cmp-char* (_chars sym) 
				nil nil))))
      (if (null (cdr elts))
	  (car elts)
	  (cons 'compose elts)))))

(defwmethod atom mac (form)
  (if (%ssyntax? form)
      (walk (%expand-syntax form))
      form))

(defwmethod list mac (head rest)
  (flet ((_macro? (x)
	   (when (symbolp x)
	     (let ((fn (%symval x)))
	       (when (%tag? 'mac fn)
		 (%rep fn))))))
    (let ((m (_macro? head)))
      (if m 
	  ; (if once? (apply m rest) ...)
	  (walk (apply m rest))
	  (cons (walk head) (mapcar #'walk rest))))))

;;; Continuation passing style



;;; Compilation

(defwalker c (arc))

(defwmethod atom c (x)
  (flet ((_literal? (a)
	   (or (null a) (eq a t)
	       (numberp a)
	       (stringp a)
	       (characterp a))))
    (cond ((_literal? x) x)
	  ((env? x) x)
	  (t (%sym x)))))

(defwmethod arc-if c (rest)
  (labels ((_if (args)
	     (cond ((null args) nil)
		   ((null (cdr args)) (car args))
		   (t `(if ,(car args)
			   ,(cadr args)
			   ,(_if (cddr args)))))))
    (_if rest)))

(defun %arc-destructure (args sym body)
  (let (acum)
    (labels ((_ac (x) (push x acum))
	     (_destr (args curr &optional root?)
	       (if (null args) nil
		   (let ((a (car args)))
		     (ecase (car a)
		       (:rst (_ac `(,(cadr a) ,curr)))
		       (:nrm (_ac `(,(cadr a) 
				     (,(if root? '%ecar '%car) ,curr))))
		       (:opt (_ac `(,(cadr a) 
				     (or (%car ,curr) ,(caddr a)))))
		       (:des (_destr (cadr a) `(%car ,curr))))
		     (_destr (cdr args) `(%cdr ,curr) root?)))))
      (_destr args sym t)
      `(let* (,@(reverse acum))
	 ,@body))))
       
(defwmethod arc-fn c (arg-list _body)
  (labels ((_rmv-opt (lst)
	     (remove '&optional lst))
	   (_rmv-dup-opts (lst)
	     (let ((p (position '&optional lst)))
	       (if p
		   (append (subseq lst 0 (1+ p))
			   (_rmv-opt (subseq lst (1+ p))))
		   lst)))
	   (_rbld (x &optional acum)
	     (if (null x) 
		 (_rmv-dup-opts acum)
		 (_rbld (cdr x)
			(ecase (caar x)
			  (:rst `(&rest ,(cadar x)))
			  (:nrm (cons (cadar x) acum))
			  (:opt `(&optional ,(cdar x) ,@acum))
			  (:des `((&optional 
				   ,@(_rmv-opt (_rbld (reverse (cadar x)))))
				  ,@acum))))))
	   (_normal? ()
	     (every #'(lambda (x) (eq (car x) :nrm)) 
		    arg-list)))
    (if (_normal?)
	(let ((syms (sym-list arg-list)))
	  `(lambda ,syms
	     ,@(when syms 
		 `((declare (ignorable ,@(sym-list arg-list)))))
	     ,@_body))
	(let ((args (gensym))
	      (body `((declare (ignorable ,@(sym-list arg-list))) 
		      ,@_body)))
	  `(lambda (&rest ,args)
	     ,(%arc-destructure arg-list args body))))))

(defwmethod arc-set c (pairs)
  (labels ((_pair (p)
	     (destructuring-bind (place . val) p
	       (if (env? place)
		   `(setf ,place ,val)
		   `(setf ,(%sym place) ,val)))))
    `(progn ,@(mapcar #'_pair pairs))))

(defwmethod arc-call c (head rest)
  (let ((_head (walk head))
	(_rest (mapcar #'walk rest)))
  (let ((len (length _rest)))
    (cond ((<= 0 len 4) 
	   `(,(%sym (format nil "FUNCALL~a" len)) ,_head ,@_rest))
	  (t 
	   `($apply ,_head (list ,@_rest)))))))

;;; arcme & arcc & arcev

(defun arcmac (form)
  (dowalk 'mac form))

(defun arcc (form)
  (dowalk 'c (arcmac form)))

(defun %arcev (form)
  (flet ((_ign-warn (condition)
	   (declare (ignore condition))
	   ;; Avoid SBCL warnings for global symbols
	   (muffle-warning))) 
    (handler-bind ((warning #'_ign-warn))
      (eval form))))

(defun arcev (form)
  (%arcev (arcc form)))
