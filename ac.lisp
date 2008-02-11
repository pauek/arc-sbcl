
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(in-package :arc)

;;; Arc walker

(defmacro def-arc-walker ((name prefix) &rest +args)
  (flet ((_ (s) (intern (format nil "~a~a" prefix s))))
    `(defun ,name (e ,@+args)
       (cond ((atom e) (,(_ 'atom) e ,@+args))
	     ((backq? e) 
	      (cons (car e) 
		    (mapcar #'(lambda (e) 
				(,name e ,@+args))
			    (cdr e))))
	     ((consp e)
	      (case (car e)
		(quote `(quote ,@(cdr e)))
		(if     (,(_ 'if) (cdr e) ,@+args))
		(fn     (,(_ 'fn) (cdr e) ,@+args))
		(set    (,(_ 'set) (cdr e) ,@+args))
		(t      (,(_ 'call) (car e) (cdr e) ,@+args))))
	     (t (error "Bad object in expression [~a]" e))))))

;;; Utilities

(defun %parse-pairs (rest)
  (labels ((_pairs (lst)
	     (cond ((null lst) nil)
		   ((consp (cdr lst))
		    (cons (cons (car lst) (cadr lst))
			  (_pairs (cddr lst))))
		   (t (error "Odd number of arguments to set")))))
    (_pairs rest)))

(defun %sym-list (pargs &optional acum)
  (if (null pargs)
      (nreverse acum)
      (ecase (caar pargs)
	((:nrm :opt)
	 (%sym-list (cdr pargs) (cons (cadar pargs) acum)))
	(:rst (cons (cadar pargs) acum))
	(:des (%sym-list (cdr pargs) 
			(append (%sym-list (cadar pargs))
				acum))))))

(defun %parse-args (args &optional acum o?)
  (flet ((_opt? (x)
	   (and (consp x) (eq (car x) 'o)))
	 (_recur (ac+ o?)
	   (%parse-args (cdr args) (cons ac+ acum) o?)))
    (cond ((null args) (nreverse acum))
	  ((symbolp args) 
	   (nreverse (cons `(:rst ,args) acum)))
	  ((consp args)
	   (let ((a (car args)))
	     (cond ((_opt? a) 
		    (_recur `(:opt ,(cadr a) ,(caddr a)) t))
		   (o? (error "Optional argument followed by non-optional"))
		   ((consp a) 
		    (_recur `(:des ,(%parse-args a)) nil))
		   ((symbolp a)
		    (_recur `(:nrm ,a) nil))
		   (t (error "Error parsing args"))))))))

(defun %rebuild-args (args)
  (labels ((_rb (a &optional acum)
	     (if (null a) acum
		 (_rb (cdr a)
		      (ecase (caar a)
			(:rst (cadar a))
			(:nrm (cons (cadar a) acum))
			(:opt (cons `(o ,@(cdar a)) acum))
			(:des (cons (_rb (reverse (cadar a)))
				    acum)))))))
    (_rb (reverse args))))

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

;;; SBCL specific
(defun backq? (e)
  (and (consp e)
       (member (car e)
	       '(sb-impl::backq-list
		 sb-impl::backq-list*
		 sb-impl::backq-cons
		 sb-impl::backq-comma
		 sb-impl::backq-append
		 sb-impl::backq-comma-at
		 sb-impl::backq-comma-dot))))

;;; Macro expansion + special syntax

(defparameter *cmp-char* #\:)
(defparameter *neg-char* #\~)

(defun %tokens (sepr source tok acc)
  (cond ((null source)
         (reverse (cons (reverse tok) acc)))
        ((eq (car source) sepr)
         (%tokens sepr (cdr source) nil
		  (cons (reverse tok) acc)))
        (t (%tokens sepr (cdr source) (cons (car source) tok)
		    acc))))

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
			(%tokens *cmp-char* (_chars sym) 
				 nil nil))))
      (if (null (cdr elts))
	  (car elts)
	  (cons 'compose elts)))))

(def-arc-walker (arcmac mac-))

(defun mac-atom (e)
  (if (%ssyntax? e)
      (arcmac (%expand-syntax e))
      e))

(defun mac-if (rest)
  `(if ,@(mapcar #'arcmac rest)))

(defun mac-set (e)
  (labels ((_1pair (p)
	     (destructuring-bind (place . val) p
	       (list (arcmac place) (arcmac val)))))
    `(set ,@(mapcan #'_1pair (%parse-pairs e)))))

(defun mac-fn (e)
  (let ((arg-list (%parse-args (car e)))
	(_body (cdr e)))
    (labels ((_warg (a)
	       (if (eq :opt (car a))
		   `(:opt ,(cadr a) ,(arcmac (caddr a)))
		   a)))
      `(fn ,(%rebuild-args (mapcar #'_warg arg-list)) 
	   ,@(mapcar #'arcmac _body)))))

(defun mac-call (head rest)
  (flet ((_macro? (x)
	   (when (symbolp x)
	     (let ((fn (%symval x)))
	       (when (%tag? 'mac fn)
		 (%rep fn))))))
    (let ((m (_macro? head)))
      (if m 
	  (arcmac (apply m rest))
	  (cons (arcmac head) 
		(mapcar #'arcmac rest))))))

;;; Continuation passing style

(defparameter *cursor* (gensym "CUR"))

(defun %cset (n f)
  (append (subseq f 0 n) `(,*cursor*) (subseq f (1+ n))))

(defun %csubs (cc new)
  (subst new *cursor* cc))


(def-arc-walker (%arccps cps-) cc)

(defun arccps (e &optional (cc *cursor*))
  (%arccps e cc))

(defun cps-atom (e cc) 
  (declare (ignore cc))
  e)

(defun cps-call (head rest cc)
  (let ((pos (position-if-not #'atom rest)))
    (cond (pos 
	   (arccps (nth pos rest)
		   (%csubs cc `(,head ,@(%cset pos rest)))))
	  ((%prim? head) (%csubs cc `(,head ,@rest)))
	  (t (let* ((k (gensym "K")))
	       `(,head (fn (,k) 
			 ,(arccps (%csubs cc k)))
		       ,@rest))))))

(defun cps-fn (e cc) (declare (ignore e cc)))
(defun cps-if (e cc) (declare (ignore e cc)))
(defun cps-set (e cc) (declare (ignore e cc)))

;;; Compilation

(def-arc-walker (arcc c-) env)

(defun c-atom (x env)
  (flet ((_literal? (a)
	   (or (null a) (eq a t)
	       (numberp a)
	       (stringp a)
	       (characterp a))))
    (cond ((_literal? x) x)
	  ((member x env) x)
	  (t (%sym x)))))

(defun c-if (e env)
  (labels ((_if (args)
	     (cond ((null args) nil)
		   ((null (cdr args)) (arcc (car args) env))
		   (t `(if ,(arcc (car args) env)
			   ,(arcc (cadr args) env)
			   ,(_if (cddr args)))))))
    (_if e)))
       
(defun c-fn (e env)
  (let ((arg-list (%parse-args (car e)))
	(_body (cdr e)))
    (labels ((_warg-list (args &optional acum)
	       (if (null args) 
		   (nreverse acum)
		   (let ((a (car args)))
		     (if (eq :opt (car a))
			 (let ((env+ (append (%sym-list acum) env)))
			   (let ((val (when (caddr a) 
					(arcc (caddr a) env+))))
			     (_warg-list (cdr args) 
					 (cons `(:opt ,(cadr a) ,val) 
					       acum))))
			 (_warg-list (cdr args) (cons a acum))))))
	     (_normal? ()
	       (every #'(lambda (x) (eq (car x) :nrm)) 
		      arg-list)))
      (let* ((syms (%sym-list arg-list))
	     (body (let ((env+ (append syms env)))
		     (mapcar #'(lambda (e) (arcc e env+)) _body))))
	(if (_normal?)
	    `(lambda ,syms
	       ,@(when syms `((declare (ignorable ,@syms))))
	       ,@body)
	    (let ((asym  (gensym))
		  (wargs (_warg-list arg-list nil))
		  (d+b  `((declare (ignorable ,@syms)) ,@body)))
	      `(lambda (&rest ,asym)
		 ,(%arc-destructure wargs asym d+b))))))))

(defun c-set (e env)
  (let ((pairs (%parse-pairs e)))
    (labels ((_pair (p)
	       (destructuring-bind (place . val) p
		 (let ((v (arcc val env)))
		   (unless (symbolp place)
		     (error "First argument to set must be a symbol [~a]" 
			    place))
		   (if (member place env)
		       `(setf ,place ,v)
		       `(setf ,(%sym place) ,v))))))
      `(progn ,@(mapcar #'_pair pairs)))))

(defun c-call (head rest env)
  (let ((_head (arcc head env))
	(_rest (mapcar #'(lambda (e) (arcc e env)) rest)))
    (let ((len (length _rest)))
      (cond ((%prim? head)
	     `(,(%sym head) ,@_rest))
	    ((<= 0 len 4) 
	     `(,(%sym (format nil "FUNCALL~a" len)) ,_head ,@_rest))
	    (t 
	     `($apply ,_head (list ,@_rest)))))))

;;; arcme & arcc & arcev

(defun %arcev (form)
  (flet ((_ign-warn (condition)
	   (declare (ignore condition))
	   ;; Avoid SBCL warnings for global symbols
	   (muffle-warning))) 
    (handler-bind ((warning #'_ign-warn))
      (eval form))))

(defun arcev (form)
  (%arcev (arcc (arcmac form) nil)))
