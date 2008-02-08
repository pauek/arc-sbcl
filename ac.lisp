
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(in-package :arc)

;;; Base arc walker

(defgeneric wif (wkr rest)
  (:method (wkr rest) `(if ,@rest)))

(defgeneric wfn (wkr arg-list body)
  (:method (wkr arg-list body)
    (declare (ignore wkr))
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

(defun parse-args (wkr args &optional acum o?)
  (flet ((_opt? (x)
	   (and (consp x) (eq (car x) 'o)))
	 (_recur (ac+ o?)
	   (parse-args wkr (cdr args) (cons ac+ acum) o?)))
    (cond ((null args) (nreverse acum))
	  ((symbolp args) 
	   (nreverse (cons `(:rst ,args) acum)))
	  ((consp args)
	   (let ((a (car args)))
	     (cond ((_opt? a) 
		    (let* ((*env* (append (sym-list acum) *env*))
			   (val (when (caddr a) (wform wkr (caddr a)))))
		      (_recur `(:opt ,(cadr a) ,val) t)))
		   (o? (error "Optional argument followed by non-optional"))
		   ((consp a) 
		    (_recur `(:des ,(parse-args wkr a)) nil))
		   ((symbolp a)
		    (_recur `(:nrm ,a) nil))
		   (t (error "Error parsing args"))))))))

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

(defwalk/sp arc fn (rest)
  (let* ((argl (parse-args (wkr) (car rest)))
	 (body (let ((*env* (append (sym-list argl) *env*)))
		 (mapcar #'walk (cdr rest)))))
    (wfn (wkr) argl body)))

;;; SBCL backquote
(macrolet ((_pass (what)
	     `(defwalk/sp arc ,what (rest)
		`(,',what ,@(mapcar #'walk rest)))))
  (_pass sb-impl::backq-list)
  (_pass sb-impl::backq-list*)
  (_pass sb-impl::backq-cons)
  (_pass sb-impl::backq-comma)
  (_pass sb-impl::backq-append)
  (_pass sb-impl::backq-comma-at)
  (_pass sb-impl::backq-comma-dot))

;;; Macro expansion

(defwalker mac (arc))

(defun tokens (sepr source tok acc)
  (cond ((null source)
         (reverse (cons (reverse tok) acc)))
        ((eq (car source) sepr)
         (tokens sepr (cdr source) nil
		 (cons (reverse tok) acc)))
        (t (tokens sepr (cdr source) (cons (car source) tok)
		   acc))))

(defwalk mac atom (form)
  (let ((_neg #\~) (_cmp #\:))
    (labels ((_chars (sym)
	       (map 'list #'identity (symbol-name sym)))
	     (_charsval (chs)
	       (read-from-string (map 'string #'identity chs)))
	     (_has-char? (str i)
	       (and (>= i 0)
		    (or (let ((c (char str i)))
			  (or (char= c _cmp) (char= c _neg)))
			(_has-char? str (1- i)))))
	     (_special? (x)
	       (and (symbolp x)
		    (not (or (eq x '+) (eq x '++)))
		    (let ((nm (symbol-name x)))
		      (_has-char? nm (1- (length nm))))))
	     (_exp1 (tok)
	       (if (eq (car tok) _neg)
		   `(complement ,(_charsval (cdr tok)))
		   (_charsval tok)))
	     (_expand (sym)
	       (let ((elts (mapcar #'_exp1 
				   (tokens _cmp (_chars sym) 
					   nil nil))))
		 (if (null (cdr elts))
		     (car elts)
		     (cons 'compose elts)))))
      (if (_special? form)
	  (walk (_expand form))
	  form))))

(defun %macex (wkr head rest &optional once?)
  (flet ((_walk (f)
	   (wform wkr f))
	 (_macro? (x)
	   (when (symbolp x)
	     (let ((fn (%symval x)))
	       (when (%tag? 'mac fn)
		 (%rep fn))))))
    (let ((m (_macro? head)))
      (if m 
	  (if once?
	      (apply m rest)
	      (_walk (apply m rest)))
	  (cons (_walk head) (mapcar #'_walk rest))))))

(defwalk mac list (head rest)
  (%macex (wkr) head rest))

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
	  (t (%sym x)))))

(defmethod wif ((wkr c) rest)
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
       
(defmethod wfn ((wkr c) arg-list _body)
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

(defmethod wset ((wkr c) pairs)
  (labels ((_pair (p)
	     (destructuring-bind (place . val) p
	       (if (env? place)
		   `(setf ,place ,val)
		   `(setf ,(%sym place) ,val)))))
    `(progn ,@(mapcar #'_pair pairs))))

(defmethod wfuncall ((wkr c) head rest)
  (let ((len (length rest)))
    (assert (not (%tag? 'mac head)))
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

