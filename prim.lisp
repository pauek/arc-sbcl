
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

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

;; cons, car, cdr...

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

(defprim err (msg &rest args)
  (let ((msg (format nil "~a~{~a~^ ~}" msg args)))
    (error msg)))

;; Arithmetic

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
  (cond ((null args) 0)
	((every #'stringp args)
	 (apply #'concatenate 'string args))
	((every #'listp args)
	 (apply #'append args))
	(t (apply #'+ args))))

(defun pairwise (pred args &optional base)
  (let ((n (length args)))
    (cond ((< n 2) base)
	  ((= n 2) (apply pred args))
	  (t (and (funcall pred (car args) (cadr args))
		  (pairwise pred (cdr args) base))))))

(macrolet ((_compare (sym)
	     (flet ((_cmp (str)
		      (intern (format nil "~a~a" str sym))))
	       `(defprim ,sym (&rest args)
		  (cond ((every #'numberp args)
			 (apply #',sym args))
			((every #'stringp args)
			 (not (null (pairwise #',(_cmp "STRING") args))))
			((every #'characterp args)
			 (pairwise #',(_cmp "CHAR") args))
			((every #'symbolp args)
			 (not (null (pairwise #',(_cmp "STRING") 
					      (mapcar #'symbol-name args)))))
			(t (apply #',sym args)))))))
  (_compare <)
  (_compare >))

(defprim len (x)
  (cond ((hash-table-p x) (hash-table-count x))
	(t (length x))))

;; Types

(defprim type (x)
  (cond ((%tagged? x)        (%type x))
	((consp x)           'cons)
	((symbolp x)         'sym) ; + null
	((functionp x)       'fn)
	((characterp x)      'char)
	((stringp x)         'string)
	((integerp x)        'int)
	((numberp x)         'num)
	((hash-table-p x)    'table)
	((input-stream-p x)  'input)
	((output-stream-p x) 'output)
	;; ((tcp-listener? x) 'socket)
	((typep x 'error) 'exception)
	(t (error "Type: unknown type ~a" x))))
	
(defprim annotate (typ x)
  (%mk-tagged typ x))

(defprim rep (x)
  (if (%tagged? x) (%rep x) x))

(defprim table ()
  (make-hash-table))

(defprim uniq ()
  (gensym "$"))

;; call-with-current-continuation

(defun %ccc (k)
  (declare (ignore k))
  (error "CCC Not implemented"))

(defprim ccc (proc)
  (%ccc proc))

(defprim infile (file)
  (open file :direction :input))

(defprim outfile (file &rest args)
  (open file 
	:direction :output 
	:if-exists (if (equal args '(append))
		       :append
		       :overwrite)))

(defprim instring (str)
  (make-string-input-stream str))

(defprim outstring ()
  (make-string-output-stream))

(defprim inside (output)
  (get-output-stream-string output))

(defprim close (p)
  (cond ((input-stream-p p) (close p))
	((output-stream-p p) (close p))
	((typep p 'socket) (sb-bsd-sockets:socket-close p))
	(t (error "Can't close ~a" p)))
  nil)

(defparameter $stdout *standard-output*)
(defparameter $stdin  *standard-input*)
(defparameter $stderr *error-output*)

(defprim call-w/stdout (port thunk)
  (let ((*standard-output* port)) (funcall thunk)))

(defprim call-w/stdin (port thunk)
  (let ((*standard-input* port)) (funcall thunk)))

(macrolet ((_f (name fn)
	     `(defprim ,name (stream)
		(let ((s (if stream
			     *standard-input*
			     stream)))
		  (,fn s nil nil)))))
  (_f readc read-char)
  (_f readb read-byte)
  (_f peekc peek-char))

(macrolet ((_port (args)
	     `(if (consp ,args) 
		  (car ,args) 
		  *standard-output*))
	   (_wr1 (name prm fn)
	     `(defprim ,name (,prm &rest args)
		(,fn ,prm (_port args))
		,prm))
	   (_wr2 (name)
	     `(defprim ,name (&rest args)
		(when (consp args)
		  (write (car args) 
			 :stream (_port (cdr args))))
		(force-output)
		nil)))
  (_wr1 writec c write-char)
  (_wr1 writeb b write-byte)
  (_wr2 write)
  (_wr2 disp))

(defprim sread (p eof)
  (read p nil eof))

(defun char->ascii (c)
  (char-code c))

(defun ascii->char (c)
  (code-char c))

(defun number->string (n &optional (radix 10) precision)
  (declare (ignore precision)) ;; will polish later...
  (format nil (format nil "~~~DR" radix) n))

(defun string->number (str &optional (radix 10))
  (parse-integer str :radix radix))

(defprim coerce (x type &rest args)
  (flet ((_err () (error "Can't coerce ~a ~a" x type)))
    (cond ((%tagged? x) 
	   (error "Can't coerce annotated object [~a]" x))
	  ((eql type (%type x)) x)
	  ((characterp x) 
	   (case type
	     (int    (char->ascii x))
	     (string (string x))
	     (sym    (intern (string x)))
	     (t      (_err))))
	  ((integerp x)
	   (case type
	     (char   (ascii->char x))
	     (string (apply #'number->string x args))
	     (t      (_err))))
	  ((numberp x)
	   (case type
	     (int    (round x))
	     (char   (ascii->char (round x)))
	     (string (apply #'number->string x args))
	     (t      (_err))))
	  ((stringp x)
	   (case type 
	     (sym    (intern x))
	     (cons   (map 'list #'identity x))
	     (int    (or (apply #'string->number x args)))
	     (t      (_err))))
	  ((consp x)
	   (case type
	     (string (map 'list #'identity x))
	     (t      (_err))))
	  ((null x)
	   (case type
	     (string "")
	     (t      (_err))))
	  ((symbolp x)
	   (case type
	     (string (symbol-name x))
	     (t      (_err))))
	  (t         x))))
