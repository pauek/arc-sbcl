
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

(defprim apply (fn &rest _args)
  (labels ((_app-args (args)
	     (cond ((null args) nil)
		   ((null (cdr args)) (car args))
		   (t (cons (car args) 
			    (_app-args (cdr args)))))))
    (let ((args (_app-args _args)))
      (cond ((functionp fn) (apply fn args))
	    ((consp fn) (nth (car args) fn))
	    ((stringp fn) (char fn (car args)))
	    ((hash-table-p fn) (gethash (car args) fn))
	    (t (error "Call to inappropriate object [~a]" fn))))))

(defprim bound (x)
  (%boundp x))

(defprim ssyntax (x)
  (%ssyntax? x))

(defparameter $sig (make-hash-table :test #'equal))

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

(defprim scar (x val)
  (if (stringp x) 
      (setf (char x 0) val)
      (setf (car x) val))
  val)

(defprim scdr (x val)
  (if (stringp x)
      (error "Can't set cdr of a string [~a]" x)
      (setf (cdr x) val))
  val)


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

(macrolet 
    ((_compare (sym)
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

(defprim is (&rest args)
  (or (every #'(lambda (x) (eql (car args) x)) (cdr args))
      (and (every #'stringp args)
	   (every #'(lambda (x) (string= (car args) x)) (cdr args)))
      (every #'null args)))

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

;; Tables

(defprim table ()
  (make-hash-table :test #'equal))

(defprim maptable (fn table)
  (maphash fn table))

;; Strings

(defprim newstring (n ch)
  (make-string n :initial-element ch))

;; Gensyms

(defprim uniq ()
  (gensym "$"))

;; call-with-current-continuation

(defun %ccc (k)
  (declare (ignore k))
  (error "CCC Not implemented"))

(defprim ccc (proc)
  (%ccc proc))

;; Input/output

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
		  (princ (car args) (_port (cdr args))))
		(force-output)
		nil)))
  (_wr1 writec c write-char)
  (_wr1 writeb b write-byte)
  (_wr2 write)
  (_wr2 disp))

(defprim sread (p eof)
  (read p nil eof))

;; Coerce

(defprim coerce (x type &rest args)
  (flet ((_err () 
	   (error "Can't coerce ~a ~a" x type))
	 (_num->str (n &optional (radix 10) precision)
	   (declare (ignore precision))
	   (format nil (format nil "~~~DR" radix) n))
	 (_str->num (str &optional (radix 10))
	   (parse-integer str :radix radix)))
    (cond ((%tagged? x) 
	   (error "Can't coerce annotated object [~a]" x))
	  ((eql type ($type x)) x)
	  ((characterp x) 
	   (case type
	     (int    (char-code x))
	     (string (string x))
	     (sym    (intern (string x)))
	     (t      (_err))))
	  ((integerp x)
	   (case type
	     (char   (code-char x))
	     (string (apply #'_num->str x args))
	     (t      (_err))))
	  ((numberp x)
	   (case type
	     (int    (round x))
	     (char   (code-char (round x)))
	     (string (apply #'_num->str x args))
	     (t      (_err))))
	  ((stringp x)
	   (case type 
	     (sym    (intern x))
	     (cons   (map 'list #'identity x))
	     (int    (or (apply #'_str->num x args)))
	     (t      (_err))))
	  ((consp x)
	   (case type
	     (string (map 'string #'identity x))
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

;; Sref

(defprim sref (com val ind) ; later make ind rest arg
  (cond ((hash-table-p com)  
	 (if (null val)
	     (remhash ind com)
	     (setf (gethash ind com) val)))
	((stringp com) 
	 (setf (char com ind) val))
	((consp com)   
	 (setf (nth ind com) val))
	(t (error "Can't set reference [~a ~a ~a]" com ind val)))
  val)


;; Threads

;; Sockets

(defprim open-socket (num)
  (let ((sk (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address sk) t)
    (socket-bind sk #(0 0 0 0) num) ;; 0.0.0.0 ????
    (sb-bsd-sockets:socket-listen sk 15) ;; from Araneida... 15?
    sk))

; socket-accept

;; System

(defprim sleep (n)
  (sleep n)
  nil)

(defprim system (cmd)
  (process-wait 
   (run-program "/bin/sh" (list "-c" cmd))) ; dirty trick for PATH
  nil)

(defprim pipe-from (cmd)
  (let ((p (run-program "/bin/sh" (list "-c" cmd))))
    (process-output p)))

(defprim protect (during after)
  (unwind-protect (funcall during)
    (funcall after)))

(defprim rand (n)
  (random n))

(defprim quit ()
  (sb-ext:quit))

;dir
;file-exists
;dir-exists
;rmfile

;; Arc

(defprim eval (e)
  (eval (arcc e)))

(defprim on-err (errfn f)
  (handler-case (funcall f)
    (error (e) (funcall errfn e))))

(defprim macex1 (e)
  (%macex (new-walker 'mac) (car e) (cdr e) t))

(defprim macex (e)
  (arcmac e))

;; (mxdef macex1 (e)
;;   (ac-macex e 'once))

(defun repl ()
  (flet ((_repl ()
	   (loop 
	      (princ "arc> ")
	      (w/no-colon (in *standard-input*)
		(let ((expr (read in)))
		  (if (eql expr :a)
		      (return 'done)
		      (let ((val (arcev expr)))
			(write val)
			(set '_that val)
			(set '_thatexpr expr)
			(terpri))))))))
    (format t "Use (quit) to quit, (tl) to return here after an interrupt.~%")
    (loop
       (handler-case (_repl)
	 (error (e) (format t "Error: ~a~%" e))))))

(defun aload (file)
  (with-open-file (_s file) 
    (w/no-colon (s _s)
      (loop for x = (read s nil t)
	 while x
	 do (arcev x))
      t)))


(defun acompile (inname)
  (flet ((acompile1 (in out)
	   (w/no-colon (_in in)
	     (loop for x = (read _in nil nil)
		while x
		do (let ((lisp (arcc x)))
		     (format t "~s~%" lisp)
		     (%arcev lisp)
		     (format t "~%")
		     (finish-output out))))))
    (let ((outname (format nil "~a.lisp" inname)))
      (with-open-file (in inname)
	(with-open-file (out outname 
			     :direction :output
			     :if-exists :supersede)
	  (acompile1 in out))))))

;;; Trace

(defprim trace (_sym)
  (unless (symbolp _sym)
    (error "Parameter must be a symbol"))
  (flet ((_reporter (orig)
	   #'(lambda (&rest args)
	       (format t "+~a~%" _sym)
	       (prog1 ($apply orig args)
		 (format t "-~a~%" _sym)))))
    (let ((sym  (%sym _sym))
	  (orig (%symval _sym)))
      (setf (get sym 'orig) orig)
      (set sym (if (%tag? 'mac orig)
		   (%mk-tagged 'mac (_reporter (%rep orig)))
		   (_reporter orig)))))
  _sym)

(defprim untrace (sym)
  (set (%sym sym)
       (get (%sym sym) 'orig))
  t)
