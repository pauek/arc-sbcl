
(declaim (optimize (debug 3)))

(in-package :arc/test)

(defmacro with-mac (macs &body body)
  (flet ((_on (m)
	   `(setf (symbol-value ',(arcsym (car m)))
		  (arc::%mk-tagged 'arc::mac 
				   #'(lambda ,(cadr m) 
				       ,@(cddr m)))))
	 (_off (m)
	   `(setf (symbol-value ',(arcsym (car m))) nil)))
    `(progn
       ,@(mapcar #'_on macs)
       (prog1 (progn ,@body)
	 ,@(mapcar #'_off macs)))))

(deftest m-simple
  (with-mac ((do (cc &rest b) (funcall cc `(progn ,@b)))
	     (x  (cc) (funcall cc 'y))
	     (z  (cc) (funcall cc 5))
	     (o  (cc &rest r) (funcall cc `(oh! ,@r))))
    (chkmac "x" 
	    'x)
    (chkmac "(fn (x) (+ x 1))" 
	    '(fn (x) (+ x 1)))
    (chkmac "(fn (x y) (+ x (x)))" 
	    '(fn (x y) (+ x y)))
    (chkmac "(fn ((o x (z))) (+ x 1))" 
	    '(fn ((o x 5)) (+ x 1)))
    (chkmac "(fn ((o x -1)) (+ x 5))" 
	    '(fn ((o x -1)) (+ x 5)))
    (chkmac "(fn ((o i (o 1 2))) i)" 
	    '(fn ((o i (oh! 1 2))) i))
    (chkmac "(fn (a b) (do a b))" 
	    '(fn (a b) (progn a b)))
    (chkev "((fn ((o y)) (set (x) 5) y))" 
	   5)))

(deftest m-nested
  (with-mac ((do (cc &rest b) (funcall cc `(progn ,@b))))
    (chkmac "(do 1 (do 3 5))" 
	    '(progn 1 (progn 3 5)))))

(deftest m-syntax
  (chkmac "(#\\: #\\;)" 
	  '(#\: #\;)) ;; chk. w/no-colon
  (chkev "(ssyntax 'a:b)" 
	 t)
  (chkev "(ssyntax '~a)" 
	 t)
  (chkev "(ssyntax 'z:y:~a)" 
	 t)
  (chkev "(ssyntax 'a)" 
	 nil)
  (chkev "(ssyntax 1)" 
	 nil)
  (chkev "(ssyntax \"hi!\")" 
	 nil)
  (chkev "(ssexpand 'a:b:c)" 
	 '(compose a b c))
  (chkev "(ssexpand '~x)" 
	 '(complement x))
  (chkev "(ssexpand 'a:~q:s)"
	 '(compose a (complement q) s))
  (chkmac "a:b:c" 
	  '(compose a b c))
  (chkmac "~a"  
	  '(complement a))
  (chkmac "a:~b:c" 
	  '(compose a (complement b) c))
  ;; nested
  (with-mac ((compose (cc &rest b) 
	       (funcall cc `(dummy ,@b))))
    (chkmac "a:b:c" 
	    '(dummy a b c))))
