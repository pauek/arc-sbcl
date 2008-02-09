
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
  (with-mac ((do (&rest b) `(progn ,@b))
	     (x  () 'y)
	     (z  () 5)
	     (o  (&rest r) `(oh! ,@r)))
    (chkmac 'x "x")
    (chkmac '(fn (x) (+ x 1)) "(fn (x) (+ x 1))")
    (chkmac '(fn (x y) (+ x y)) "(fn (x y) (+ x (x)))")
    (chkmac '(fn ((o x 5)) (+ x 1)) "(fn ((o x (z))) (+ x 1))")
    (chkmac '(fn ((o x -1)) (+ x 5)) "(fn ((o x -1)) (+ x 5))")
    (chkmac '(fn ((o i (oh! 1 2))) i) "(fn ((o i (o 1 2))) i)")
    (chkmac '(fn (a b) (progn a b)) "(fn (a b) (do a b))")
    (chkmac '5 "((fn ((o y)) (set (x) 5) y))")))

(deftest m-nested
  (with-mac ((do (&rest b) `(progn ,@b)))
    (chkmac '(progn 1 (progn 3 5)) "(do 1 (do 3 5))")))

(deftest m-syntax
  (chkmac '(#\: #\;) "(#\\: #\\;)") ;; chk. w/no-colon
  (chkev t "(ssyntax 'a:b)")
  (chkev t "(ssyntax '~a)")
  (chkev t "(ssyntax 'z:y:~a)")
  (chkev nil "(ssyntax 'a)")
  (chkev nil "(ssyntax 1)")
  (chkev nil "(ssyntax \"hi!\")")
  (chkev '(compose a b c) "(ssexpand 'a:b:c)")
  (chkev '(complement x) "(ssexpand '~x)")
  (chkev '(compose a (complement q) s)
	 "(ssexpand 'a:~q:s)")
  (chkmac '(compose a b c) "a:b:c")
  (chkmac '(complement a)  "~a")
  (chkmac '(compose a (complement b) c) "a:~b:c")
  (with-mac ((compose (&rest b) `(dummy ,@b)))
    (chkmac '(dummy a b c) "a:b:c"))) ;; nested
