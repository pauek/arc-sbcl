
(declaim (optimize (debug 3)))

(in-package :arc/test)

(defmacro with-mac (macs &body body)
  (flet ((_on (m)
	   `(setf (symbol-value ',(arcsym (car m)))
		  (arc::%mk-tagged 'arc::mac 
				   #'(lambda ,(cadr m) 
				       ,@(cddr m)))))
	 (_off (m)
	   `(unintern ',(arcsym (car m)))))
    `(progn
       ,@(mapcar #'_on macs)
       (prog1 (progn ,@body)
	 ,@(mapcar #'_off macs)))))

(deftest mexp-1 
  (with-mac ((do (&rest b) `(progn ,@b))
	     (x  () 'y)
	     (z  () 5)
	     (o  (&rest r) `(oh! ,@r)))
    (chkmac '(fn (x) (+ x 1)) "(fn (x) (+ x 1))")
    (chkmac '(fn (x y) (+ x y)) "(fn (x y) (+ x (x)))")
    (chkmac '(fn ((o x 5)) (+ x 1)) "(fn ((o x (z))) (+ x 1))")
    (chkmac '(fn ((o x -1)) (+ x 5)) "(fn ((o x -1)) (+ x 5))")
    (chkmac '(fn ((o i (oh! 1 2))) i) "(fn ((o i (o 1 2))) i)")
    (chkmac '(progn a b) "(do a b)")))