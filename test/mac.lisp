
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
  (with-mac ((do (&rest body)
	       `(progn ,@body)))
    (chkmac '(progn a b) "(do a b)")))