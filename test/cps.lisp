
(declaim (optimize (debug 3)))

(in-package :arc/test)

(deftest t-equal/cps 
  (chk (%equal/cps '(a b) '(a b)))
  (chk (%equal/cps '(#\a 1) '(#\a 1)))
  (chk (%equal/cps '(#:k) '(#:z)))
  (chk (not (%equal/cps '(#:i #:i) '(#:a #:b))))
  (chk (not (%equal/cps '(#:i #:j) '(#:a #:a))))
  (chk (%equal/cps '(fn (#:k1) a b c) '(fn (#:k2) a b c)))
  (chk (%equal/cps '(#:k2 #:k3 1 #\a "b") '(#:a2 #:a3 1 #\a "b")))
  (chk (not (%equal/cps '(#:k2 #:k3 1 #\a "b") '(#:a3 #:a3 1 #\a "b")))))

(deftest c-simple
  (chkcps "a" 
	  'a)
  (chkcps "#\\a" 
	  #\a)
  (chkcps "\"hi\"" 
	  "hi")
  (chkcps "(+ 1 x)" 
	  '(+ 1 x))
  (chkcps "(sqrt (+ 1 x))" 
	  '(sqrt (+ 1 x)))
  (chkcps "(+ '(1 2) '(3 4))"
	  '(+ '(1 2) '(3 4))))

(deftest c-backq
  (chkcps "`(,a)"
	  '`(,a))
  (chkcps "(fn (a) `(,a))"
	  '(fn (#:k a) (#:k `(,a))))
  (chkcps "(fn (a b) `(1 ,a ,@b))"
	  '(fn (#:k a b) (#:k `(1 ,a ,@b)))))

(deftest c-block
  (chkcps "(fn () a)"     
	  '(fn (#:k) (#:k a)))
  (chkcps "(fn () a b)"   
	  '(fn (#:k) (#:k b)))
  (chkcps "(fn () a b c)" 
	  '(fn (#:k) (#:k c)))
  (chkcps "(fn (x) (+ 1 (sqrt x)))"
	  '(fn (#:k x) (#:k (+ 1 (sqrt x)))))
  (chkcps "(fn () (a 1) b)"
	  '(fn (#:k) (a (fn (#:r) (#:k b)) 1)))
  (chkcps "(fn () a (b 1) c)"
	  '(fn (#:k) (b (fn (#:r) (#:k c)) 1)))
  (chkcps "(fn () a (b 1) c d)"
	  '(fn (#:k) (b (fn (#:r) (#:k d)) 1)))
  (chkcps "(fn () (a 1))"
	  '(fn (#:k) (a (fn (#:r) (#:k #:r)) 1)))
  (chkcps "(fn () (a 1) (b 2))"
	  '(fn (#:k) (a (fn (#:r1) (b (fn (#:r2) (#:k #:r2)) 2)) 1))))

(deftest c-funcall
  (chkcps "(+)" 
	  '(+))
  (chkcps "(a (+))" 
	  '(a (fn (#:k) #:k) (+)))
  (chkcps "(a (b))"
	  '(b (fn (#:k1) (a (fn (#:k2) #:k2) #:k1))))
  (chkcps "(a (+ 1))"
	  '(a (fn (#:k) #:k) (+ 1)))
  (chkcps "(a (+ 1) (b 2))"
	  '(b (fn (#:k1)
		(a (fn (#:k2) #:k2)
		   (+ 1)
		   #:k1))
	    2))
  (chkcps "(+ 1 (a 1 (b 2) 3))"
	  '(b (fn (#:k1) (a (fn (#:k2) (+ 1 #:k2)) 1 #:k1 3)) 2))
  (chkcps "(+ 1 (- 2 (hi (sqrt x))))"
	  '(hi (fn (#:k)
		(+ 1 (- 2 #:k)))
	    (sqrt x)))
  (chkcps "(a (b x))"
	  '(b (fn (#:r1) (a (fn (#:r2) #:r2) #:r1)) x))
  (chkcps "(+ (a x) 2)"
	  '(a (fn (#:r1) (+ #:r1 2)) x))
  (chkcps "(+ (a 1) (b x))"
	  '(a (fn (#:r1)
		(b (fn (#:r2) 
		     (+ #:r1 #:r2))
		 x))
	    1))
  (chkcps "(* (a (+ (b 1) 1)) 2)"
	  '(b (fn (#:k1) 
		(a (fn (#:k2) 
		     (* #:k2 2)) 
		   (+ #:k1 1))) 
	    1))
  (chkcps "(fn (x f) (f (+ 1 x)))"
	  '(fn (#:k x f) 
	    (f (fn (#:r1) 
		 (#:k #:r1))
	     (+ 1 x)))))

(deftest c-if
  (chkcps "(if a b)"
	  '((fn (#:k) (if a (#:k b) (#:k nil)))
	    (fn (#:r) #:r)))
  (chkcps "(if a b c)"
	  '((fn (#:k) (if a (#:k b) (#:k c))) 
	    (fn (#:r) #:r)))
  (chkcps "(if (< a b) c d)"
	  '((fn (#:k) (if (< a b) (#:k c) (#:k d)))
	    (fn (#:r) #:r)))
  (chkcps "(if (a 1) x y)"
	  '((fn (#:k) (a (fn (#:a) (if #:a (#:k x) (#:k y))) 1))
	    (fn (#:r) #:r)))
  (chkcps "(if a (x 2) y)"
	  '((fn (#:k) (if a (x (fn (#:x) (#:k #:x)) 2) (#:k y)))
	    (fn (#:r) #:r)))
  (chkcps "(+ 1 (if a x y))"
	  '((fn (#:k) 
	      (if a (#:k x) (#:k y))) 
	    (fn (#:r) (+ 1 #:r))))
  (chkcps "(* 2 i (if (a 1) x y))"
	  '((fn (#:k) 
	      (a (fn (#:r1) 
		   (if #:r1 (#:k x) (#:k y))) 
		 1))
	    (fn (#:x) (* 2 i #:x))))
  (chkcps "(if x 0 y 1 (a 0) 2 3)"
	  '((fn (#:k)
	     (if x (#:k 0)
		 y (#:k 1)
		 (a (fn (#:a) 
		       (if #:a (#:k 2) (#:k 3)))
		    0)))
	    (fn (#:r) #:r)))
  (chkcps "(if a b c d e)"
	  '((fn (#:k)
	     (if a (#:k b)
		 c (#:k d)
		 (#:k e)))
	    (fn (#:r) #:r)))
  (chkcps "(if x 0 (a 1) 2 y 3 4)"
	  '((fn (#:k)
	     (if x (#:k 0)
		 (a (fn (#:a) 
		      (if #:a (#:k 2) 
			  y (#:k 3) (#:k 4)))
		    1)))
	    (fn (#:r) #:r)))
  (chkcps "(if (a 1) x (b 2) y z)"
	  '((fn (#:k)
	     (a (fn (#:a) 
		  (if #:a (#:k x)
		      (b (fn (#:b) 
			   (if #:b (#:k y) (#:k z)))
			 2)))
	        1))
	    (fn (#:r) #:r))))

(deftest c-set 
  (chkcps "(set a 1)" 
	  '(set a 1))
  (chkcps "(set a 1 b 2)" 
	  '(:do (set a 1) (set b 2)))
  (chkcps "(+ 1 (set a (b 2)))"
	  '(b (fn (#:b) (+ 1 (set a #:b))) 2))
  (chkcps "(set a 1 b (c 2))"
	  '(:do (set a 1) (c (fn (#:c) (set b #:c)) 2)))
  (chkcps "(+ 1 (set a 1 b (c 2)))"
	  '(:do (set a 1) (c (fn (#:c) (+ 1 (set b #:c))) 2))))

(deftest c-complex
  (chkcps "(set a (if (b 1) 5 10))"
	  '((fn (#:i) 
	     (b (fn (#:b)
		  (if #:b (#:i 5) (#:i 10)))
	      1))
	    (fn (#:r) (set a #:r))))
  (chkcps "((fn ()
             (if a b c)
             (set x 10)))"
	  '((fn (#:k)
	     ((fn (#:i) (if a (#:i b) (#:i c)))
	      (fn (#:n) (#:k (set x 10)))))
	    (fn (#:r) #:r))))

#|
  (chkcps "(fn () (if a b) d)"
	  '(fn (#:k)
	    ((fn (#:i) 
	      (if a (#:i b)
|#
	     
		    