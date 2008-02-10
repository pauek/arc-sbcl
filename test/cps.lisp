
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
  (chkcps 'a "a")
  (chkcps #\a "#\\a")
  (chkcps "hi" "\"hi\"")
  (chkcps '(+ 1 x) "(+ 1 x)")
  (chkcps '(sqrt (+ 1 x)) "(sqrt (+ 1 x))"))

(deftest c-fn
  (chkcps '(fn (#:k) (#:k a))     "(fn () a)")
  (chkcps '(fn (#:k) a (#:k b))   "(fn () a b)")
  (chkcps '(fn (#:k) a b (#:k c)) "(fn () a b c)")
  (chkcps '(fn (#:k x) (#:k (+ 1 (sqrt x))))
	  "(fn (x) (+ 1 (sqrt x)))"))

(deftest c-if
  (chkcps '(fn (#:k x) (if (< x 0) (#:k 0) (#:k 1)))
	  "(fn (x) (if (< x 0) 0 1))")
  (chkcps '(fn (#:k x) 
	    (if (< x 0) 
		(hi (fn (#:r1) (#:k #:r1)) 5) 
		(#:k x)))
	  "(fn (x) (if (< x 0) (hi 5) x))"))

(deftest c-funcall
  (chkcps '(fn (#:k1 x) 
	    (hi (fn (#:k2)
		 (#:k1 (+ 1 (- 2 #:k2))))
	     (sqrt x)))
	  "(fn (x) (+ 1 (- 2 (hi (sqrt x)))))")
  (chkcps '(fn (#:k x f) (f #:k (+ 1 x)))
	  "(fn (x f) (f (+ 1 x)))")
  (chkcps '(fn (#:k x) (b (fn (#:r1) (a #:k #:r1)) x))
	  "(fn (x) (a (b x)))")
  (chkcps '(fn (#:k x)
	    (a (fn (#:r1) (#:k (+ #:r1 2))) 
	     2))
	  "(fn (x) (+ (a x) 2))")
  (chkcps '(fn (#:k x)
	    (a (fn (#:r1)
		(b (fn (#:r2) (#:k (+ #:r1 #:r2)))
		   x))
	     1))
	  "(fn (x) (+ (a 1) (b x)))"))