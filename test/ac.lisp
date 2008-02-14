
(in-package :arc/test)

(deftest t-literals
  (chkev "1"     
	 1)
  (chkev "#\\a"  
	 #\a)
  (chkev "\"a\"" 
	 "a")
  (chkev "1.45"  
	 1.45)
  (chkev "nil"   
	 nil)
  (chkev "t"     
	 t)
  (chkev "(quote a)"       
	 'a)
  (chkev "(quote (1 2 3))" 
	 '(1 2 3)))

(deftest t-funcalls
  (chkev "((fn () -1))" 
	 -1)
  (chkev "((fn (x) (+ 1 x)) 2)" 
	 3)
  (chkev "((fn (a b) (+ a b)) 2 3)" 
	 5)
  (chkev "((fn (x . y) (cons y x)) 1 2 3)" 
	 '((2 3) . 1))
  (chkev "((fn x x) '(1 2))" 
	 '((1 2)))
  (chkev "((fn (x (o y 6)) (+ x y)) 5 6)" 
	 11)
  (chkev "((fn (x (o y 6)) (+ x y)) 5)" 
	 11)
  (chkev "((fn ((o a #\\a)) a) #\\b)" 
	 #\b)
  (chkev "((fn ((o a)) a))" 
	 nil)
  (chkev "((fn ((o a)) \"zz\"))" 
	 "zz")
  (chkev "((fn (x y (o z 1)) (* z (+ x y))) 5 5)" 
	 10)
  (chkev "((fn (x y (o z 1)) (* z (+ x y))) 5 5 2)" 
	 20)
  (chkev "((fn (x) (if (< x 0) 1) 5) 1)"
	 5))

(deftest t-cmplx-arg
  (chkev "((fn ((a b)) (- a b)) '(2 1))" 
	 1)
  (chkev "((fn (a (o b a)) (+ b a)) \"x\")" 
	 "xx")
  (chkev "((fn (a (o b 2) (o c (+ a b))) (* a b c)) 1)" 
	 6)
  (chkev "((fn (a (o b 2) (o c (+ a b))) (* a b c)) 2 3)" 
	 30)
  (chkev "((fn (a (o b 2) (o c (+ a b))) (* a b c)) 2 2 2)" 
	 8)
  (chkev "((fn ((x (y z))) `((,x ,y) ,z)) '(a (b c)))" 
	 '((a b) c))
  (chkev "((fn ((x (y z))) `((,x ,y) ,z)) nil)" 
	 '((nil nil) nil))
  (chkev "((fn (x (y z) w) `(,x ,y ,z ,w)) 'j '(k l) 'm)" 
	 '(j k l m))
  (chkev "((fn (x (y z) . w) `((,x ,y) ,z ,w)) 'j '(k l) 'm 'n 'o)" 
	 '((j k) l (m n o)))
  (chkev "((fn ((x y)) `(,x ,y)) '(a))"
	 '(a nil))
  (chkev "((fn ((x y z)) `(,x ,y ,z)) '(a b))" 
	 '(a b nil))
  (chkev "((fn ((x y z)) `(,x ,y ,z)) nil)" 
	 '(nil nil nil)))

(deftest t-index 
  (chkev "(\"hiho\" 0)"  
	 #\h)
  (chkev "(\"hiho\" 1)"
	 #\i)
  (chkev "(\"hiho\" 2)"  
	 #\h)
  (chkev "(\"hiho\" 3)"  
	 #\o)
  (chkerr "(\"hiho\" 4)")
  (chkev "('(1 2 3) 0)" 
	 1)
  (chkev "('(1 2 3) 1)" 
	 2)
  (chkev "('(1 2 3) 2)" 
	 3))

(defparameter arc::$x nil) ; Avoid SBCL warning

(deftest t-env
  (chkerr "(set x)")
  (chkerr "(set x 1 y)")
  (chkerr "(set x 1 y 2 z)")
  (chkerr "(set #\a 1)")
  (chkerr "(set \"h\" 'a)")
  (chkev "((fn () (set x 5) x))" 
	 5)
  (chkev "((fn ((o a) (o b)) (set a -3 b a) (cons a b)))" 
	 '(-3 . -3))
  (chkev "((fn (x) (set x #\\a) x) #\\z)" 
	 #\a)
  (chkev "((fn ((o x)) (set x 1) x))"   
	 1)
  (chkev "((fn ((o x 5)) ((fn ((o x 2)) x))))"   
	 2)
  (chkev "((fn ((o x)) ((fn (y) (set x y)) 3) x))"   
	 3)
  (chkerr "((fn (x y) nil))")
  (chkerr "((fn ((o x) y) x))"))

(deftest t-if 
  (chkev "(if t   0 1)" 
	 0)
  (chkev "(if nil 0 1)" 
	 1)
  (chkev "(if t   0 nil 1 2)" 
	 0)
  (chkev "(if t   0 t   1 2)" 
	 0)
  (chkev "(if nil 0 t   1 2)" 
	 1)
  (chkev "(if nil 0 nil 1 2)" 
	 2)
  (chkev "(if nil 0 nil 1 nil 2 t   3 nil 4)" 
	 3)
  (chkev "(if nil 0 nil 1 nil 2 nil 3 t   4)" 
	 4))

(deftest t-backq
  (chkev "((fn (x) `(1 ,x)) 2)"         
	 '(1 2))
  (chkev "((fn (x y) `(,x 2 ,y)) 1 3)"       
	 '(1 2 3))
  (chkev "((fn (x) `(1 ,@x)) '(2 3))"       
	 '(1 2 3))
  (chkev "((fn (x y) `(1 ,@x ,y)) '(2 3) 4)"     
	 '(1 2 3 4))
  (chkev "((fn (x) `(1 (2 ,x))) 3)"     
	 '(1 (2 3)))
  (chkev "((fn (x) `(1 (2 ,x))) '(3 4))" 
	 '(1 (2 (3 4))))
  (chkev "((fn x `((1 () ,@x))) 2 3)"  
	 '((1 () 2 3))))

(deftest t-brackets
  (chkev "([+ _ 1] 1)"        
	 2)
  (chkev "([cons _ 2] 1)" 
	 '(1 . 2))
  (chkev "([+ _ \", ho\"] \"hi\")" 
	 "hi, ho"))

(deftest t-error
  (chkerr "(err \"This is an error\")"))
