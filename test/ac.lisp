
(in-package :arc/test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Test suite 

(deftest t-literals
  (chkev 1    "1")
  (chkev #\a  "#\\a")
  (chkev "a"  "\"a\"")
  (chkev 1.45 "1.45")
  (chkev nil  "nil")
  (chkev t    "t")
  (chkev 'a "(quote a)")
  (chkev '(1 2 3)   "(quote (1 2 3))"))


(deftest t-arith-int
  (chkev  3 "(+ 1 2)")
  (chkev  6 "(* 2 3)")
  (chkev  4 "(/ 12 3)")
  (chkev  1 "(- 2 1)")
  (chkev  0 "(+)")
  (chkev  1 "(*)")
  (chkev -1 "(- 1)")
  (chkev  1 "(+ 1)")
  (chkev 15 "(+ (+ 1 2) (+ 3 (+ 4 5)))")
  (chkev 10 "(+ 1 1 1 1 1 1 1 1 1 1)"))

(deftest t-plus
  (chkev  "hi" "(+ \"h\" \"i\")"))

(deftest t-funcalls
  (chkev 3 "((fn (x) (+ 1 x)) 2)")
  (chkev 5 "((fn (a b) (+ a b)) 2 3)")
  (chkev '((2 3) . 1) "((fn (x . y) (cons y x)) 1 2 3)")
  (chkev '((1 2)) "((fn x x) '(1 2))")
  (chkev 11 "((fn (x (o y 6)) (+ x y)) 5 6)")
  (chkev 11 "((fn (x (o y 6)) (+ x y)) 5)"))

(deftest t-if 
  (chkev 0 "(if t   0 1)")
  (chkev 1 "(if nil 0 1)")
  (chkev 0 "(if t   0 nil 1 2)")
  (chkev 1 "(if nil 0 t   1 2)")
  (chkev 2 "(if nil 0 nil 1 2)"))

(deftest t-backq
  (chkev '(1 2)         "((fn (x) `(1 ,x)) 2)")
  (chkev '(1 2 3)       "((fn (x y) `(,x 2 ,y)) 1 3)")
  (chkev '(1 2 3)       "((fn (x) `(1 ,@x)) '(2 3))")
  (chkev '(1 2 3 4)     "((fn (x y) `(1 ,@x ,y)) '(2 3) 4)")
  (chkev '(1 (2 3))     "((fn (x) `(1 (2 ,x))) 3)")
  (chkev '(1 (2 (3 4))) "((fn (x) `(1 (2 ,x))) '(3 4))")
  (chkev '((1 () 2 3))  "((fn x `((1 () ,@x))) 2 3)"))

(deftest t-set 
  (chkev #\a "((fn (x) (set x #\\a) x) #\\z)"))

(deftest t-brackets
  (chkev 2 "([+ _ 1] 1)"))

; TODO: t-coerce

#|
(deftest t-compose
  (chkc '((compose a b c) 1) ; ??
	"(a:b:c 1)"))
|#
