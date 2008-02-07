
(in-package :arc/test)

(deftest p-cons
  (chkev 1      "(car '(1 . 2))")
  (chkev 2      "(cdr '(1 . 2))")
  (chkev 1      "(car '(1 2 3))")
  (chkev '(2 3) "(cdr '(1 2 3))")
  (chkerr "(car #\a)"))

(deftest p-arith-int
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

(deftest p-plus
  (chkev "hi"         "(+ \"h\" \"i\")")
  (chkev "hi, ho, ho" "(+ \"hi\" \", ho\" \", ho\")")
  (chkev '(1 2 3 4)   "(+ '(1 2) '(3 4))"))








    