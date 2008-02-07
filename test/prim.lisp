
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

(deftest t-compare
  (chkev t   "(< 0 1 2)")
  (chkev nil "(< 1 0 2)")
  (chkev t   "(< -5 0 5)")
  (chkev nil "(< 0 1 2 3 4 5 4)")
  (chkev t   "(< #\\a #\\z)")
  (chkev nil "(< #\\b #\\a)")
  (chkev t   "(< \"hi\" \"ho\")")
  (chkev t   "(< \"hi\" \"ho\" \"hu\")")
  (chkev nil "(< \"aaz\" \"aaa\"")
  (chkev t   "(< 'a 'b 'c 'd)")
  (chkev nil "(< 'a 'b 't 's)"))

(deftest t-len 
  (chkev 0  "(len nil)")
  (chkev 1  "(len '(1))")
  (chkev 2  "(len '(1 2))")
  (chkev 5  "(len '(a b c d e))")
  (chkev 0  "(len \"\")")
  (chkev 1  "(len \"a\")")
  (chkev 2  "(len \"ab\")")
  (chkev 10 "(len \"abcdefghij\")")
  ;; hash-tables
  )

(deftest t-type
  (chkev 'cons   "(type '(1))")
  (chkev 'cons   "(type (cons 1 2))")
  (chkev 'sym    "(type nil)")
  (chkev 'sym    "(type 'a)")
  (chkev 'fn     "(type +)")
  (chkev 'char   "(type #\\a)")
  (chkev 'string "(type \"hi\")")
  (chkev 'int    "(type 1)")
  (chkev 'num    "(type 1.4)")
  (chkev 'table  "(type (table))")
  ;; input
  ;; output
  ;; socket
  ;; exception
  (chkev 'pk     "(type (annotate 'pk 1001))"))

(deftest t-rep
  (chkev 5    "(rep 5)")
  (chkev #\a  "(rep #\\a)")
  (chkev 'a   "(rep 'a)")
  (chkev 1.6  "(rep 1.6)")
  (chkev "hi" "(rep \"hi\")")
  (chkev 1001 "(rep (annotate 'pk 1001))"))

(deftest t-coerce 
  (chkev 65    "(coerce #\\A 'int)")
  (chkev "a"   "(coerce #\\a 'string)")
  (chkev '|a|  "(coerce #\\a 'sym)")
  (chkev 'a    "(coerce #\\A 'sym)")
  (chkerr "(coerve #\\a 'num)")
  (chkerr "(coerve #\\a 'output)")
  ;;
  (chkev #\b   "(coerce 98 'char)")
  (chkev "351" "(coerce 351 'string)")
  ;;
  (chkev 5      "(coerce 5.1  'int)")
  (chkev #\C    "(coerce 67.2 'char)")
  (chkev "56.1" "(coerce 56.1 'string)"))
   
