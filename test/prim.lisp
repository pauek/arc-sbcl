
(in-package :arc/test)

(deftest p-cons
  (chkev "(car '(1 . 2))" 1)
  (chkev "(cdr '(1 . 2))" 2)
  (chkev "(car '(1 2 3))" 1)
  (chkev "(cdr '(1 2 3))" '(2 3))
  (chkerr "(car #\a)"))

(deftest p-arith-int
  (chkev  "(+ 1 2)"   3)
  (chkev  "(* 2 3)"   6)
  (chkev  "(/ 12 3)"  4)
  (chkev  "(- 2 1)"   1)
  (chkev  "(+)"       0)
  (chkev  "(*)"       1)
  (chkev "(- 1)"     -1)
  (chkev  "(+ 1)"     1)
  (chkev "(+ (+ 1 2) (+ 3 (+ 4 5)))"  15)
  (chkev "(+ 1 1 1 1 1 1 1 1 1 1)"    10))

(deftest p-plus
  (chkev "(+ \"h\" \"i\")" 
	 "hi")
  (chkev "(+ \"hi\" \", ho\" \", ho\")" 
	 "hi, ho, ho")
  (chkev "(+ '(1 2) '(3 4))"   
	 '(1 2 3 4)))

(deftest t-compare
  (chkev "(< 0 1 2)"                t)
  (chkev "(< 1 0 2)"                nil)
  (chkev "(< -5 0 5)"               t)
  (chkev "(< 0 1 2 3 4 5 4)"        nil)
  (chkev "(< #\\a #\\z)"            t)
  (chkev "(< #\\b #\\a)"            nil)
  (chkev "(< \"hi\" \"ho\")"        t)
  (chkev "(< \"hi\" \"ho\" \"hu\")" t)
  (chkev "(< \"aaz\" \"aaa\""       nil)
  (chkev "(< 'a 'b 'c 'd)"          t)
  (chkev "(< 'a 'b 't 's)"          nil))

(deftest t-len 
  (chkev "(len nil)"            0)
  (chkev "(len '(1))"           1)
  (chkev "(len '(1 2))"         2)
  (chkev "(len '(a b c d e))"   5)
  (chkev "(len \"\")"           0)
  (chkev "(len \"a\")"          1)
  (chkev "(len \"ab\")"         2)
  (chkev "(len \"abcdefghij\")" 10)
  ;; hash-tables
  )

(deftest t-type
  (chkev "(type '(1))"         'cons)
  (chkev "(type (cons 1 2))"   'cons)
  (chkev "(type nil)"          'sym)
  (chkev "(type 'a)"           'sym)
  (chkev "(type +)"            'fn)
  (chkev "(type #\\a)"         'char)
  (chkev "(type \"hi\")"       'string)
  (chkev "(type 1)"            'int)
  (chkev "(type 1.4)"          'num)
  (chkev "(type (table))"      'table)
  ;; input
  ;; output
  ;; socket
  ;; exception
  (chkev "(type (annotate 'pk 1001))" 'pk))

(deftest t-rep
  (chkev "(rep 5)"        5)
  (chkev "(rep #\\a)"     #\a)
  (chkev "(rep 'a)"       'a)
  (chkev "(rep 1.6)"      1.6)
  (chkev "(rep \"hi\")"   "hi")
  (chkev "(rep (annotate 'pk 1001))" 
	 1001))

(deftest t-coerce 
  (chkev "(coerce #\\A 'int)"     65)
  (chkev "(coerce #\\a 'string)"  "a")
  (chkev "(coerce #\\a 'sym)"     '|a|)
  (chkev "(coerce #\\A 'sym)"     'a)
  (chkerr "(coerce #\\a 'num)")  
  (chkerr "(coerce #\\a 'output)")
  (chkev "(coerce 98 'char)"      #\b)
  (chkev "(coerce 351 'string)"   "351")
  (chkev "(coerce 5.1  'int)"     5)
  (chkev "(coerce 67.2 'char)"    #\C)
  (chkev "(coerce 56.1 'string)"  "56.1")
  (chkev "(coerce \"ABC\" 'sym)"  'abc)
  (chkev "(coerce \"abc\" 'cons)" '(#\a #\b #\c))
  (chkev "(coerce \"123\" 'int)"  123)
  (chkev "(coerce '(#\\j #\\k #\\l) 'string)"  
	 "jkl")
  (chkev "(coerce nil 'string)"   "")
  (chkev "(coerce 'symb 'string)" "SYMB"))
   
(deftest t-sref
  (chkev "((fn ((o x)) 
                     (set x \"hello\")
                     (sref x #\\c 0)
                     x))" 
	 "cello")
  (chkev "((fn ((o x)) 
             (set x '(1 2 3 4))
             (sref x 100 1)
             x))" 
	 '(1 100 3 4))
  (chkev "((fn ((o x)) 
             (set x (table))
             (sref x \"ho\" \"hi\")
             (x \"hi\")))" 
	 "ho"))

(deftest t-apply 
  (chkev "(apply + '(1 2 3))" 6)
  (chkev "(apply + 1 '(2 3))" 6)
  (chkev "(apply + 1 2 3 nil)" 6)
  (chkev "(apply * '(2 2 2))" 8)
  (chkev "(apply car '((1 . 2)))" 1)
  (chkev "(apply cdr '((1 . 2)))" 2)
  (chkev "(apply rep '(5))" 5)
  (chkev "(apply [* _ 2] '(7))" 14)
  (chkev "(apply (fn (x) (+ x \" \")) '(\"hola\"))" "hola "))
