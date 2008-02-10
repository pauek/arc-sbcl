
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(defpackage :arc
  (:use :cl 
	:sb-gray 
	:sb-ext 
	:sb-thread
	:sb-bsd-sockets
	:pk-walk)
  (:export :arcmac
	   :arccps
	   :arcc
	   :arcev
	   :w/no-colon
	   :compose
	   :fn
	   :o
	   :++
	   :int
	   :num
	   :sym
	   :table
	   :arcsym
	   :rmsym))
