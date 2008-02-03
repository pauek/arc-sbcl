
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(declaim (optimize (debug 3)))

(in-package :arc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Bracket notation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\] 
    (get-macro-character #\) nil))
  (set-macro-character #\[
    #'(lambda (stream char)
	(declare (ignore char))
	(list 'fn (list '_) (read-delimited-list #\] stream t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Handle package marker #\:

;; Version 1: Fragile hack...

(defmacro w/no-colon-1 (&body body)
  (flet ((_colon (on?)
	   `(setf (elt sb-impl::*constituent-trait-table* (char-code #\:))
		  ,(if on? 
		       'sb-impl::+char-attr-package-delimiter+
		       'sb-impl::+char-attr-constituent+))))
    `(progn
       ,(_colon nil)
       (let (res)
	 (unwind-protect (setf res (progn ,@body))
	   ,(_colon t)
	   res)))))

;; Version 2: Gray streams...

(defclass wrap-stream (fundamental-stream)
  ((stream :initarg :wrap :reader %stream)))

(defmethod stream-element-type ((s wrap-stream))
  (stream-element-type (%stream s)))

(defmethod close ((s wrap-stream) &key abort)
  (close (%stream s) :abort abort))


(defclass esc-stream (wrap-stream)
  ((echar :initarg :escape :accessor %char)
   (esc? :initform nil :accessor esc?)))

(defmethod stream-read-char ((s esc-stream))
  (if (esc? s)
      (prog1 (%char s) (setf (esc? s) nil))
      (let ((c (read-char (%stream s) nil :eof)))
	(cond ((eql c :eof) :eof)
	      ((char= c (%char s)) (prog1 #\\ (setf (esc? s) t)))
	      (t c)))))

(defmethod stream-unread-char ((s esc-stream) char)
  (if (esc? s) 
      (setf (esc? s) nil)
      (unread-char char (%stream s))))

(defmethod stream-peek-char ((s esc-stream))
  (if (esc? s) 
      (%char s)
      (let ((c (peek-char nil (%stream s))))
	(if (char= c (%char s)) #\\ c))))

(defun new-esc-stream (esc-char stream)
  (make-instance 'esc-stream 
		 :escape esc-char
		 :wrap stream))

;;;; Transform ':' into '\:' throughout.

(defmacro w/no-colon-2 ((var stream) &body body)
  `(with-open-stream (,var (new-esc-stream #\: ,stream))
     ,@body))
