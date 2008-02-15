
;;; Implementation of Arc in SBCL
;;; 2008 (c) Pau Fernandez
;;; See COPYING for details

(in-package :arc)

(declaim #.*arc-opt*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Bracket notation

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\] 
    (get-macro-character #\) nil))
  (set-macro-character #\[
    #'(lambda (stream char)
	(declare (ignore char))
	(list 'fn (list (intern "_")) 
	      (read-delimited-list #\] stream t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Handle package marker #\:

;; Version 1: Fragile hack...

(defun %copy-trait-table (tab)
  (let* ((sz (length tab))
	 (newtab (make-array sz :element-type '(unsigned-byte 8))))
    (dotimes (i sz)
      (setf (aref newtab i) (aref tab i)))
    newtab))

(defmacro w/no-colon (&body body)
  `(let* ((old sb-impl::*constituent-trait-table*)
	  (new (%copy-trait-table old)))
     (setf (elt new (char-code #\:)) 
	   sb-impl::+char-attr-constituent+)
     (let ((sb-impl::*constituent-trait-table* new))
       ,@body)))

#|
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
	      ((char= c (%char s)) 
	       (prog1 #\\ (setf (esc? s) t)))
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

(defmacro w/no-colon ((var stream) &body body)
  `(with-open-stream (,var (new-esc-stream #\: ,stream))
     ,@body))

|#