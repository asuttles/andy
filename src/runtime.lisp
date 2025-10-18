(defpackage :andy.runtime
  (:use :cl)
  (:export :get-runtime
	   :allocate-constant-memory))

(in-package :andy.runtime)

(defparameter *io-runtime*
  (uiop:read-file-string "~/programming/lisp/andy/inc/io.wat"))

(defparameter *memory-map*
  '((:const-base        . #x0000)
    (:globals-base      . #x0400)
    (:runtime-headers   . #x0800)
    (:runtime-buffers   . #x0900)
    (:heap-base         . #x1000)
    (:stack-top         . #xF000)))

(defparameter *constant-offset* 1)	; Offset 0 reserved for /n

(defun get-addr (key)
  (cdr (assoc key *memory-map*)))

(defun get-runtime ()
  *io-runtime*)

(defun allocate-constant-memory (size)
  (prog1
      (+ (get-addr :const-base) *constant-offset*)
    (setf *constant-offset* (+ size *constant-offset*))))
