(defpackage :andy.runtime
  (:use :cl)
  (:export :get-runtime
	   :get-addr))

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

(defun get-addr (key)
  (cdr (assoc key *memory-map*)))

(defun get-runtime ()
  *io-runtime*)

