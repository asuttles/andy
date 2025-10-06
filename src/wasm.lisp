(defpackage :andy.wasm
  (:use :cl)
  (:export :emit-wasm))

(in-package :andy.wasm)

(defvar *stream* nil)			; Output stream

(defun emit-program (prog-node)
  (format *stream* "(module ~%")
  (format *stream* "(func $main)~%")
  (format *stream* "(start $main)")
  (format *stream* ")"))

(defun emit-wasm (ast fn)
  (with-open-file (*stream* fn
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format t "Emitting WASM...~%")
    (emit-program ast)))
