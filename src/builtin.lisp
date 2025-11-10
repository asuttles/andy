(defpackage :andy.builtin
  (:use :cl :andy.ast :andy.runtime)
  (:export
   :lookup-builtin
   :import-math-p
   ))

(in-package :andy.builtin)

(defparameter *builtins*
  '(("sqrt"      :params (:float)        :type :float :emitter emit-sqrt  :import nil)
    ("int2float" :params (:int)          :type :float :emitter emit-int-to-float :import nil)
    ("float2int" :params (:float)        :type :int   :emitter emit-float-to-int :import nil)
    ("abs"       :params (:float)        :type :float :emitter emit-abs   :import nil)
    ("neg"       :params (:float)        :type :float :emitter emit-neg   :import nil)
    ("min"       :params (:float :float) :type :float :emitter emit-min   :import nil)
    ("max"       :params (:float :float) :type :float :emitter emit-max   :import nil)
    ("ceil"      :params (:float)        :type :float :emitter emit-ceil  :import nil)
    ("floor"     :params (:float)        :type :float :emitter emit-floor :import nil)
    ("pow"       :params (:float :float) :type :float :emitter emit-pow   :import import-math)    
    ("sin"       :params (:float)        :type :float :emitter emit-sin   :import import-math)
    ("cos"       :params (:float)        :type :float :emitter emit-cos   :import import-math)
    ("tan"       :params (:float)        :type :float :emitter emit-tan   :import import-math)
    ("exp"       :params (:float)        :type :float :emitter emit-exp   :import import-math)
    ("ln"        :params (:float)        :type :float :emitter emit-ln    :import import-math)))


(defun get-builtin-func (name)
  (cdr (assoc name *builtins* :test #'string=)))

(defun lookup-builtin (name)
  "Lookup NAME in built-in library and return symbol structure, if found"
  (let* ((func (get-builtin-func name))
	 (import (getf func :import)))
    (when func
      (progn
	;; If func not in wasm, import a runtime
	(if import (funcall import))
	;; Make built-in funcall node
	(make-symbol-entry
	 name :builtin
	 :type (getf func :type)
	 :value (getf func :emitter)
	 :params (mapcar
		  (lambda (type)
		    (make-instance 'andy.ast:identifier :symbol name :type type))
		  (getf func :params)))))))


;;; Code Emitters - Math Functions
(defun emit-sqrt (stream offset)
  (format stream "~VTf64.sqrt~%" offset))

(defun emit-abs (stream offset)
  (format stream "~VTf64.abs~%" offset))

(defun emit-neg (stream offset)
  (format stream "~VTf64.neg~%" offset))

(defun emit-min (stream offset)
  (format stream "~VTf64.min~%" offset))

(defun emit-max (stream offset)
  (format stream "~VTf64.max~%" offset))

(defun emit-ceil (stream offset)
  (format stream "~VTf64.ceil~%" offset))

(defun emit-floor (stream offset)
  (format stream "~VTf64.floor~%" offset))

(defun emit-int-to-float (stream offset)
  (format stream "~VTf64.convert_i32_s~%" offset))

(defun emit-float-to-int (stream offset)
  (format stream "~VTi32.trunc_f64_s~%" offset))

(defun emit-exp (stream offset)
  (format stream "~VTcall $exp~%" offset))

(defun emit-ln (stream offset)
  (format stream "~VTcall $ln~%" offset))

(defun emit-sin (stream offset)
  (format stream "~VTcall $sin~%" offset))

(defun emit-cos (stream offset)
  (format stream "~VTcall $cos~%" offset))

(defun emit-tan (stream offset)
  (format stream "~VTcall $tan~%" offset))

(defun emit-pow (stream offset)
  (format stream "~VTcall $pow~%" offset))

