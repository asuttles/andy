(defpackage :andy.builtin
  (:use :cl :andy.ast)
  (:export
   :lookup-builtin
   ))

(in-package :andy.builtin)

(defparameter *builtins*
  '(("sqrt"      :params (:float)        :type :float :emitter emit-sqrt)
    ("int2float" :params (:int)          :type :float :emitter emit-int-to-float)
    ("float2int" :params (:float)        :type :int   :emitter emit-float-to-int)
    ("abs"       :params (:float)        :type :float :emitter emit-abs)
    ;;("pow"       :params (:float :float) :type :float :emitter emit-pow)
    ("neg"       :params (:float)        :type :float :emitter emit-neg)
    ("min"       :params (:float :float) :type :float :emitter emit-min)
    ("max"       :params (:float :float) :type :float :emitter emit-min)
    ("ceil"      :params (:float)        :type :float :emitter emit-ceil)
    ("floor"     :params (:float)        :type :float :emitter emit-floor)))

;;; (defvar *wasi-imports* '())

;;;(defun wasm-type (type)
;;;  (ecase type
;;;    (:int "i32")
;;;    (:float "f64")
;;;    (:void "")))

(defun get-builtin-func (name)
  (cdr (assoc name *builtins* :test #'string=)))

;;;(defun emit-wasi-imports (stream)
;;;  (dolist (name *wasi-imports*)
;;;    (let ((func (get-builtin-func name))
;;;	  (params ""))
;;;      ;; Import only WASI-defined Functions
;;;      (when (getf func :wasi-p))
;;;	(dolist (p (getf func :params))
;;;	  (setf params (format nil "~A (param ~A) " params (wasm-type p))))
;;;	(format stream "(import \"env\" \"~A\" (func $~A ~A (result ~A)))~%"
;;;		name name params (wasm-type (getf func :type)))))))

(defun lookup-builtin (name)
  "Lookup NAME in built-in library and return symbol structure, if found"
  (let ((func (get-builtin-func name)))
    (when func
      (progn
	;;	(pushnew name *wasi-imports*)	; Record all imported built-ins
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

