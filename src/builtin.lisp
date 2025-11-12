(defpackage :andy.builtin
  (:use :cl :andy.ast :andy.runtime)
  (:export
   :lookup-builtin
   :import-math-p
   ))

(in-package :andy.builtin)

;; Summary of Built-In Functions
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
    ("ln"        :params (:float)        :type :float :emitter emit-ln    :import import-math)
    ("cls"       :params ()              :type :void  :emitter emit-cls   :import nil)))

;; Memory Locations for Terminal Escape Sequences
(defparameter *escapes*
  '(
    :newline                  (0   1)
    :clear-screen             (1   7)
    :cursor-home              (8   3)
    :hide-cursor              (11  6)
    :show-cursor              (17  6)
    :reset-style              (23  4)
    :bold                     (27  4)
    :italic                   (31  4)
    :underline                (35  4)
    :black-fg                 (39  5)
    :red-fg                   (44  5)
    :green-fg                 (49  5)
    :yellow-fg                (54  5)
    :blue-fg                  (59  5)
    :magenta-fg               (64  5)
    :cyan-fg                  (69  5)
    :white-fg                 (74  5)
    :black-bg                 (79  5)
    :red-bg                   (84  5)
    :green-bg                 (89  5)
    :yellow-bg                (94  5)
    :blue-bg                  (99  5)
    :magenta-bg               (104 5)
    :cyan-bg                  (109 5)
    :white-bg                 (114 5)
    :clear-line-from-cursor   (119 3)
    :clear-line               (122 4)
    :set-title                (126 9)
  ))


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


;;; Emit Terminal Escape Sequences

(defun emit-term-esc (esc name stream indent)
  (format stream "~VT;; write ~A~%" indent name)
  (format stream "~VTi32.const ~A   ;; offset~%" indent (first esc))
  (format stream "~VTi32.const ~A   ;; length~%" indent (second esc))
  (format stream "~VTcall $write_string~%" indent))


(defun emit-cls (stream offset)
  (emit-term-esc (getf *escapes* :clear-screen) "Clear Screen" stream offset))

