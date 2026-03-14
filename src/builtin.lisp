(defpackage :andy.builtin
  (:use :cl :andy.ast)
  (:export
   :lookup-builtin))

(in-package :andy.builtin)

;; Summary of Built-In Functions
(defparameter *builtins*
  '(("sqrt"         :params (:float)         :type :float :runtime "andy_sqrt")
    ("int2float"    :params (:int)           :type :float :runtime "andy_i2f")
    ("float2int"    :params (:float)         :type :int   :runtime "andy_f2i")
    ("abs"          :params (:float)         :type :float :runtime "andy_abs")
    ("neg"          :params (:float)         :type :float :runtime "andy_neg")
    ("min"          :params (:float :float)  :type :float :runtime "andy_min")
    ("max"          :params (:float :float)  :type :float :runtime "andy_max")
    ("ceil"         :params (:float)         :type :float :runtime "andy_ceil")
    ("floor"        :params (:float)         :type :float :runtime "andy_floor")
    ("pow"          :params (:float :float)  :type :float :runtime "andy_pos")
    ("sin"          :params (:float)         :type :float :runtime "andy_sin")
    ("cos"          :params (:float)         :type :float :runtime "andy_cos")
    ("tan"          :params (:float)         :type :float :runtime "andy_tan")
    ("exp"          :params (:float)         :type :float :runtime "andy_exp")
    ("ln"           :params (:float)         :type :float :runtime "andy_ln")
    ("pressEnter"   :params (:void)          :type :void  :runtime "andy_press_enter")
    ("setFieldSize" :params (:int)           :type :void  :runtime "andy_set_fieldsize")
    ("setSigDigits" :params (:int)           :type :void  :runtime "andy_set_sigDigits")
    ("randomize"    :params (:void)          :type :void  :runtime "andy_randomize")
    ("random"       :params (:void)          :type :float :runtime "andy_random")
    ("seed"         :params (:int)           :type :int   :runtime "andy_seed")
    ("randomInt"    :params (:int)           :type :int   :runtime "andy_random_int")
    ("cls"          :params (:void)          :type :void  :runtime "andy_clearScreen")
    ("reset"        :params (:void)          :type :void  :runtime "andy_resetConsole")
    ("cll"          :params (:void)          :type :void  :runtime "andy_clearLine")
    ("bold"         :params (:void)          :type :void  :runtime "andy_bold")
    ("italic"       :params (:void)          :type :void  :runtime "andy_italic")
    ("underline"    :params (:void)          :type :void  :runtime "andy_underline")
    ("blink"        :params (:void)          :type :void  :runtime "andy_blink")
    ("reverse"      :params (:void)          :type :void  :runtime "andy_reverse")
    ("setFGrgb"    :params (:int :int :int) :type :void  :runtime "andy_setForegroundRGB")
    ("setBGrgb"    :params (:int :int :int) :type :void  :runtime "andy_setBackgroundRGB")))

;;; Handle Built-In Functions
(defun get-builtin-func (name)
  (cdr (assoc name *builtins* :test #'string=)))

(defun lookup-builtin (name)
  "Lookup NAME in built-in library and return symbol structure, if found"
  (let* ((func (get-builtin-func name))
	 (runtime-func (getf func :runtime)))
    (when func
      ;; Make built-in funcall node
      (make-symbol-entry
       name :builtin
       :type (getf func :type)
       :value runtime-func
       :params (mapcar
		(lambda (type)
		  (make-instance 'andy.ast:identifier :symbol "param" :type type))
		(getf func :params))))))
