(defpackage :andy.c-emitter
  (:use :cl :andy.ast)
  (:export :emit-c)
  (:import-from :andy.analyzer
   :abstract-symbol-kind
   :abstract-symbol-type
   :abstract-symbol-value))

(in-package :andy.c-emitter)

(defvar *stream* nil)			; Output stream


;;; Indent the Output Source File Structures

(defvar *indent* 0)

(defun indent ()
  (setf *indent* (+ *indent* 2)))

(defun outdent ()
  (unless (<= *indent* 0)
    (setf *indent* (- *indent* 2))))

(defun get-ind ()
  *indent*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-andy-type (type)
  (case type
    (:int "andy_int")
    (:float "andy_float")
    (:string "andy_string")
    (t (error "Emitter: Unknown data type: ~A" type))))

;;; Function Hoisting - all functions at top-level in wasm
(defun collect-functions (node)
  "Return a flat list of all function declarations in the AST."
  (cond
    ((typep node 'function-declaration)
     (cons node
           (collect-functions (func-body node))))
    ((typep node 'program-block)
     ;; Recursively build list, ignoring NILs...
     (mapcan #'collect-functions (block-funcs node)))
    (t nil)))

;;; Emit Includes
(defun emit-c-headers ()
  "Emit Included C Headers"
  (write-line "#include \"andy.h\"" *stream*)
  (terpri *stream*)
  (terpri *stream*))

(defun emit-c-memory-allocation ()
  "Emit Globa Arena Allocation from Heap"
  (write-line "/* Initialize Program Heap Allocation */" *stream*)
  (write-line "andy_runtime_init();" *stream*)
  (write-line "arena_t* MEMORY = andy_get_global_arena();" *stream*)
  (terpri *stream*)
  (terpri *stream*))
  
;;; Emit Function Prototypes
(defun emit-c-function-prototypes (funcs)
  "Emit C Functions Definitions"
  (if funcs (write-line "/* Function Definitions */" *stream*))
  (dolist (f funcs)
    ;; Function Type and Name
    (format *stream* "static ~A ~A("
	    (get-andy-type (func-type f)) (func-symbol f))
    ;; Function Parameters
    (loop for param in (func-params f)
	  for i from 0
	  do (progn
	       (if (not (zerop i)) (format *stream* ", "))
	       (format *stream* "~A" (get-andy-type (expr-type param)))))
    (format *stream* ");~%")))

(defun emit-c-global-consts (consts)
  (if consts
      (write-line "/* Global Constants */" *stream*))
  (dolist (c consts)
    (let ((type (const-type c))
	  (name (const-symbol c))
	  (value (const-value c)))
      (case type
	;; Strings
	(:string 
	 (format *stream* "static const char ~A_data[] = ~A;~%" name value)
	 (format *stream* "const andy_string ~A = {~%" name)
	 (format *stream* "   .length = sizeof(~A_data) - 1,~%" name)
	 (format *stream* "   .data   = (char *)~A_data~%" name)
	 (write-line "};" *stream*))
	;; Integers
	(:int
	 (format *stream* "const andy_int ~A = ~A;~%" name value))
	;; Floats
	(:float
	 (format *stream* "const andy_float ~A = ~A;~%" name value)))))
  (if consts (terpri *stream*)))


(defun emit-c (ast fn)
  (with-open-file (*stream* fn
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format t "Emitting IR...~%")
    (let* ((pb (program-block ast))
	   (funcs (collect-functions pb)))
      ;; Start Program
      (emit-c-headers)
      (emit-c-memory-allocation)
      ;; Global Data
      (emit-c-global-consts (block-consts pb))
      ;; Function Definitions
      (emit-c-function-prototypes funcs)
      )))
