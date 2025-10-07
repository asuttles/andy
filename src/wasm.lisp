(defpackage :andy.wasm
  (:use :cl)
  (:export :emit-wasm))

(in-package :andy.wasm)

(defvar *stream* nil)			; Output stream


(defun get-wasm-type (pl0-type)
  (case pl0-type
    (:int "i32")
    (:float "f64")
    (t (error "Emitter: Unknown data type: ~A" pl0-type))))

(defun get-wasm-initializer (pl0-type)
  (case pl0-type
    (:int 0)
    (:float 0.0)
    (t (error "Emitter: Unknown data type: ~A" pl0-type))))

(defun emit-program ()
  (format *stream* "(module ~%"))	  

(defun emit-global-consts (consts)
  (dolist (c consts)
    (let ((wtype (get-wasm-type (const-type c))))
      (format *stream* "  (global $~A ~A (~A.const ~A))~%"
              (const-symbol c) wtype wtype
              (const-value c)))))

(defun emit-global-vars (vars)
  (dolist (v vars)
    (let* ((ptype (var-type v))
	   (wtype (get-wasm-type ptype))
	   (winit (get-wasm-initializer ptype)))
      (format *stream* "  (global $~A (mut ~A) (~A.const ~A))~%"
              (var-symbol v) wtype wtype winit))))

;;; Procedure Hoisting - all procedures at top-level in wasm
(defun collect-procedures (node)
  "Return a flat list of all procedure declarations in the AST."
  (cond
    ((typep node 'procedure-declaration)
     (cons node
           (collect-procedures (proc-body node))))
    ((typep node 'program-block)
     ;; Recursively build list, ignoring NILs...
     (mapcan #'collect-procedures (block-procs node)))
    (t nil)))


(defun emit-procedures (procs)
  (dolist (p procs)
    (format *stream* "~%  (func $~A~%   nop~%  )~%"
	    (proc-symbol p))))
  
(defun emit-end-program ()
  (format *stream* ")~%"))

(defun emit-wasm (ast fn)
  (with-open-file (*stream* fn
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format t "Emitting WASM...~%")
    (let* ((pb (program-block ast))
	   (procs (collect-procedures pb)))
      ;; Module
      (emit-program)
      ;; Constants
      (emit-global-consts
       (block-consts pb))
      ;; Global Variables
      (emit-global-vars	
       (block-vars pb))
      ;; Procedures
      (emit-procedures procs)
      ;; End Program
      (emit-end-program))))
