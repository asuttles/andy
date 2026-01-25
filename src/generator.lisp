(defpackage :andy.generator
  (:use :cl :andy.ast)
  (:export :generate-ir)
  (:import-from :andy.analyzer
   :abstract-symbol-kind
   :abstract-symbol-type
   :abstract-symbol-value))

(in-package :andy.generator)

(defvar *ir* nil)			; Program IR


;;; Function Hoisting
;;;    Recursively collect a list of all block functions
;;;    and hoist them to the top-level
(defun hoist-funcs (node)
  "Return a flat list of all function declarations in the AST."
  (cond
    ((typep node 'function-declaration)
     (cons node
           (hoist-funcs (func-body node))))
    ((typep node 'program-block)
     ;; Recursively build list, ignoring NILs...
     (mapcan #'hoist-funcs (block-funcs node)))
    (t nil)))


;;; ============================================================================
;;; Generate the Intermediate Representation (IR) as a Proper List 
;;; ============================================================================

;;; Generate Statements
(defun generate-statements (stmnt)
  nil)

;;; Generate Main Program Structure
(defun generate-constants (consts)
  (loop for c in consts
	collect (list :const
		      :symbol (const-symbol c)
		      :type (const-type c)
		      :val (const-value c))))

(defun generate-variables (vars)
  (loop for v in vars
	collect (list :var
		      :symbol (var-symbol v)
		      :type (var-type v))))

(defun generate-func-params (params)
  (loop for p in params
	collect
	(list :symbol (id-symbol p)
	      :type (expr-type p))))

(defun generate-func-body (block)
  (append
   (generate-constants (block-consts block))
   (generate-variables (block-vars block))
   (generate-statements (block-body block))))
   
(defun generate-func-def (name params ret-type block)
  (list :func
	:name name
	:params (generate-func-params params)
	:return-type ret-type
	:body (generate-func-body block)))
  
(defun generate-functions (funcs)
  (loop for f in funcs
	collect (generate-func-def (func-symbol f)
				   (func-params f)
				   (func-type f)
				   (func-body f))))

(defun generate-main-block (type body)
  (list (generate-func-def "main" nil type body)))

(defun generate-ir (ast)
  "Lower the source Syntax Tree into a flat Lisp IR"
  (format t "Generating Lisp IR...~%")
  (let* ((pb (program-block ast)))
    (setf *ir*
	  (nconc
	   (generate-constants  (block-consts pb))  ; Constants
	   (generate-variables  (block-vars pb))    ; Variables
	   (generate-functions  (hoist-funcs pb))   ; Functions
	   (generate-main-block (program-type ast)  ; Main Function
				pb))))
  *ir*)
