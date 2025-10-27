(defpackage :andy.emitter
  (:use :cl :andy.ast :andy.runtime)
  (:export :emit-wasm))

(in-package :andy.emitter)

(defvar *stream* nil)			; Output stream


;;; Operator Hash Tables
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +op-table-int+
    '((:eql "i32.eq")
      (:neq "i32.ne")
      (:lss "i32.lt_s")
      (:leq "i32.le_s")
      (:gtr "i32.gt_s")
      (:geq "i32.ge_s")
      (:or  "i32.or")
      (:and "i32.and")
      (:xor "i32.xor")
      (:xnor "i32.xor~%i32.eqz")))
  (defparameter +op-table-float+
    '((:eql "f64.eq")
      (:neq "f64.ne")
      (:lss "f64.lt")
      (:leq "f64.le")
      (:gtr "f64.gt")
      (:geq "f64.ge")))
  (defparameter +op-inversions+
    '((:eql :neq)
      (:neq :eql)
      (:lss :geq)
      (:leq :gtr)
      (:gtr :leq)
      (:geq :lss)
      (:and :or)
      (:or  :and)
      (:xor :xnor))))

(defparameter *string-addresses*	; Addresses for immutable strings
  '())

;;; Indent the Output Source File Structures

(defparameter *indent* 0)

(defun indent ()
  (setf *indent* (+ *indent* 2)))

(defun outdent ()
  (unless (<= *indent* 0)
    (setf *indent* (- *indent* 2))))

(defun get-ind ()
  *indent*)

;;; Helper Functions for Types

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

(defun get-op-code-string (type op)
  (cond ((eq type :int)
	 (cadr (assoc op +op-table-int+)))
	((eq type :float)
	 (cadr (assoc op +op-table-float+)))))

(defun get-scope-string (scope)
  (if (eq scope :local) "local" "global"))

;;; Emit Helpers
(defun emit-program ()
  "Emit Module Header and WASI IO Runtime"
  (format *stream* "~VT(module~%~%" (get-ind))
  (format *stream* "~A" (get-runtime)))

(defun emit-string (str)
  (let* ((name  (const-symbol str))
	 (text (const-value str))
	 (len  (length text))
	 (addr (allocate-constant-memory len)))
    (format *stream* "~VT(data (i32.const ~A) ~A)~%" (get-ind) addr text)
    (push (list name (cons len addr)) *string-addresses*)))

(defun emit-global-consts (consts)
  (dolist (c consts)
    (let ((type (const-type c)))
      (if (eq type :string)
	  ;; Emit String Data in Runtime Memory
	  (emit-string c)
	  ;; Emit Number Constants in WASM Memory
	  (let ((wtype (get-wasm-type type)))
	    (format *stream* "~VT(global $~A ~A (~A.const ~A))~%" (get-ind)
		    (const-symbol c) wtype wtype
		    (const-value c)))))))

(defun emit-global-vars (vars)
  (dolist (v vars)
    (let* ((ptype (var-type v))
	   (wtype (get-wasm-type ptype))
	   (winit (get-wasm-initializer ptype)))
      (format *stream* "~VT(global $~A (mut ~A) (~A.const ~A))~%" (get-ind)
              (var-symbol v) wtype wtype winit)))
    (format *stream* "~%"))

;;; Emit Binary Operator
(defun emit-binary-op (op typ)
  (let ((wtyp (get-wasm-type typ)))
    (cond
      ;; +
      ((eq op :plus)
       (format *stream* "~VT~A.add~%" (get-ind) wtyp))
      ;; -
      ((eq op :minus)
       (format *stream* "~VT~A.sub~%" (get-ind) wtyp))
      ;; *
      ((eq op :times)
       (format *stream* "~VT~A.mul~%" (get-ind) wtyp))
      ;; /
      ((eq op :divide)
       (format *stream* "~VT~A.div_s~%" (get-ind) wtyp))
      ;; %
      ((eq op :modulo)
       (format *stream* "~VT~A.rem_s~%" (get-ind) wtyp)))))

;;; Emit Expressions
(defun emit-expression (expr)
  (cond
    ;; Number Literal
    ((typep expr 'number-literal)
     (format *stream* "~VT~A.const ~A~%" (get-ind)
	     (get-wasm-type (expr-type expr))
	     (number-value expr)))
    ;; Identifier
    ((typep expr 'identifier)
     (format *stream* "~VT~A.get $~A~%" (get-ind)
	     (get-scope-string (id-scope expr)) (id-symbol expr)))
    ;; Binary Expression
    ((typep expr 'binary-expression)
     (emit-expression (binary-lhs expr))
     (emit-expression (binary-rhs expr))
     (emit-binary-op (binary-op expr) :int)))
     ;;(emit-binary-op (binary-op expr) (expr-type (binary-lhs expr)))))
    ;; Unary Expression
  )

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


(defun emit-procedure-constants (consts)
  (when consts
    (dolist (c consts)
      (let* ((sym  (const-symbol c))
	     (ptyp (const-type   c))
	     (val  (const-value  c))
	     (wtyp (get-wasm-type ptyp)))
	(format *stream* "~VT(local $~A ~A)~%" (get-ind) sym wtyp)
	(format *stream* "~VT~A.const ~A~%" (get-ind) wtyp val)
	(format *stream* "~VTlocal.set $~A~%" (get-ind) sym)))))

(defun emit-procedure-variables (vars)
  (when vars
    (dolist (v vars)
      (let* ((sym  (var-symbol v))
	     (ptyp (var-type   v))
	     (wtyp (get-wasm-type ptyp)))
	(format *stream* "~VT(local $~A ~A)~%" (get-ind) sym wtyp)))))

(defun invert-operator (condition)
  "The operator in 'While <cond> ...' must be inverted
for the WASM '<cond> br_if' style of looping."
  (setf (cond-op condition)
	(cadr (assoc (cond-op condition) +op-inversions+)))
  condition)

(defun emit-cond-statements (c &key (invert nil))
  (if invert (invert-operator c))
  (let ((lhs (cond-lhs c))
	(rhs (cond-rhs c))
	(op  (cond-op c)))
    (if (typep lhs 'conditional-expression)
	(emit-cond-statements lhs :invert invert)
	(emit-expression lhs))
    (if (typep rhs 'conditional-expression)
	(emit-cond-statements rhs :invert invert)
	(emit-expression rhs))
    (indent)
    (format *stream* "~VT~A~%" (get-ind)
	    (get-op-code-string (expr-type lhs) op))
    (outdent)))

(defun emit-case-jump-table (c num sel)
  (indent)
  (emit-expression sel)
  (format *stream* "~VTi32.const ~A~%" (get-ind) (case-label c))
  (format *stream* "~VTi32.eq~%" (get-ind))
  (format *stream* "~VTbr_if $case~A~%~%" (get-ind) num)
  (outdent))  
	     
(defun emit-case-body (c)
  (indent)
  (emit-statements (case-body c))
  ;;(format *stream* "~VTbr $switch_~A~%" (get-ind) sym)
  (format *stream* "~VT)~%" (get-ind))
  (outdent))


(defun emit-switch-statement (stmnt)
     (let* ((sym (switch-label stmnt))
	    (cases (switch-cases stmnt))
	    (len (length cases))
	    (expr (switch-selector stmnt)))
       (format *stream* "~VT(block ~A~%" (get-ind) sym)
       (indent)
       (format *stream* "~VT(block $default_~A~%" (get-ind) sym)
       (indent)
       (loop for num downfrom len downto 1
	     do (format *stream* "~VT(block $case~A~%" (get-ind) num))
       (loop for c in cases
	     for num from 1 to len 
	     do (emit-case-jump-table c num expr))
       (format *stream* "~VTbr $default_~A~%" (get-ind) sym)
       (outdent)
       (format *stream* "~VT)~%" (get-ind))
       (outdent)
       (loop for c in cases
	     do (emit-case-body c))
       (emit-statements (switch-default stmnt))
       (format *stream* "~VT)~%" (get-ind))))


(defun emit-statements (stmnt-node)
  (cond
    ;; Compound Statements
    ((typep stmnt-node 'compound-statement)
     (indent)
     (dolist (s (cmpnd-stmnts stmnt-node))
       (emit-statements s))
     (outdent))
    ;; Assign Statement
    ((typep stmnt-node 'assign-statement)
     (let* ((lhs-id    (assign-var stmnt-node))
	    (rhs-expr  (assign-expr stmnt-node))
	    (lhs-scope (id-scope lhs-id)))
       (emit-expression rhs-expr)
       (format *stream* "~VT~A.set $~A~%" (get-ind)
	       (if (eq lhs-scope :local) "local" "global")
	       (id-symbol lhs-id))))
    ;; IF .. THEN statement
    ((typep stmnt-node 'if-statement)
     (let ((condition (if-cond stmnt-node))
	   (then-stmnts (if-conseq stmnt-node))
	   (else-stmnts (if-else stmnt-node)))
       (emit-cond-statements condition)
       (format *stream* "~VT(if~%" (get-ind))
       (format *stream* "~VT(then~%" (get-ind))
       (indent)
       (emit-statements then-stmnts)
       (outdent)
       (format *stream* "~VT)~%" (get-ind))
       (if else-stmnts
	   (progn
	     (format *stream* "~VT(else~%" (get-ind))
	     (emit-statements else-stmnts)
	     (format *stream* "~VT)~%" (get-ind))))
       (format *stream* "~VT)~%" (get-ind))))
    ;; While Statement
    ((typep stmnt-node 'while-statement)
     (let ((condition (while-cond stmnt-node))
	   (body (while-body stmnt-node))
	   (sym  (while-label stmnt-node)))
       (format *stream* "~VT(block ~A~%" (get-ind) sym)
       (indent)
       (format *stream* "~VT(loop $loop_~A~%" (get-ind) sym)
       (indent)
       (emit-cond-statements condition :invert t)
       (format *stream* "~VTbr_if ~A~%" (get-ind) sym)
       (emit-statements body)
       (outdent)
       (format *stream* "~VTbr $loop_~A~%" (get-ind) sym)
       (outdent)
       (format *stream* "~VT))~%" (get-ind))))
    ;; Break
    ((typep stmnt-node 'break-statement)
     (format *stream* "~VTbr ~A~%" (get-ind) (break-label stmnt-node)))
    ;; Switch ... Case Statements
    ((typep stmnt-node 'switch-statement)
     (emit-switch-statement stmnt-node))
    ;; Write <Expressions>
    ((typep stmnt-node 'write-statement)
     (cond
       ;; Write Newline
       ((write-nl stmnt-node)
	(format *stream* "~VTcall $write_newline~%" (get-ind)))
       ;; Write String
       ((eq (expr-type (write-expr stmnt-node)) :string)
	(let* ((name (id-symbol (write-expr stmnt-node)))
	       (str-data (cadr (assoc name *string-addresses* :test #'string=)))
	       (str-len  (car str-data))
	       (str-addr (cdr str-data)))
	  (format *stream* "~VT(call $write_string (i32.const ~A) (i32.const ~A))~%" (get-ind)
		  str-addr str-len)))
       ;; Write Result of Math/Logic Expressions
       (t 
	(progn
	  (emit-expression (write-expr stmnt-node))
	  (format *stream* "~VTcall $write_i32~%" (get-ind))))))
    ;; Call Procedure Statement
    ((typep stmnt-node 'call-statement)
     (format *stream* "~VTcall $~A~%" (get-ind) (call-proc-name stmnt-node)))))

(defun emit-procedures (procs)
  (dolist (p procs)
    (format *stream* "~%~VT(func $~A~%" (get-ind)
	    (proc-symbol p))
    (let ((b (proc-body p)))
      (emit-procedure-constants (block-consts b))
      (emit-procedure-variables (block-vars   b))
      (emit-statements (block-body b)))
    (format *stream* "~VT)~%~%" (get-ind))))

(defun emit-main-procedure (body)
  (format *stream* "~VT(func $main (export \"_start\")~%" (get-ind))
  (indent)
  (emit-statements body)
  (format *stream* "~VT)~%~%" (get-ind)))

(defun emit-end-program ()
  (outdent)
  (format *stream* "~VT)~%"(get-ind)))

(defun emit-wasm (ast fn)
  (setf *string-addresses* '())
  (with-open-file (*stream* fn
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format t "Emitting WASM...~%")
    (let* ((pb (program-block ast))
	   (procs (collect-procedures pb)))
      ;; Start Program
      (emit-program)
      ;; Constants
      (emit-global-consts
       (block-consts pb))
      ;; Global Variables
      (emit-global-vars	
       (block-vars pb))
      ;; Procedures
      (emit-procedures procs)
      ;; Main Procedure
      (emit-main-procedure (block-body pb))
      ;; End Program
      (emit-end-program))))
