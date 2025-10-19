(defpackage :andy.emitter
  (:use :cl :andy.ast :andy.runtime)
  (:export :emit-wasm))

(in-package :andy.emitter)

(defvar *stream* nil)			; Output stream


;;; Logic Operators Hash Table
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +op-table-int+
    '((:eql "i32.eq")
      (:neq "i32.ne")
      (:lss "i32.lt_s")
      (:leq "i32.le_s")
      (:gtr "i32.gt_s")
      (:geq "i32.ge_s")))
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
      (:geq :lss))))

(defparameter *string-addresses*
  '())

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

(defun emit-program ()
  "Emit Module Header and WASI IO Runtime"
  (format *stream* "(module~%~%")
  (format *stream* "~A~%~%" (get-runtime)))

(defun emit-string (str)
  (let* ((name  (const-symbol str))
	 (text (const-value str))
	 (len  (length text))
	 (addr (allocate-constant-memory len)))
    (format *stream* "  (data (i32.const ~A) ~A)~%" addr text)
    (push (list name (cons len addr)) *string-addresses*)))

(defun emit-global-consts (consts)
  (dolist (c consts)
    (let ((type (const-type c)))
      (if (eq type :string)
	  ;; Emit String Data in Runtime Memory
	  (emit-string c)
	  ;; Emit Number Constants in WASM Memory
	  (let ((wtype (get-wasm-type type)))
	    (format *stream* "  (global $~A ~A (~A.const ~A))~%"
		    (const-symbol c) wtype wtype
		    (const-value c)))))))

(defun emit-global-vars (vars)
  (dolist (v vars)
    (let* ((ptype (var-type v))
	   (wtype (get-wasm-type ptype))
	   (winit (get-wasm-initializer ptype)))
      (format *stream* "  (global $~A (mut ~A) (~A.const ~A))~%"
              (var-symbol v) wtype wtype winit)))
    (format *stream* "~%~%"))

;;; Emit Binary Operator
(defun emit-binary-op (op typ)
  (let ((wtyp (get-wasm-type typ)))
    (cond
      ;; +
      ((eq op :plus)
       (format *stream* "   ~A.add~%" wtyp))
      ;; -
      ((eq op :minus)
       (format *stream* "   ~A.sub~%" wtyp))
      ;; *
      ((eq op :times)
       (format *stream* "   ~A.mul~%" wtyp))
      ;; /
      ((eq op :divide)
       (format *stream* "   ~A.div_s~%" wtyp))
      ;; %
      ((eq op :modulo)
       (format *stream* "   ~A.rem_s~%" wtyp)))))

;;; Emit Expressions
(defun emit-expression (expr)
  (cond
    ;; Number Literal
    ((typep expr 'number-literal)
     (format *stream* "   ~A.const ~A~%"
	     (get-wasm-type (expr-type expr))
	     (number-value expr)))
    ;; Identifier
    ((typep expr 'identifier)
     (format *stream* "   ~A.get $~A~%"
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
	(format *stream* "   (local $~A ~A)~%" sym wtyp)
	(format *stream* "   ~A.const ~A~%" wtyp val)
	(format *stream* "   local.set $~A~%" sym)))))

(defun emit-procedure-variables (vars)
  (when vars
    (dolist (v vars)
      (let* ((sym  (var-symbol v))
	     (ptyp (var-type   v))
	     (wtyp (get-wasm-type ptyp)))
	(format *stream* "   (local $~A ~A)~%" sym wtyp)))))

(defun emit-cond-statements (cond)
  (let ((lhs (cond-lhs cond))
	(rhs (cond-rhs cond))
	(op  (cond-op cond)))
    (emit-expression lhs)
    (emit-expression rhs)
    (format *stream* "   ~A~%"
	    (get-op-code-string (expr-type lhs) op))))

(defun invert-operator (condition)
  "The operator in 'While <cond> ...' must be inverted
for the WASM '<cond> br_if' style of looping."
  (setf (cond-op condition)
	(cadr (assoc (cond-op condition) +op-inversions+)))
  condition)

(defun emit-statements (stmnt-node)
  (cond
    ;; Compound Statements
    ((typep stmnt-node 'compound-statement)
     (dolist (s (cmpnd-stmnts stmnt-node))
       (emit-statements s)))
    ;; Assign Statement
    ((typep stmnt-node 'assign-statement)
     (let* ((lhs-id    (assign-var stmnt-node))
	    (rhs-expr  (assign-expr stmnt-node))
	    (lhs-scope (id-scope lhs-id)))
       (emit-expression rhs-expr)
       (format *stream* "   ~A.set $~A~%"
	       (if (eq lhs-scope :local) "local" "global")
	       (id-symbol lhs-id))))
    ;; IF .. THEN statement
    ((typep stmnt-node 'if-statement)
     (let ((condition (if-cond stmnt-node))
	   (then-stmnts (if-conseq stmnt-node))
	   (else-stmnts (if-else stmnt-node)))
       (emit-cond-statements condition)
       (format *stream* "  (if~%")
       (format *stream* "     (then~%")
       (emit-statements then-stmnts)
       (format *stream* "     )~%")
       (if else-stmnts
	   (progn
	     (format *stream* "     (else~%")
	     (emit-statements else-stmnts)
	     (format *stream* "     )~%"))
	   (format t "else-stmnts = ~A~%" else-stmnts))
       (format *stream* "  )~%")))
    ;; While Statement
    ((typep stmnt-node 'while-statement)
     (let ((condition (while-cond stmnt-node))
	   (body (while-body stmnt-node))
	   (sym  (gensym)))
       (format *stream* "   (block $while_~A~%" sym)
       (format *stream* "     (loop $loop_~A~%" sym)
       (emit-cond-statements (invert-operator condition))
       (format *stream* "       br_if $while_~A~%" sym)
       (emit-statements body)
       (format *stream* "   br $loop_~A~%" sym)
       (format *stream* "   ))~%")))
    ;; Write <Expressions>
    ((typep stmnt-node 'write-statement)
     (cond
       ;; Write Newline
       ((write-nl stmnt-node)
	(format *stream* "   call $write_newline~%"))
       ;; Write String
       ((eq (expr-type (write-expr stmnt-node)) :string)
	(let* ((name (id-symbol (write-expr stmnt-node)))
	       (str-data (cadr (assoc name *string-addresses* :test #'string=)))
	       (str-len  (car str-data))
	       (str-addr (cdr str-data)))
	  (format *stream* "   (call $write_string (i32.const ~A) (i32.const ~A))~%"
		  str-addr str-len)))
       ;; Write Result of Math/Logic Expressions
       (t 
	(progn
	  (emit-expression (write-expr stmnt-node))
	  (format *stream* "   call $write_i32~%")))))
    ;; Call Procedure Statement
    ((typep stmnt-node 'call-statement)
     (format *stream* "   call $~A~%" (call-proc-name stmnt-node)))))

(defun emit-procedures (procs)
  (dolist (p procs)
    (format *stream* "~%  (func $~A~%"
	    (proc-symbol p))
    (let ((b (proc-body p)))
      (emit-procedure-constants (block-consts b))
      (emit-procedure-variables (block-vars   b))
      (emit-statements (block-body b)))
    (format *stream* "  )~%~%")))

(defun emit-main-procedure (body)
  (format *stream* "  (func $main (export \"_start\")~%")
  (emit-statements body)
  (format *stream* "  )~%~%"))

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
