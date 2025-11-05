(defpackage :andy.analyzer
  (:use :cl :andy.ast :andy.builtin)
  (:export
   :analyze-ast))

(in-package :andy.analyzer)


(defvar *symbol-table* nil
  "A stack of scopes. Each scope is a list of symbols.
The list on top of the stack is the innermost scope.")

(defvar *loop-stack* nil
  "A stack of loop blocks to track local jumps from break statements.")

(defvar *function-stack* nil
  "A stack of procedure contexts for use by return function analysis.")

;;; ─── Scope Management ──────────────────────────────────────────────

(defun enter-scope ()
  "Push a new, empty scope onto the symbol table stack."
  (push '() *symbol-table*))

(defun leave-scope ()
  "Pop the innermost scope from the stack."
  (pop *symbol-table*))

(defun current-scope ()
  "Return the current innermost scope."
  (first *symbol-table*))

;;; ─── Symbol Creation and Insertion ────────────────────────────────

(defun add-symbol (sym)
  "Insert a SYMBOL object into the current (innermost) scope.
Raises an error if the name is already defined in this scope."
  (unless *symbol-table*
    (error "Semantic Analyzer Error: No active scope. Call ENTER-SCOPE first.~%"))
  (let ((name (abstract-symbol-name sym)))
    (when (symbol-defined-in-current-scope-p name)
      (error "Semantic Analyzer Error: ~A is already defined in this scope.~%" name)))
  (push sym (first *symbol-table*)))


;;; ─── Symbol Lookup ────────────────────────────────────────────────

(defun symbol-defined-in-current-scope-p (name)
  "Check if NAME is defined in the current scope."
  (when *symbol-table*
    (find name (first *symbol-table*)
          :key #'abstract-symbol-name :test #'equal)))

(defun lookup-symbol (name)
  "Look up NAME from innermost scope outward."
  (dolist (scope *symbol-table* nil)
    (let ((sym (find name scope :key #'abstract-symbol-name :test #'equal)))
      (when sym (return sym)))))

(defun get-local-or-global (name)
  (if (and (symbol-defined-in-current-scope-p name)
	   (> (length *symbol-table*) 1))
      :local :global))

;;; -----------------------------------------------------------------
;;; Perform semantic Analysis of AST and Build Symbol Table
;;;   - Expressions
;;;   - Statements
;;;   - Program/Block
;;; -----------------------------------------------------------------

;;; ─── Expression Analysis ─────────────────────────────────────────

;;; Function Call Analysis
(defun analyze-funcall (f)
  (let* ((name (funcall-symbol f))
	 (symbol (or
		  (lookup-symbol name)	     ; User Defined Function 
		  (lookup-builtin name))))   ; Built-In Function
    (setf (funcall-binding f) symbol)
    ;; Check function scope
    (unless symbol
      (error "Semantic Error: Call of func ~A not in scope or not defined." name))
    ;; Match arguments with function parameters
    (let ((arg-types (mapcar #'analyze-expression (andy.ast:funcall-args f)))
	  (param-types (mapcar #'expr-type (abstract-symbol-params symbol))))
      (unless (equal arg-types param-types)
	(error "Semantic Error: Call func ~A has wrong argument types." name)))
    ;; Return the function type
    (setf (expr-type f) (abstract-symbol-type symbol))))

;;; Number, Identifier, and Expressions
(defun analyze-expression (e)
  (cond
    ;; Number Literal
    ((typep e 'number-literal)
     (unless (member (expr-type e) '(:int :float))
       (error "Unknown type for number literal: ~A~%"
	      (number-value e)))
     (expr-type e))
    ;; Identifier
    ((typep e 'identifier)
     (let ((name (id-symbol e)))
       (let ((sym (lookup-symbol name)))
         (unless sym
	   (error "Semantic Error: Use of undeclared identifier ~A" name))
         ;; Annotate AST identifier node with binding and scope
         (setf (id-binding e) sym)
	 (setf (id-scope e) (get-local-or-global name))
	 (setf (expr-type e) (abstract-symbol-type sym)))))
    ;; Function Call
    ((typep e 'function-call)
     (analyze-funcall e))
    ;; Binary Expression
    ((typep e 'binary-expression)
     (let ((ltype (analyze-expression (binary-lhs e)))
           (rtype (analyze-expression (binary-rhs e)))
           (op (binary-op e)))
       ;; arithmetic ops require numeric types
       (cond
         ((member op '(:plus :minus :times :divide :modulo))
          (if (eq ltype rtype)
	      (setf (expr-type e) ltype)
              (error "Operator ~A requires matching types, got ~A and ~A" op ltype rtype)))
         (t (error "Semantic Error: Unknown binary op ~A" op)))))
    ;; Unary Expression
    ((typep e 'unary-expression)
     (let ((typ (analyze-expression (unary-expr e)))
           (op (unary-op e)))
       (cond
         ((eq op :minus)
          (unless (member typ '(:int :float)) (error "Unary - requires float or integer"))
          typ)
         ((eq op :plus)
          typ)
         (t (error "Unknown unary op ~A" op)))))
    (t (error "Unknown expression node: ~A" (class-of e)))))

;;; Analyze Conditional Expressions
(defun analyze-condition (c)
  "Analyze a logical condition expression AST node."
  ;; conditional expression: expr1 <op> expr2
  ;; analyze sub-expressions
  (let ((op  (cond-op c)))
    ;; Logic Operator Check
    (unless (member op '(:eql :neq :lss :leq :gtr :geq :or :and :xor))
      (error "Semantic Error: Conditional expression has unknown operator: ~A" op))
    ;; Type Checking
    (let* ((lhs (cond-lhs c))
	   (rhs (cond-rhs c))
	   (ltype (if (typep lhs 'conditional-expression)
		      (analyze-condition lhs)
		      (analyze-expression lhs)))
	   (rtype (if (typep rhs 'conditional-expression)
		      (analyze-condition rhs)
		      (analyze-expression rhs))))
      (if (eq ltype rtype)
	  (setf (expr-type c) ltype)
	  (error "Type mismatch for conditional expression ~A ~A ~A"
		 ltype op rtype))
      ltype)))

;;; ─── Statement Analysis ──────────────────────────────────────────-

;;; Constant Analysis
(defun analyze-constant-decl (c)
  "C is a constant-declaration AST node with slots: const-symbol, const-value, const-type"
  (let ((name (const-symbol c))
        (val (const-value c))
        (type (const-type c)))
    (unless (member type '(:int :float :string))
      (error "Semantic Error: Unrecognized type (~A) for const: ~A~%" type name))
    (let ((sym (make-abstract-symbol :name name :kind :const :type type :value val)))
      (add-symbol sym))))

;;; Variable Analysis
(defun analyze-variable-decl (v)
  "V is variable-declaration node with var-symbol and var-type"
  (let ((name (var-symbol v)) (type (var-type v)) (kind (var-kind v)) (addr (var-addr v)))
    (let ((sym (make-abstract-symbol :name name :kind kind :type type :value addr)))
      (add-symbol sym))))

;;; Procedure Analysis
(defun analyze-procedure (p)
  "proc-node has slots :symbol (name) and :body (a block AST)."
  (let ((name (proc-symbol p)))
    ;; Verify P is in local scope - allows recursion
    (let ((proc-sym (lookup-symbol name)))
      (unless (and proc-sym (eq (abstract-symbol-kind proc-sym) :procedure))
        (error "Semantic Error: Procedure ~A not predeclared" name))
      (enter-scope)
      ;; Enter parameters into local scope
      ;; Analyze nested declarations and statements inside its body:
      (analyze-block (proc-body p))
      (leave-scope))))

;;; Function Definition Analysis
(defun analyze-function (f)
  (let* ((name (func-symbol f))
	 (sym (lookup-symbol name)))
    (unless (and sym (eq (abstract-symbol-kind sym) :function))
      (error "Semantic Error: Function ~A not predeclared" name))
    (push sym *function-stack*)
    (enter-scope)
    (dolist (p (func-params f))
      (add-symbol (make-abstract-symbol :name (id-symbol p)
					:kind :param
					:type (expr-type p)
					:value nil)))
    (analyze-block (func-body f))
    (pop *function-stack*)
    (leave-scope)))

;;; Analyze Function Return
(defun analyze-return (r)
  (let* ((scope (car *function-stack*))
	 (func-type
	  ;; Program or Function Return
	   (if (= (length *function-stack*) 1)
	       scope 			; Main Program 
	       (abstract-symbol-type	; Function
		scope)))
	 (ret-expr (return-expr r)))
    (analyze-expression ret-expr)
    (unless (eq func-type (expr-type ret-expr))
      (error "Error: Function ~A returns wrong type: ~A"
	     (abstract-symbol-name (car *function-stack*))
	     (expr-type ret-expr)))))

;;;  Assignment Statements
(defun analyze-assignment (a)
  (let* ((id-node (assign-var a))
	 (name (id-symbol id-node))
	 (sym (lookup-symbol name))
	 (scope (get-local-or-global name)))
    (unless sym
      (error "Semantic Error: Assignment to undeclared identifier ~A" name))
    (when (eq (abstract-symbol-kind sym) :const)
      (error "Semantic Error: Assignment to constant ~A" name))
    ;; Type-check RHS against var type
    (let ((rhs-type (analyze-expression (assign-expr a)))
	  (lhs-type (abstract-symbol-type sym)))
      (if (not (eq rhs-type lhs-type))
          (error "Semantic Error: Type mismatch assigning to ~A: got ~A, expected ~A"
                 name rhs-type lhs-type)
	  (setf (expr-type id-node) lhs-type)))
    (setf (id-scope id-node) scope)
    (setf (id-binding id-node) sym)))

;;; Procedure Calls
(defun analyze-call (call)
  (let ((name (call-proc-name call)))
    (let ((sym (lookup-symbol name)))
      (unless sym (error "Semantic Error: Call to undeclared procedure ~A~%" name))
      (unless (eq (abstract-symbol-kind sym) :procedure)
	(error "Semantic Error: Call to a symbol (~A) that is not a procedure~%" name))
      ;; NOTE:
      ;; Check Procedure Arguments HERE
      )))

;;; Write to Console
(defun analyze-write (stmt)
  (let* ((expr (write-expr stmt))
	 (nl  (write-nl stmt))
	 (typ (if expr
		  (analyze-expression expr)
		  nil)))
    (cond ((and nl expr)
	   (error "Semantic Error: No arguments expected for writeNL command~%"))
	  ((and (null nl) (not (member typ '(:int :float :string))))
	   (error "Semantic Error: Only floats,ints and strings can be printed.~%")))))

;;; Analyze Switch Cases
(defun analyze-cases (cases)
  (loop for c in cases do
    (if (not (integerp (case-label c)))
	(error "Case label ~A is not an integer~%" (case-label c))
	(analyze-statement (case-body c)))))

;;; Statement Analysis
(defun analyze-statement (stmt)
  (cond
    ;; Recurse through nested blocks of statements...
    ((typep stmt 'compound-statement)
     (dolist (s (cmpnd-stmnts stmt))
       (analyze-statement s)))

    ;; Assign Statement
    ((typep stmt 'assign-statement)
     (analyze-assignment stmt))

    ;; Procedure Call Statement
    ((typep stmt 'call-statement)
     (analyze-call stmt))

    ;; Return Statement
    ((typep stmt 'return-statement)
     (analyze-return stmt))
    
    ;; Write Statement
    ((typep stmt 'write-statement)
     (analyze-write stmt))
    
    ;; If Statement
    ((typep stmt 'if-statement)
     (analyze-condition (if-cond stmt))
     (analyze-statement (if-conseq stmt))
     (let ((else-conseq (if-else stmt)))
       (if else-conseq
	   (analyze-statement else-conseq))))

    ;; While Statement
    ((typep stmt 'while-statement)
     (let ((condition (while-cond stmt))
	   (label (format nil "$while_~A" (gensym))))
       (analyze-condition condition)
       (setf (while-label stmt) label)
       (push label *loop-stack*)       
       (analyze-statement (while-body stmt))
       (pop *loop-stack*)))

    ;; For Statement
    ((typep stmt 'for-statement)
     (let ((init (for-init stmt))
	   (cont (for-cont stmt))
	   (iter (for-iter stmt))
	   (body (for-body stmt))
	   (label (format nil "$for_~A" (gensym))))
       (setf (for-label stmt) label)
       (analyze-assignment init)
       (analyze-condition cont)
       (analyze-assignment iter)
       (push label *loop-stack*)
       (analyze-statement body)
       (pop *loop-stack*)))
    
    ;; Switch Statement
    ((typep stmt 'switch-statement)
     (let ((selector (switch-selector stmt))
	   (cases (switch-cases stmt))
	   (default (switch-default stmt))
	   (label (format nil "$switch_~A" (gensym))))
       (analyze-expression selector)
       (setf (switch-label stmt) label)
       (push label *loop-stack*)
       (analyze-cases cases)
       (analyze-statement default)
       (pop *loop-stack*)))

    ;; Break
    ((typep stmt 'break-statement)
     (when (null *loop-stack*)
       (error "Semantic Error: Cannot break from non-loop block.~%"))
     (setf (break-label stmt) (car *loop-stack*)))
    
    ;; Ill-formed Statement
    (t
     (error "Semantic Error: Unknown statement type: ~A" (class-of stmt)))))


;;; ─── Program Analysis ────────────────────────────────────────────

;;; Block Analysis
(defun analyze-block (block-node)
  "Analyze constants, variables, procedures and body."
  
  ;; CONSTANTS
  (dolist (c (block-consts block-node))
    (analyze-constant-decl c))

  ;; VARIABLES
  (dolist (v (block-vars block-node)) 
    (analyze-variable-decl v))

  ;; PROCEDURES and FUNCTIONS -- two pass:

  ;; Insert procedure name symbols into current scope
  (dolist (p (block-procs block-node))
    (let* ((name (proc-symbol p))
           (sym (make-symbol-entry name :procedure)))
      (add-symbol sym)))

  ;; Now analyze procedure bodies
  ;; With symbol in local scope, recursion is possible
  (dolist (p (block-procs block-node))
    (analyze-procedure p))

  ;; Insert Function name symbols into current scope
  (dolist (f (block-funcs block-node))
    (let* ((name (func-symbol f))
	   (sym (make-symbol-entry name :function
				   :type (func-type f)
				   :params (func-params f))))
      (add-symbol sym)))

  ;; Analyze Function Body
  (dolist (f (block-funcs block-node))
    (analyze-function f))
  
  ;; Code BODY (begin ... end)
  (analyze-statement (block-body block-node)))


;;; Program Analysis
(defun analyze-program (prog-node)
  (let ((block (program-block prog-node)))
    (push (program-type prog-node) *function-stack*)
    (analyze-block block)))

;;; Perform Symantic Analysis and Build Symbol Table
(defun analyze-ast (ast)
  "Semantic analysis entry point."
  (format t "Checking semantics .......")
  (setq *symbol-table* nil
	*loop-stack* nil
	*function-stack* nil)
  (enter-scope)		
  (analyze-program ast)
  (leave-scope)
  ast)
