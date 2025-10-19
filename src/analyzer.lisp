(defpackage :andy.analyzer
  (:use :cl :andy.ast :andy.runtime)
  (:export
   :analyze-ast))

(in-package :andy.analyzer)

(defstruct abstract-symbol
  name					; string or symbol
  kind					; :const, :var, :procedure
  type					; Future Expansion
  value					; for constants
  params				; for procedures
)

(defparameter *symbol-table* nil
  "A stack of scopes. Each scope is a list of symbols.
The list on top of the stack is the innermost scope.")


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

(defun make-symbol-entry (name kind &key type value params)
  "Helper to create a symbol struct with the current scope level."
  (make-abstract-symbol :name name
               :kind kind
	       :type type
               :value value
               :params params))

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

;;; Number, Identifier, and Expressions
(defun analyze-expression (e)
  (cond
    ;; Number Literal
    ((typep e 'number-literal)
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
    ;; Binary Expression
    ((typep e 'binary-expression)
     (let ((ltype (analyze-expression (binary-lhs e)))
           (rtype (analyze-expression (binary-rhs e)))
           (op (binary-op e)))
       ;; arithmetic ops require numeric types (int for now)
       (cond
         ((member op '(:plus :minus :times :divide :modulo))
          (unless (and (eq ltype :int) (eq rtype :int))
            (error "Arithmetic operator ~A requires integers, got ~A and ~A" op ltype rtype))
          :int)
         (t (error "Semantic Error: Unknown binary op ~A" op)))))
    ;; Unary Expression
    ((typep e 'unary-expression)
     (let ((typ (analyze-expression (unary-expr e)))
           (op (unary-op e)))
       (cond
         ((eq op :minus)
          (unless (eq typ :int) (error "Unary - requires integer"))
          :int)
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
    (unless (member op '(:eql :neq :lss :leq :gtr :geq))
      (error "Semantic Error: Conditional expression has unknown operator: ~A" op))
    ;; Type Checking
    (let ((ltype (analyze-expression (cond-lhs c)))
	  (rtype (analyze-expression (cond-rhs c))))
      (unless (eq ltype rtype)
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
  (let ((name (var-symbol v)) (type (var-type v)))
    (let ((sym (make-abstract-symbol :name name :kind :var :type type :value nil)))
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
	  ((and (null nl) (not (or (eq typ :int) (eq typ :string))))
	   (error "Semantic Error: Only integer writes are permitted!~%")))))


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
     (let ((condition (while-cond stmt)))
       (analyze-condition condition)
       (analyze-statement (while-body stmt))))

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

  ;; PROCEDURES -- two pass:

  ;; Insert procedure name symbols into current scope
  (dolist (p (block-procs block-node))
    (let* ((name (proc-symbol p))
           (sym (make-symbol-entry name :procedure)))
      (add-symbol sym)))

  ;; Now analyze procedure bodies
  ;; With symbol in local scope, recursion is possible
  (dolist (p (block-procs block-node))
    (analyze-procedure p))

  ;; Code BODY (begin ... end)
  (analyze-statement (block-body block-node)))


;;; Program Analysis
(defun analyze-program (prog-node)
  (let ((block (program-block prog-node)))
    (analyze-block block)))

;;; Perform Symantic Analysis and Build Symbol Table
(defun analyze-ast (ast)
  "Semantic analysis entry point."
  (setq *symbol-table* nil)
  (enter-scope)		
  (analyze-program ast)
  (leave-scope))
