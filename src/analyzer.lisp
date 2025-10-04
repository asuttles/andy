;;; TODO
;;; Write analyze-assignment and analyze-call
;;; Finish up analyze-condition and test


(defpackage :andy.analyzer
  (:use :cl)
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

;;; -----------------------------------------------------------------
;;; Perform semantic Analysis of AST and Build Symbol Table
;;; -----------------------------------------------------------------

;;; Constant Analysis
(defun analyze-constant-decl (c)
  "C is a constant-declaration AST node with slots: const-symbol, const-value, const-type"
  (let ((name (const-symbol c))
        (val (const-value c))
        (type (const-type c)))
    (unless (eq type :int)
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

;;;     (analyze-assignment stmt))
;;;     (analyze-call stmt))


;;; Condition Analysis
(defun analyze-condition (c)
  "Analyze a logical condition expression AST node."
  ;; conditional expression: expr1 <op> expr2
  ;; analyze sub-expressions
  (let ((op (cond-op c)))
    (unless (member op '(:eql :neq :lss :leq :gtr :geq))
      (error "Semantic Error: Conditional expression has unknown operator: ~%" op))
    (analyze-expression (cond-lhs c))
    (analyze-expression (cond-rhs c)))
  ;; type checking
  (unless (eq (expression-type (condition-left node))
              (expression-type (condition-right node)))
    (error "Type mismatch in condition"))

  
;;; Statement Analysis
(defun analyze-statement (stmt)
  (cond
    ;; Recurse through nested blocks of statements...
    ((typep stmt 'compound-statement)
     (dolist (s (cmpnd-stmnts stmt))
       (analyze-statement s)))
    ;; Assign Statement
    ((typep stmt 'assign-statement)
     ;;(analyze-assignment stmt)
     (print "Assignment"))
    ;; Procedure Call Statement
    ((typep stmt 'call-statement)
     ;;(analyze-call stmt))
     (print "Call Statement"))
    ;; If Statement
    ((typep stmt 'if-statement)
     (let ((condition (if-cond stmt)))
       ;;(analyze-condition condition)
       (print condition)
       (analyze-statement (if-conseq stmt))))
    ;; While Statement
    ((typep stmt 'while-statement)
     (let ((condition (while-cond stmt)))
       ;;(analyze-condition condition)
       (print condition)
       (analyze-statement (while-body stmt))))
    ;; Ill-formed Statement
    (t
     (error "Semantic Error: Unknown statement type: ~A" (class-of stmt)))))


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
))

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
