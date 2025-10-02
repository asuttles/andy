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
      (error "Semantic Error: Unrecognized type (~A) for const: ~A~%" type, name))
    (let ((sym (make-abstract-symbol :name name :kind :const :type type :value val :node c)))
      (insert-symbol sym))))

;;; Variable Analysis
(defun analyze-variable-decl (v)
  "V is variable-declaration node with var-symbol and var-type"
  (let ((name (var-symbol v)) (type (var-type v)))
    (let ((sym (make-abstract-symbol :name name :kind :var :type type :value nil :node v)))
      (insert-symbol sym))))



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
  (analyze-program program-node)	
  (leave-scope))



