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




;;; Perform Symantic Analysis and Build Symbol Table
(defun analyze (ast)
  ast)

