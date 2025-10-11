(defpackage :andy.ast
  (:use :cl)
  (:export
   ;; Base classes
   :ast-node :statement :expression

   ;; Program structure
   :program :program-block

   ;; Declarations
   :constant-declaration :variable-declaration :procedure-declaration

   ;; Statements
   :compound-statement :assign-statement :call-statement
   :if-statement :while-statement :read-statement :write-statement

   ;; Expressions
   :identifier :number-literal :conditional-expression
   :binary-expression :unary-expression

   ;; Accessors (slot readers)
   :expr-type
   :program-block
   :block-consts :block-vars :block-procs :block-body
   :const-symbol :const-value :const-type
   :var-symbol :var-type
   :proc-symbol :proc-body
   :cmpnd-stmnts
   :assign-var :assign-expr
   :call-proc-name
   :if-cond :if-conseq
   :while-cond :while-body
   :read-var
   :write-expr
   :id-symbol :id-binding :id-scope
   :number-value
   :cond-lhs :cond-op :cond-rhs
   :binary-lhs :binary-op :binary-rhs
   :unary-op :unary-expr))

(in-package :andy.ast)

;;; Base Classes for AST Nodes
(defclass ast-node () ())

(defclass statement (ast-node) ())

(defclass expression (ast-node)
  ((type :initarg :type :accessor expr-type)))


;;; PROGRAM Structure
(defclass program (ast-node)
  ((block :initarg :block :accessor program-block)))

(defclass program-block (ast-node)
  ((constants  :initarg :consts :accessor block-consts :initform '())
   (variables  :initarg :vars   :accessor block-vars   :initform '())
   (procedures :initarg :procs  :accessor block-procs  :initform '())
   (body       :initarg :body   :accessor block-body   :initform '())))

;;; Declarations
(defclass constant-declaration (ast-node)
  ((symbol  :initarg :symbol :accessor const-symbol)
   (value   :initarg :value  :accessor const-value)
   (type    :initarg :type   :accessor const-type)))

(defclass variable-declaration (ast-node)
  ((symbol :initarg :symbol :accessor var-symbol)
   (type   :initarg :type   :accessor var-type)))

(defclass procedure-declaration (ast-node)
  ((symbol :initarg :symbol :accessor proc-symbol)
   (body   :initarg :body  :accessor proc-body)))


;;; STATEMENTS
(defclass compound-statement (statement)
  ((statements :initarg :stmnts :accessor cmpnd-stmnts :initform '())))

(defclass assign-statement (statement)
  ((variable   :initarg :var  :accessor assign-var)
   (expression :initarg :expr :accessor assign-expr)))

(defclass call-statement (statement)
  ((name :initarg :name :accessor call-proc-name)))

(defclass if-statement (statement)
  ((condition   :initarg :cond   :accessor if-cond)
   (consequence :initarg :conseq :accessor if-conseq)))

(defclass while-statement (statement)
  ((condition :initarg :cond :accessor while-cond)
   (body      :initarg :body :accessor while-body)))

(defclass read-statement (statement)
  ((variable :initarg :var :accessor read-var)))

(defclass write-statement (statement)
  ((expression :initarg :expr :accessor write-expr)))


;;; EXPRESSIONS
(defclass identifier (expression)
  ((symbol  :initarg :symbol  :accessor id-symbol)
   ;; Binding slot contains pointer to objects in symbol table
   (binding :initarg :binding :accessor id-binding :initform nil)
   ;; Scope is :local or :global
   (scope   :initarg :scope   :accessor id-scope   :initform nil)))

(defclass number-literal (expression)
  ((value  :initarg :value :accessor number-value)))

(defclass conditional-expression (expression)
  ((lhs :initarg :lhs :accessor cond-lhs)
   (op  :initarg :op  :accessor cond-op)
   (rhs :initarg :rhs :accessor cond-rhs)))

(defclass binary-expression (expression)
  ((lhs :initarg :lhs :accessor binary-lhs)
   (op  :initarg :op  :accessor binary-op)
   (rhs :initarg :rhs :accessor binary-rhs)))

(defclass unary-expression (expression)
  ((op         :initarg :op   :accessor unary-op)
   (expression :initarg :expr :accessor unary-expr)))
