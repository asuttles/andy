(defpackage :andy.ast
  (:use :cl)
  (:export
   ;; Abstract Symbols for Semantic Analysis
   :make-abstract-symbol :abstract-symbol-name :abstract-symbol-kind
   :abstract-symbol-type :abstract-symbol-value :abstract-symbol-params
   :make-symbol-entry

   ;; Base classes
   :ast-node :statement :expression

   ;; Program structure
   :program :program-block :program-type

   ;; Declarations
   :constant-declaration :variable-declaration :procedure-declaration :function-declaration

   ;; Statements
   :compound-statement :assign-statement :call-statement :return-statement
   :if-statement :read-statement :write-statement
   :while-statement :for-statement
   :switch-statement :break-statement :case-statement 
   
   ;; Expressions
   :identifier :number-literal :conditional-expression
   :function-call :binary-expression :unary-expression

   ;; Accessors (slot readers)
   :expr-type
   :program-block
   :block-consts :block-vars :block-procs :block-funcs :block-body
   :const-symbol :const-value :const-type
   :var-symbol :var-type :var-kind :var-size :var-addr
   :proc-symbol :proc-body
   :func-symbol :func-params :func-type :func-body
   :funcall-symbol :funcall-binding :funcall-args
   :cmpnd-stmnts
   :assign-var :assign-expr
   :call-proc-name :return-expr
   :if-cond :if-conseq :if-else
   :while-label :while-cond :while-body
   :switch-label :switch-selector :switch-cases :switch-default
   :case-label :case-body
   :break-label
   :for-label :for-init :for-cont :for-iter :for-body
   :read-var
   :write-expr :write-nl
   :id-symbol :id-binding :id-scope :id-index
   :number-value
   :cond-lhs :cond-op :cond-rhs
   :binary-lhs :binary-op :binary-rhs
   :unary-op :unary-expr))

(in-package :andy.ast)


(defstruct abstract-symbol
  name					; string or symbol
  kind					; :const, :var, :procedure
  type					; Future Expansion
  value					; for constants
  params				; for procedures
  )

(defun make-symbol-entry (name kind &key type value params)
  "Helper to create a symbol struct with the current scope level."
  (make-abstract-symbol :name name
			:kind kind
			:type type
			:value value
			:params params))


;;; Base Classes for AST Nodes
(defclass ast-node () ())

(defclass statement (ast-node) ())

(defclass expression (ast-node)
  ((type :initarg :type :accessor expr-type)))


;;; PROGRAM Structure
(defclass program (ast-node)
  ((block :initarg :block :accessor program-block)
   (type  :initarg :type  :accessor program-type)))

(defclass program-block (ast-node)
  ((constants  :initarg :consts :accessor block-consts :initform '())
   (variables  :initarg :vars   :accessor block-vars   :initform '())
   (procedures :initarg :procs  :accessor block-procs  :initform '())
   (functions  :initarg :funcs  :accessor block-funcs  :initform '())
   (body       :initarg :body   :accessor block-body   :initform '())))

;;; Declarations
(defclass constant-declaration (ast-node)
  ((symbol  :initarg :symbol  :accessor const-symbol)
   (value   :initarg :value   :accessor const-value)
   (type    :initarg :type    :accessor const-type)))

(defclass variable-declaration (ast-node)
  ((symbol :initarg :symbol :accessor var-symbol)
   (type   :initarg :type   :accessor var-type)
   ;; Kind = :scalar or :array
   (kind    :initarg :kind  :accessor var-kind :initform :scalar)
   (size    :initarg :size  :accessor var-size :initform 1)
   ;; Heap Address, if applicable
   (address :initarg :addr  :accessor var-addr)))
   
(defclass procedure-declaration (ast-node)
  ((symbol :initarg :symbol :accessor proc-symbol)
   (body   :initarg :body  :accessor proc-body)))

(defclass function-declaration (ast-node)
  ((symbol :initarg :symbol :accessor func-symbol)
   (params :initarg :params :accessor func-params :initform '())
   (type   :initarg :type   :accessor func-type)
   (body   :initarg :body   :accessor func-body)))


;;; STATEMENTS
(defclass compound-statement (statement)
  ((statements :initarg :stmnts :accessor cmpnd-stmnts :initform '())))

(defclass assign-statement (statement)
  ((variable   :initarg :var  :accessor assign-var)
   (expression :initarg :expr :accessor assign-expr)))

(defclass call-statement (statement)
  ((name :initarg :name :accessor call-proc-name)))

(defclass return-statement (statement)
  ((expr :initarg :expr :accessor return-expr)))

(defclass if-statement (statement)
  ((condition        :initarg :cond   :accessor if-cond)
   (consequence      :initarg :conseq :accessor if-conseq)
   (else-consequence :initarg :else   :accessor if-else :initform '())))

(defclass while-statement (statement)
  ((label     :initarg :label :accessor while-label :initform nil)
   (condition :initarg :cond :accessor while-cond)
   (body      :initarg :body :accessor while-body)))

(defclass for-statement (statement)
  ((label          :initarg :label :accessor for-label :initform nil)
   (initialization :initarg :init  :accessor for-init)
   (condition      :initarg :cont  :accessor for-cont)
   (iteration      :initarg :iter  :accessor for-iter)
   (body           :initarg :body  :accessor for-body)))
  
(defclass break-statement (statement)
  ((break-label :initarg :lbl :accessor break-label :initform nil)))

(defclass read-statement (statement)
  ((variable :initarg :var :accessor read-var)))

(defclass write-statement (statement)
  ((expression :initarg :expr :accessor write-expr)
   (newline-p  :initarg :nl   :accessor write-nl :initform nil)))

(defclass switch-statement (statement)
  ((label           :initarg :label    :accessor switch-label :initform nil)
   (selector        :initarg :selector :accessor switch-selector)
   (case-statements :initarg :cases    :accessor switch-cases   :initform '())
   (default         :initarg :default  :accessor switch-default :initform '())))

(defclass case-statement (statement)
  ((label  :initarg :label :accessor case-label)
   (body   :initarg :body  :accessor case-body :initform '())))

;;; EXPRESSIONS
(defclass identifier (expression)
  ((symbol  :initarg :symbol  :accessor id-symbol)
   ;; Binding slot contains pointer to objects in symbol table
   (binding :initarg :binding :accessor id-binding :initform nil)
   ;; Scope is :local or :global
   (scope   :initarg :scope   :accessor id-scope   :initform nil)
   (index   :initarg :index   :accessor id-index   :initform nil)))

(defclass function-call (expression)
  ((symbol    :initarg :symbol  :accessor funcall-symbol)
   (binding   :initarg :binding :accessor funcall-binding)
   (arguments :initarg :args    :accessor funcall-args :initform '())))

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
