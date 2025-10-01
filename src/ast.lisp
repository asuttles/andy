(defpackage :andy.ast
  (:use :cl)
  (:export :generate-ast))

(in-package :andy.ast)


;;; Base Classes for AST Nodes
(defclass ast-node () ())

(defclass statement (ast-node) ())

(defclass expression (ast-node) ())


;;; PROGRAM Structure
(defclass program (ast-node)
  ((block :initarg :block :accessor program-block)))

(defclass program-block (ast-node)
  ((constants  :initarg :consts :accessor block-consts :initform '())
   (variables  :initarg :vars   :accessor block-vars   :initform '())
   (procedures :initarg :procs  :accessor block-procs  :initform '())
   (body       :initarg :body   :accessor block-body    :initform '())))

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
  ((symbol :initarg :symbol :accessor id-symbol)))

(defclass number-literal (expression)
  ((value  :initarg :value :accessor number-value)
   (type   :initarg :type  :accessor number-type)))

(defclass binary-expression (expression)
  ((left  :initarg :left  :accessor binary-left)
   (op    :initarg :op    :accessor binary-op)
   (right :initarg :right :accessor binary-right)))

(defclass unary-expression (expression)
  ((op         :initarg :op   :accessor unary-op)
   (expression :initarg :expr :accessor unary-expr)))
