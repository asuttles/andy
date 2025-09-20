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

(defclass block (ast-node)
  ((constants  :initarg :consts :accessor block-consts :initarg '())
   (variables  :initarg :vars   :accessor block-vars   :initarg '())
   (procedures :initarg :procs  :accessor block-procs  :initarg '())
   (body       :initarg :body  :accessor block-body    :initarg '())))

;;; Declarations
(defclass constant-declaration (ast-node)
  ((symbol  :initarg :symbol :accessor const-symbol)
   (value   :initarg :value  :accessor const-value)))

(defclass variable-declaration (ast-node)
  ((symbol  :initarg :symbol :accessor var-symbol)))

(defclass procedure-declaration (ast-node)
  ((symbol  :initarg :symbol :accessor proc-symbol)
   (block   :initarg :block  :accessor proc-block)))


;;; STATEMENTS
(defclass compound-statement (statement)
  ((statements :initarg :stmnts :accessor cmpnd-stmnts) :initarg '()))

(defclass assign-statement (statement)
  ((variable :initarg :var :accessor assign-var)
   (value    :initarg :val :accessor assign-val)))

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
  ((symbol :initarg symbol :accessor id-symbol)))

(defclass number-literal (expression)
  ((value  :initarg value :accessor number-value)))

(defclass binary-expression (expression)
  ((left  :initarg left  :accessor binary-left)
   (op    :initarg op    :accessor binary-op)
   (right :initarg right :accessor binary-right)))

(defclass unary-expression (expression)
  ((op         :initarg op   :accessor unary-op)
   (expression :initarg expr :accessor unary-expr)))


