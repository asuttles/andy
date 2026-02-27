(defpackage :andy.c-emitter
  (:use :cl :andy.ast)
  (:export :emit-c)
  (:import-from :andy.analyzer
   :abstract-symbol-kind
   :abstract-symbol-type
   :abstract-symbol-value))

(in-package :andy.c-emitter)

(defvar *stream* nil)			; Output stream


;;; Operator Associations Table
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +op-table+
    '((:eql    "==")
      (:neq    "!=")
      (:lss    "<")
      (:leq    "<=")
      (:gtr    ">")
      (:geq    ">=")
      (:or     "||")
      (:and    "&&")
      (:xor    "xor")
      (:plus   "+")
      (:minus  "-")
      (:times  "*")
      (:divide "/")
      (:modulo "%"))))

;;;			       Helpers
;;; ------------------------------------------------------------------

;;; Indent/Outdent the Output Source File
(defvar *indent* 0)

(defun indent ()
  (setf *indent* (+ *indent* 4)))

(defun outdent ()
  (unless (<= *indent* 0)
    (setf *indent* (- *indent* 4))))

(defun get-ind ()
  *indent*)

;;; Code Emitters
(defun emit-indent ()
  (write-string
   (make-string (get-ind)
                :initial-element #\Space)
   *stream*))

(defun emit (fmt &rest args)
  "Indent and print fmt string"
  (emit-indent)
  (apply #'format *stream* fmt args))

(defun emit-str (fmt &rest args)
  "Print fmt string only"
  (apply #'format *stream* fmt args))

(defun emit-str-nl (fmt &rest args)
  "Print fmt string and newline"
  (apply #'emit-str fmt args)
  (terpri *stream*))

(defun emit-line (fmt &rest args)
  "Indent, print fmt string and newline"
  (apply #'emit fmt args)
  (terpri *stream*))

(defun emit-newline ()
  (terpri *stream*))

(defun emit-funcall (f)
  ;; Get Function Call Name
  (let ((name (funcall-symbol f))
	(sym  (funcall-binding f)))
    ;; Convert to Andy Runtime Call, If Builtin
    (when (eq (abstract-symbol-kind sym) :builtin)
      (setf name (abstract-symbol-value sym)))
    ;; Emit the Function Call 
    (emit-str "~A(" name)
    (loop for arg in (funcall-args f)
	  for i from 0
	  do (progn
	       (if (not (zerop i)) (emit-str ", "))
	       (emit-c-expression arg)))))

(defun emit-c-operator (op)
  (emit-str " ~A " (cadr (assoc op +op-table+))))


;;; Format the andy types
(defun get-andy-type (type)
  (case type
    (:int "andy_int")
    (:float "andy_float")
    (:string "andy_string")
    (t (error "Emitter: Unknown data type: ~A" type))))

;;; Function Hoisting - Coll all funcs at top-level
(defun collect-functions (node)
  "Return a flat list of all function declarations in the AST."
  (cond
    ((typep node 'function-declaration)
     (cons node
           (collect-functions (func-body node))))
    ((typep node 'program-block)
     ;; Recursively build list, ignoring NILs...
     (mapcan #'collect-functions (block-funcs node)))
    (t nil)))


;;;			 Data Initialization
;;; ------------------------------------------------------------------

;;; Emit C Constant Definition
(defun emit-c-constant (c)
  (let ((type (const-type c))
	(name (const-symbol c))
	(value (const-value c)))
    (case type
      ;; Strings
      (:string 
       (emit-line "static const char ~A_data[] = ~A;" name value)
       (emit-line "static const andy_string ~A = {" name)
       (indent)
       (emit-line ".length = sizeof(~A_data) - 1," name)
       (emit-line ".data   = (char *)~A_data" name)
       (outdent)
       (emit-line "};"))
      ;; Integers
      (:int
       (emit-line "static const andy_int ~A = ~A;" name value))
      ;; Floats
      (:float
       (emit-line "static const andy_float ~A = ~A;" name value)))))

;;; Emit Global Constants
(defun emit-c-global-consts (consts)
  (when consts
    (progn
      (emit-line "/* Global Constants */")
      (dolist (c consts)
	(emit-c-constant c))
      (emit-newline))))

;;; Emit Local Constants
(defun emit-c-local-consts (consts)
  (when consts
    (progn
      (emit-line "/* Global Constants */")
      (dolist (c consts)
	(emit-c-constant c))
      (emit-newline))))

;;; Emit c Variable Declaration
(defun emit-c-variable (v)
  (let ((type (var-type v))
	(name (var-symbol v)))
    (case type
      ;; Integers
      (:int
       (emit-line "static andy_int ~A;" name))
      ;; Floats
      (:float
       (emit-line "static andy_float ~A;" name)))))
  
;;; Emit Unitialized Global Varaibles
(defun emit-c-global-vars (vars)
  (when vars
    (progn
      (emit-line "/* Global Uninitialized Variables */")
      (dolist (v vars)
	(emit-c-variable v))
      (emit-newline))))

;;; Emit Unitialized Local Variables
(defun emit-c-local-vars (vars)
  (when vars
    (progn
      (emit-line "/* Local Scope Variables */")
      (dolist (v vars)
	(emit-c-variable v))
      (emit-newline))))

;;;			   Emit Expressions
;;; ------------------------------------------------------------------
(defgeneric emit-c-expression (expr))

;;; Number Literal Expression
(defmethod emit-c-expression ((expr number-literal))
  (emit-str "~A" (number-value expr)))

;;; Identifier Expression
(defmethod emit-c-expression ((expr identifier))
  (emit-str "~A" (id-symbol expr)))

;;; Function Call Expression
(defmethod emit-c-expression ((expr function-call))
  (emit-funcall expr)
  (emit-str ")"))

;;; Conditional Expression
(defmethod emit-c-expression ((expr conditional-expression))
  ;; LHS Expression
  (emit-c-expression (cond-lhs expr))
  ;; Operator
  (emit-c-operator (cond-op expr))
  ;; RHS Expression
  (emit-c-expression (cond-rhs expr)))

;;; Binary Operations
(defmethod emit-c-expression ((expr binary-expression))
  ;; LHS Expression
  (emit-c-expression (binary-lhs expr))
  ;; Operator
  (emit-c-operator (binary-op expr))
  ;; RHS Expression
  (emit-c-expression (binary-rhs expr)))


;;;			   Emit Statements
;;; ------------------------------------------------------------------
(defgeneric emit-c-statement (stmnt))

;; Compound Statement
(defmethod emit-c-statement ((stmnt compound-statement))
  (dolist (s (cmpnd-stmnts stmnt))
    (emit-c-statement s)))

;;; Assign Statement
(defun emit-assignment (stmnt)
  (let ((lhs-sym (id-symbol (assign-var stmnt)))
	(rhs-exp (assign-expr stmnt)))
    (emit "~A = " lhs-sym)
    (emit-c-expression rhs-exp)))
  
(defmethod emit-c-statement ((stmnt assign-statement))
  (emit-assignment stmnt)
  (emit-str-nl ";"))

;;; If-then-else Statement
(defmethod emit-c-statement ((stmnt if-statement))
  (let ((condition (if-cond stmnt))
	(then-stmnt (if-conseq stmnt))
	(else-stmnt (if-else stmnt)))
    ;; if (...) { ... }
    (emit "if (")
    (emit-c-expression condition)
    (emit-str-nl ") {")
    (indent)
    (emit-c-statement then-stmnt)
    (outdent)
    (emit-line "}")
    ;; else { ... }
    (when else-stmnt
      (emit "else {")
      (indent)
      (emit-c-statement else-stmnt)
      (outdent)
      (emit-line "}"))))

;; While Statement
(defmethod emit-c-statement ((stmnt while-statement))
  (let ((condition (while-cond stmnt))
	(body (while-body stmnt)))
    ;; while (...) { ... }
    (emit "while (")
    (emit-c-expression condition)
    (emit-line ") {")
    (indent)
    (emit-c-statement body)
    (outdent)
    (emit-line "}")))

;;; Break Statement
(defmethod emit-c-statement ((stmnt break-statement))
  (emit-line "break;"))

;;; Switch Statement
(defmethod emit-c-statement ((stmnt switch-statement))
  (let* ((expr (switch-selector stmnt))
	 (cases (switch-cases stmnt))
	 (def (switch-default stmnt)))
    ;; switch (...) {
    (emit "switch (")
    (emit-c-expression expr)
    (emit-str-nl ") {")
    (indent)
    ;; case: ... break;
    (dolist (c cases)
      (let ((label (case-label c))
	    (body (case-body c)))
	(emit-line "case ~A:" label)
	(indent)
	(dolist (s body)
	  (emit-c-statement s))
	(emit-line "break;")
	(outdent)))
    ;; default:
    (when def
      (emit-line "default:")
      (indent)
      (dolist (s def)
	(emit-c-statement s))
      (outdent))
    ;; ... }
    (outdent)
    (emit-line "}")))

;;; For Statement
(defmethod emit-c-statement ((stmnt for-statement))
  (let ((init  (for-init stmnt))
	(cont  (for-cont stmnt))
	(iter  (for-iter stmnt))
	(body  (for-body stmnt)))
    ;; for (...; ...; ...) { ... }
    (emit "for (")
    (emit-assignment init)
    (emit-str "; ")
    (emit-c-expression cont)
    (emit-str "; ")
    (emit-assignment iter)
    (emit-str-nl ") {")
    (indent)
    (emit-c-statement body)
    (outdent)
    (emit-line "}")))

;;; Funcall Statement
(defmethod emit-c-statement ((stmnt function-call))
  (emit-funcall stmnt)
  (emit-str-nl ");"))

;;; Return Statement
(defmethod emit-c-statement ((stmnt return-statement))
  (emit-newline)
  (emit "return ")
  (emit-c-expression (return-expr stmnt))
  (emit-str-nl ";"))

;;; Write Statement
(defmethod emit-c-statement ((stmnt write-statement))
  (if (write-nl stmnt)
      (emit-line "andy_print_newline();")
      (let ((expr (write-expr stmnt)))
	(case (expr-type expr)
	  (:int
	   (emit "andy_print_int(")
	   (emit-c-expression expr)
	   (emit-str-nl ");"))
	  (:float
	   (emit "andy_print_float(")
	   (emit-c-expression expr)
	   (emit-str-nl ");"))
	  (:string
	   (emit-line "andy_print_string(~A);" 
		      (id-symbol expr)))))))


;;;			      Functions
;;; ------------------------------------------------------------------

;;; Emit Function Signature
(defun emit-c-function-signature (f)
  "Emit the C Function Signature"
  ;; Function Type and Name
  (emit "static ~A ~A("
	(get-andy-type (func-type f)) (func-symbol f))
  ;; Function Parameters
  (loop for param in (func-params f)
	for i from 0
	do (progn
	     (if (not (zerop i)) (emit-str ", "))
	     (emit-str "~A ~A"
		     (get-andy-type (expr-type param))
		     (id-symbol param))))
  ;; Closing rparen
  (emit-str ")"))

;;; Emit Function Prototypes
(defun emit-c-function-prototypes (funcs)
  "Emit C Functions Prototypes"
  (if funcs (emit-line "/* Function Prototypes */"))
  (dolist (f funcs)
    (emit-c-function-signature f)
    (emit-str-nl ";"))
  (if funcs (emit-newline)))

;;; Emit the Full Function Definition
(defun emit-c-function-definitions (funcs)
  (if funcs (emit-line "/* Function Definitions */"))
  (dolist (f funcs)
    (let ((fb (func-body f)))
      ;; Function Signature
      (emit-c-function-signature f)
      (emit-str-nl " {")
      (emit-newline)
      (indent)
      ;; Local Scope Constants/Variables
      (emit-c-local-consts (block-consts fb))
      (emit-c-local-vars (block-vars fb))
      ;; Function Body
      (emit-c-statement (block-body fb))
      ;; Exit Function Scope
      (outdent)
      (emit-line "}")
      (emit-newline))))


;;;			      C PROGRAM
;;; ------------------------------------------------------------------

(defun emit-c-headers ()
  "Emit Included C Headers"
  (emit-line "#include \"andy/andy.h\"")
  (emit-newline)
  (emit-newline))

(defun emit-c-main-function (stmnt)
  "Emit the Main function, if module code body exists"
  (when stmnt
    (emit-line "int main(int argc, [[maybe_unused]] char** argv) {")
    (emit-newline)
    (indent)
    (emit-line "/* Initialize Program Heap Allocation */")
    (emit-line "andy_runtime_init();")
    (emit-newline)
    (emit-c-statement stmnt)
    (emit-newline)
    (emit-line "return EXIT_SUCCESS;")
    (outdent)
    (emit-line "}")))

;;; Emit C Output File
(defun emit-c (ast fn)
  ;; Output filename matches source file specification
  (when (not (string= (andy.ast:program-name ast)
		      (pathname-name fn)))
    (format t "~%~%WARNING: Source filename and ~A name do not match.~%~%"
	    (program-type ast))
    (setf fn (concatenate 'string (andy.ast:program-name ast) ".c")))
  (with-open-file (*stream* fn
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format t "Emitting IR...~%")
    (setf *indent* 0)
    (let* ((pb (program-block ast))
	   (funcs (collect-functions pb)))

      ;; Headers and Prototypes
      (emit-c-headers)
      (emit-c-function-prototypes funcs)
      
      ;; Global Data
      (emit-c-global-consts (block-consts pb))
      (emit-c-global-vars (block-vars pb))

      ;; Functions
      (emit-c-function-definitions funcs)
      (emit-c-main-function (block-body pb))
      )))
