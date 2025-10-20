(defpackage :andy.parser
  (:use :cl :andy.ast :andy.runtime)
  (:export :parse)
  (:import-from :andy.lexer
   :token-type :token-lexeme
   :token-line :token-column))

(in-package :andy.parser)


;;; Parser Data Structure and Token Operations ────────────────────────

(defstruct parser
  tokens				; Token Stream
  (pos 0))				; Index

(defun current-token (parser)
  "Return the Token from PARSER at position POS"
  (nth (parser-pos parser) (parser-tokens parser)))

(defun next-token (parser)
  "Return the Token from PARSER at position POS + 1"
  (nth (1+ (parser-pos parser)) (parser-tokens parser)))

(defun advance-token (parser)
  "Advance the index to the next token"
  (incf (parser-pos parser)))

(defun match-token (parser type)
  "Returns T if the current token in PARSER matches TYPE"
  (eq (token-type (current-token parser)) type))

(defun expect-token (parser type)
  "Returns current token lexeme, if it matches TYPE"
  (let ((tok (current-token parser)))
    (if (eq (token-type tok) type)
        (prog1 tok (advance-token parser))
        (error "Parse Error: Expected ~A, got ~A at line ~A, Column ~A~%"
	       type (token-type tok) (token-line tok) (token-column tok)))))

;;; String Literals ───────────────────────────────────────────────────

(defparameter *string-literals* '())


;;; Token Value Getters  ──────────────────────────────────────────────

(defun get-ident-token (parser)
  "Consume an identifier token and return its name as a string"
  (let ((tok (expect-token parser :ident)))
    (token-lexeme tok)))

(defun get-number-token (parser)
  "Consume a number token and return its value as an integer"
  (let ((tok (expect-token parser :number)))
    (parse-integer (token-lexeme tok))))

(defun get-number-type (token)
  ;; Function Stub
  (if token :int nil))


;;; ─── Parse Statements  ────────────────────────────────────────────

;;; Assignment Statement
(defun parse-assignment (parser)
  (let* ((var-name (get-ident-token parser))
	 (var-node (make-instance 'identifier :symbol var-name)))
    (expect-token parser :assign)
    (let ((expr (parse-expression parser)))
      (expect-token parser :semicolon)
      (make-instance 'assign-statement :var var-node :expr expr))))

;;; Call Statement
(defun parse-call (parser)
  (expect-token parser :call)
  (let ((name (get-ident-token parser)))
    (expect-token parser :semicolon)
    (make-instance 'call-statement :name name)))

;;; Block of Statements
(defun parse-begin-block (parser)
  (expect-token parser :begin)
  (let ((stmnts '()))
    (loop
      do (push (parse-statements parser) stmnts)
      until (eq (token-type (current-token parser)) :end))
    (expect-token parser :end)
    (make-instance 'compound-statement :stmnts (nreverse stmnts))))


;;; Generate a const declaration and identifier
;;;   reference for a literal string.
(defun make-string-literal (tok parser)
  (let* ((sym (gensym)))
    (push 
     (make-instance 'constant-declaration
		    :symbol sym
		    :value (token-lexeme tok)
		    :type :string) *string-literals*)
    (advance-token parser)
    (make-instance 'identifier
		   :symbol sym
		   :type :string)))

;;; Write Statement
(defun parse-write (parser)
  (expect-token parser :write)
  (let* ((tok (current-token parser))
	 (expr 
	   (if (eq (token-type tok) :string)
	       (make-string-literal tok parser)
	       (parse-expression parser))))
    (expect-token parser :semicolon)
    (make-instance 'write-statement :expr expr)))


;;; Write a Newline to the Console
(defun parse-writeNL (parser)
  (expect-token parser :writeNL)
  (expect-token parser :semicolon)
  (make-instance 'write-statement :expr nil :nl t))

;;; If Statement
(defun parse-if (parser)
  "Parse an if statement from PARSER,
of the form if <condition> then <body> [else <body>]"
  (expect-token parser :if)
  (let ((condition (parse-condition parser)))
    (expect-token parser :then)
    (let ((body (parse-statements parser))
	  (else (if (eq (token-type (current-token parser)) :else)
		    (progn
		      (advance-token parser) ;Consumme :else
		      (parse-statements parser))
		    nil)))
      (make-instance 'if-statement
		     :cond condition
		     :conseq body
		     :else   else))))

;;; While Statement
(defun parse-while (parser)
  "Parse a while statement from PARSER,
of the form while <condition> do <body>"
  (expect-token parser :while)
  (let ((condition (parse-condition parser)))
    (expect-token parser :do)
    (let ((body (parse-statements parser)))
      (make-instance 'while-statement
		     :cond condition
		     :body body))))

;;; Statements
(defun parse-statements (parser)
  (let ((tok (current-token parser)))
    (case (token-type tok)
      (:ident   (parse-assignment parser))
      (:call    (parse-call parser))
      (:begin   (parse-begin-block parser))
      (:write   (parse-write parser))
      (:writeNL (parse-writeNL parser))
      (:if      (parse-if parser))
      (:while   (parse-while parser))
      (t (error "Parse Error: Unexpected token ~A at line ~A, Column ~A.~%"
		(token-lexeme tok) (token-line tok) (token-column tok))))))

;;; ─── Parse Expressions ────────────────────────────────────────────

;;; Expression Factors
(defun parse-factor (parser)
  "Parse the factor (variable, number, or expression) from PARSER"
  (let ((tok (current-token parser)))
    (case (token-type tok)
      (:plus				; +<Factor>
       (advance-token parser)
       (make-instance 'unary-expression
		      :op :plus
		      :expr (parse-factor parser)))
      (:minus				; -<Factor>
       (advance-token parser)
       (make-instance 'unary-expression
		      :op :minus
		      :expr (parse-factor parser)))
      (:ident				; Variable
       (make-instance 'identifier
		      :symbol (get-ident-token parser)))
      (:number				; Number Literal
       (make-instance 'number-literal
		      :value (get-number-token parser)
		      :type  (get-number-type  tok)))
      (:lparen				; ( expr )
       (advance-token parser) 
       (let ((expr (parse-expression parser)))
         (expect-token parser :rparen)
         expr))
      (t (error "Parse Error: Unexpected token type ~A at line ~A, Column ~A.~%"
		(token-type tok) (token-line tok) (token-column tok))))))

;;; Term Factors
(defun parse-term (parser)
  "Parse a term from the PARSER token stream,
where term = 'factor binary-op factor'"
  ;; AST-NODE is an AST subtree that accumulates structure
  ;; as additional rhs sub-tree gets folded in.
  ;; AST-NODE <- (binary op AST-NODE rhs)
  ;; The AST-NODE, effectively, represents the lhs of the
  ;; expression at each binary-op token.
  (let ((AST-NODE (parse-factor parser)))
    (loop while (member (token-type (current-token parser)) '(:times :divide :modulo))
          do (let ((op (token-type
			(prog1
			  (current-token parser)			  
			  (advance-token parser))))
                   (rhs (parse-factor parser)))
               (setf AST-NODE (make-instance 'binary-expression
                                             :lhs AST-NODE
                                             :op op
                                             :rhs rhs))))
    AST-NODE))


;;; Conditional Expressions


;;; ORIGINAL Conditional Expression Parser
;;; -> did not allow for logical operators
;;; -> now subordinate to logical operator parsing
(defun parse-comparison (parser)
 "Parse a logical comparison expression in PARSER,
where the expression is formed by 'lhs binary-op rhs'
and the binary-op is member of '(=, #, <, <=, >, >=)."
 (let* ((lhs (parse-expression parser))
	 (tok (current-token parser))
	 (op (token-type tok)))
   (unless (member op '(:eql :neq :lss :leq :gtr :geq))
     (error "Parse Error: Unexpected binary-op token ~A at line ~A, Column ~A.~%"
	     op (token-line tok) (token-column tok)))
   (advance-token parser)
   (let ((rhs (parse-expression parser)))
     (make-instance 'conditional-expression
		     :lhs lhs :op op :rhs rhs))))


;;; AND/XOR -> Second Level of Precedence for Logic Ops
(defun parse-logic-and/xor (parser)
 "Parse a logical comparison expression in PARSER,
where the expression is formed by 'lhs binary-op rhs'
and the binary-op is member of '(and, xor)."
  ;; AST-NODE is an AST subtree that accumulates structure
  ;; as additional rhs sub-tree gets folded in.
  ;; AST-NODE <- (binary op AST-NODE rhs)
  ;; The AST-NODE, effectively, represents the lhs of the
  ;; expression at each binary-op token.
  (let ((AST-NODE (parse-comparison parser)))
    (loop while (member (token-type (current-token parser)) '(:and :xor))
          do (let ((op (token-type (current-token parser))))
               (advance-token parser)
               (setf AST-NODE (make-instance 'conditional-expression
                                         :lhs AST-NODE
                                         :op op
                                         :rhs (parse-comparison parser)))))
    AST-NODE))

;;; OR -> Highest Precedence for Logic Operations
(defun parse-logic-or (parser)
 "Parse a logical comparison expression in PARSER,
where the expression is formed by 'lhs OR rhs'."
  ;; AST-NODE is an AST subtree that accumulates structure
  ;; as additional rhs sub-tree gets folded in.
  ;; AST-NODE <- (binary op AST-NODE rhs)
  ;; The AST-NODE, effectively, represents the lhs of the
  ;; expression at each binary-op token.
  (let ((AST-NODE (parse-logic-and/xor parser)))
    (loop while (member (token-type (current-token parser)) '(:or))
          do (let ((op (token-type (current-token parser))))
               (advance-token parser)
               (setf AST-NODE (make-instance 'conditional-expression
                                         :lhs AST-NODE
                                         :op op
                                         :rhs (parse-logic-and/xor parser)))))
    AST-NODE))

;;; Driver for Conditional Expression Parsing
(defun parse-condition (parser)
  (parse-logic-or parser))


;;; Expressions
(defun parse-expression (parser)
  "Parse an expression: term { (+|-) term }."
  ;; AST-NODE is an AST subtree that accumulates structure
  ;; as additional rhs sub-tree gets folded in.
  ;; AST-NODE <- (binary op AST-NODE rhs)
  ;; The AST-NODE, effectively, represents the lhs of the
  ;; expression at each binary-op token.
  (let ((AST-NODE (parse-term parser)))  ;; term handles *, / and unary +/- via parse-factor
    (loop
      for tok = (token-type (current-token parser))
      while (member tok '(:plus :minus))
      do (let ((op (prog1 tok (advance-token parser)))
               (rhs (parse-term parser)))
           (setf AST-NODE (make-instance 'binary-expression
                                         :lhs AST-NODE
                                         :op op
                                         :rhs rhs))))
    AST-NODE))

;;; ─── Parse Declarations ───────────────────────────────────────────

;;; Block - Constants
(defun parse-constant-declarations (parser)
  (when (eq (token-type (current-token parser)) :const)
    (advance-token parser)
    (let ((consts nil))
      (loop
	    (let* ((this-token (current-token parser))
		   (type (token-type this-token)))
	      (unless (member type '(:int :float :string))
		(error "Parse Error: Unknown type ~A at line ~A, Column ~A~%"
		       type (token-line this-token) (token-column this-token)))
	      (advance-token parser)
              (let ((name (get-ident-token parser)))
		(expect-token parser :eql)
		(let ((val
			(case type
			  (:int (get-number-token parser))
			  (:string (prog1
				       (token-lexeme (current-token parser))
				     (advance-token parser))))))
		  (push (make-instance 'andy.ast:constant-declaration
				       :symbol name
				       :value val
				       :type type)
			consts))))
        (if (match-token parser :comma)
	    (advance-token parser)
	    (return (progn
		      (expect-token parser :semicolon)
		      (nreverse consts))))))))

;;; Block - Variables - Integers
(defun parse-integer-declarations (parser)
  (when (eq (token-type (current-token parser)) :int)
    (advance-token parser)
    (let ((vars nil))
      (loop
	(push (make-instance 'variable-declaration
			     :symbol (get-ident-token parser)
			     :type :int) vars)
	(if (match-token parser :comma)
	    (advance-token parser)
	    (return (progn
		      (expect-token parser :semicolon)
		      (nreverse vars))))))))

;;; Block - Procedures
(defun parse-procedure-declarations (parser)
 (let ((procs nil))
   (loop while (eq (token-type (current-token parser)) :procedure)
         do (progn
              (advance-token parser)
              (let ((name (get-ident-token parser)))
                (expect-token parser :semicolon)
                (let ((body (parse-block parser)))
                  (expect-token parser :semicolon)
		  (push (make-instance 'procedure-declaration
				       :symbol name
				       :body body)
			procs)))))
   (nreverse procs)))


;;; ─── Parse Program ────────────────────────────────────────────────

;;; Program/Procedure Blocks
(defun parse-block (parser)
  (let ((consts (parse-constant-declarations parser))
        (ints   (parse-integer-declarations parser))
        (procs  (parse-procedure-declarations parser))
        (body   (parse-statements parser)))
    (make-instance 'program-block
                   :consts consts
                   :vars ints
                   :procs procs
                   :body body)))

;;; Program
(defun parse-program (parser)
  (let ((block (parse-block parser)))
    (unless (eq (token-type (current-token parser)) :period)
      (error "Parse Error: Expected '.' at end of program"))
    (advance-token parser)
    ;; Append string literals to constants list
    (setf (block-consts block) (append *string-literals* (block-consts block)))
    (make-instance 'program :block block)))


;;; Parse the token stream...
(defun parse (tokens)
  "Create parser data struct from TOKENS stream, and begin parsing"
  (format t "Parsing token stream...~%")
  (let ((parser (make-parser :tokens tokens)))
    (parse-program parser)))

