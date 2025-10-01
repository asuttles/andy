(defpackage :andy.parser
  (:use :cl)
  (:export :parse)
  (:import-from :andy.lexer
   :token-type :token-lexeme
   :token-line :token-column))

(in-package :andy.parser)


;;; Parser Data Structure and Token Operations
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


;;; Token Value Getters
(defun get-ident-token (parser)
  "Consume an identifier token and return its name as a string"
  (let ((tok (expect-token parser :ident)))
    (token-lexeme tok)))

(defun get-number-token (parser)
  "Consume a number token and return its value as an integer"
  (let ((tok (expect-token parser :number)))
    (parse-integer (token-lexeme tok))))


;;; Parse the token stream...
(defun parse (tokens)
  "Create parser data struct from TOKENS stream, and begin parsing"
  (format t "Parsing token stream...~%")
  (let ((parser (make-parser :tokens tokens)))
    (parse-program parser)))

;;; Program
(defun parse-program (parser)
  (let ((block (parse-block parser)))
    (unless (eq (token-type (current-token parser)) :period)
      (error "Parse Error: Expected '.' at end of program"))
    (advance-token parser)
    (make-instance 'program :block block)))

;;; Block
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

;;; Block - Constants
(defun parse-constant-declarations (parser)
  (when (eq (token-type (current-token parser)) :const)
    (advance-token parser)
    (let ((consts nil))
      (loop
	    (let* ((this-token (current-token parser))
		   (type (token-type this-token)))
	      (unless (member type '(:int :float))
		(error "Parse Error: Unknown type ~A at line ~A, Column ~A~%"
		       type (token-line this-token) (token-column this-token)))
	      (advance-token parser)
              (let ((name (get-ident-token parser)))
		(expect-token parser :eql)
		(let ((num (get-number-token parser)))
		  (push (make-instance 'constant-declaration
				       :symbol name
				       :value num
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
                  (push (cons name body) procs)))))
   (nreverse procs)))


;;; Statements
(defun parse-statements (parser)
  (let ((tok (current-token parser)))
    (case (token-type tok)
      (:ident (parse-assignment parser))
      (:call  (parse-call parser))
      (:begin (parse-begin-block parser))
      (:if    (parse-if parser))
      (:while (parse-while parser))
      (t (error "Parse Error: Unexpected token ~A at line ~A, Column ~A.~%"
		(token-lexeme tok) (token-line tok) (token-column tok))))))

(defun parse-assignment (parser)
  (let ((variable (get-ident-token parser)))
    (expect-token parser :assign)
    (let ((expr (parse-expression parser)))
      (expect-token parser :semicolon)
      (make-instance 'assign-statement :var variable :expr expr))))

(defun parse-call (parser)
  (expect-token parser :call)
  (let ((name (get-ident-token parser)))
    (expect-token parser :semicolon)
    (make-instance 'call-statement :name name)))

(defun parse-begin-block (parser)
  (expect-token parser :begin)
  (let ((stmnts '()))
    (loop
      do (push (parse-statements parser) stmnts)
      until (eq (token-type (current-token parser)) :end))
    (expect-token parser :end)
    (make-instance 'compound-statement :stmnts (nreverse stmnts))))

(defun parse-if (parser)
  "Parse an if statement from PARSER,
of the form if <condition> then <body>"
  (expect-token parser :if)
  (let ((condition (parse-condition parser)))
    (expect-token parser :then)
    (let ((body (parse-statements parser)))
      (make-instance 'if-statement
		     :cond condition
		     :body body))))

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

;;; Expressions
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
		      :type :int))
      (:lparen				; ( expr )
       (advance-token parser) 
       (let ((expr (parse-expression parser)))
         (expect-token parser :rparen)
         expr))
      (t (error "Parse Error: Unexpected token type ~A at line ~A, Column ~A.~%"
		(token-type tok) (token-line tok) (token-column tok))))))

(defun parse-term (parser)
  "Parse a term from the PARSER token stream,
where term = 'factor binary-op factor'"
  ;; AST-NODE is an AST subtree that accumulates structure
  ;; as additional rhs sub-tree gets folded in.
  ;; AST-NODE <- (binary op AST-NODE rhs)
  ;; The AST-NODE, effectively, represents the lhs of the
  ;; expression at each binary-op token.
  (let ((AST-NODE (parse-factor parser)))
    (loop while (member (token-type (current-token parser)) '(:times :divide))
          do (let ((op (token-type (advance-token parser)))
                   (rhs (parse-factor parser)))
               (setf AST-NODE (make-instance 'binary-expression
                                             :left AST-NODE
                                             :op op
                                             :right rhs))))
    AST-NODE))

(defun parse-condition (parser)
  "Parse a logical expression in PARSER,
where the expression is formed by 'lhs binary-op rhs'"
  (let* ((lhs (parse-expression parser))
	 (tok (current-token parser))
	 (op (token-type tok)))
    (unless (member op '(:eql :neq :lss :leq :gtr :geq))
      (error "Parse Error: Unexpected binary-op token ~A at line ~A, Column ~A.~%"
	     op (token-line tok) (token-column tok)))
    (advance-token parser)
    (let ((rhs (parse-expression parser)))
      (make-instance 'binary-expression
		     :left lhs :op op :right rhs))))

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
                                         :left AST-NODE
                                         :op op
                                         :right rhs))))
    AST-NODE))

