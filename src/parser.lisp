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
    (advance parser)
    (make-instance 'program-node :block block)))

;;; Block
(defun parse-block (parser)
  (let ((consts (parse-constant-declarations parser))
        (vars   (parse-variable-declarations parser))
        (procs  (parse-procedure-declarations parser))
        (body   (parse-statements parser)))
    (make-instance 'program-block
                   :consts consts
                   :vars vars
                   :procs procs
                   :body body)))

;;; Block - Constants
(defun parse-constant-declarations (parser)
  (when (eq (token-type (current-token parser)) :const)
    (advance-token parser)
    (let ((consts nil))
      (loop
        (let ((name (get-ident-token parser)))
          (expect-token parser :eql)
          (let ((num (get-number-token parser)))
            (push (list name num) consts)))
        (if (match-token parser :comma)
            (advance-token parser)
            (return (progn
                      (expect-token parser :semicolon)
                      (nreverse consts))))))))

;;; Block - Variables
(defun parse-variable-declarations (parser)
  (when (eq (token-type (current-token parser)) :var)
    (advance-token parser)
    (let ((vars nil))
      (loop
	(push (get-ident-token parser) vars)
	(if (match-token parser :comma)
	    (advance-token parser)
	    (return (progn
		      (expect-token parser :semicolon)
		      (nreverse vars))))))))

;;; Block - Procedures
(defun parse-procedure-declarations (parser)
  (when (eq (token-type (current-token parser)) :procedure)
    (advance-token parser)
    (let ((procs nil))
      (loop
	(let ((name (get-ident-token parser)))
	  (expect-token parser :semicolon)
	  (let ((body (parse-block parser)))
	    (expect-token parser :semicolon)
            (push (cons name body) procs)))
	finally (return (nreverse procs))))))

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
      until (eq (token-type (current-token parser) :end)))
    (match-token parser :end)
    (match-token parser :semicolon)
    (make-instance 'compound-statement :stmnts (nreverse stmnts))))


(defun parse-factor (parser)
  "Parse the factor (variable, number, or expression) from PARSER"
  (let ((tok (current-token parser)))
    (case (token-type tok)
      (:ident				; Variable
       (make-instance 'identifier
                      :symbol (get-ident-token parser)))
      (:number				; Number Literal
       (make-instance 'number-literal
                      :value (parse-integer (token-lexeme (expect-token parser :number)))))
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




    
    

	
