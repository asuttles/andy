(defpackage :andy.parser
  (:use :cl)
  (:export :parse)
  (:import-from :andy.lexer
   :token-type :token-lexeme
   :token-line :token-column))

(in-package :andy.parser)


;;; Parser Data Structure and Token Operations
(defstruct parser
  tokens
  (pos 0))

(defun current-token (parser)
  (nth (parser-pos parser) (parser-tokens parser)))

(defun next-token (parser)
  (nth (1+ (parser-pos parser)) (parser-tokens parser)))

(defun advance-token (parser)
  (incf (parser-pos parser)))

(defun match-token (parser type)
  (eq (token-type (current-token parser)) type))

(defun expect-token (parser type)
  (let ((tok (current-token parser)))
    (if (eq (token-type tok) type)
        (prog1 tok (advance-token parser))
        (error "Parse Error: Expected ~A, got ~A"
	       type (token-type tok)))))


;;; Token Value Getters
(defun get-ident-token (parser)
  "Consume an identifier token and return its name as a string."
  (let ((tok (expect-token parser :ident)))
    (token-lexeme tok)))

(defun get-number-token (parser)
  "Consume a number token and return its value as an integer."
  (let ((tok (expect-token parser :number)))
    (parse-integer (token-lexeme tok))))


;;; Parse the token stream...
(defun parse (tokens)
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
  (current-token parser)
  nil)
