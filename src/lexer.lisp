(defpackage :andy.lexer
  (:use :cl :uiop)
  (:export :tokenize :read-file))

(in-package :andy.lexer)

;;; TOKEN datatype
(defstruct token
  type
  lexeme
  line
  column)

;;; Token List
(defvar *token-list* '())

;;; All Token Types
;;(defconstant +token-types+
;;  '(:ident :number
;;    :const :var :procedure :call :begin :end :if :then :while :do
;;    :odd :plus :minus :times :divide
;;    :eql :neq :lss :leq :gtr :geq
;;    :assign :comma :semicolon :period :lparen :rparen))

;;; Slurp source file into a string
(defun read-file (filename)
  (format t "Reading file: ~A~%~%" filename)
  (handler-case (uiop:read-file-string filename)
    (file-error (e)
      (format t "~A~%" e) "")))

;;; Match Digists and Letters
(defun digit-p  (c)
  (and c (char<= #\0 c #\9)))

(defun letter-p (c)
  (and c (alpha-char-p c)))

;;; Make a Number Token
(defun make-number-token (src pos line nl-pos)
  (let ((start pos))
    ;; Scan digits
    (loop while (and (< pos (length src))
                     (digit-p (char src pos)))
          do (incf pos))
    (push (make-token
	   :type :number
	   :lexeme (subseq src start pos)
	   :line line
	   :column (- start nl-pos)) *token-list*))
  pos)
  
;;; Tokenize the string
(defun tokenize (src)
  (format t "Scanning...~% ~A~%" src)
  ;; Tokenizer State
  (let ((len (length src))
	(pos 0)
	(line 1)
	(nl-pos 0))			; position of last NL
    ;; Scanner Functions
    (labels
	((peek () (when (< pos len) (char src pos)))
	 (getc () (prog1 (peek) (incf pos))))
      ;; Character Scanner
      (loop while (< pos len) do
	(let ((c (peek)))
	  (cond
	    ;; Numbers
	    ((digit-p c)
	     (setf pos (make-number-token src pos line nl-pos)))
	    ;; Newline
	    ((char= c #\Newline)
	     (progn (getc) (setf nl-pos pos) (incf line)))
	    ;; Space
	    ((find c " \t\r") (getc))
	    ;; Error
	    (t (getc)))))))
  (reverse *token-list*))

