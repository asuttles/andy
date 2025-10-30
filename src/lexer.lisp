(defpackage :andy.lexer
  (:use :cl :uiop)
  (:export
   :read-file
   :tokenize 
   :token-type
   :token-lexeme
   :token-line
   :token-column))   
   
(in-package :andy.lexer)

;;; TOKEN datatype
(defstruct token
  type
  lexeme
  line
  column)

;;; Token List
(defvar *token-list* '())

;;; Language Keywords
(defparameter +keywords+
  '(("const"     . :const)
    ("int"       . :int)
    ("void"      . :void)
    ("str"       . :string)
    ("procedure" . :procedure)
    ("call"      . :call)
    ("fun"       . :function)
    ("ret"       . :return)
    ("begin"     . :begin)
    ("end"       . :end)
    ("write"     . :write)
    ("writeNL"   . :writeNL)
    ("if"        . :if)
    ("then"      . :then)
    ("else"      . :else)
    ("while"     . :while)
    ("for"       . :for)
    ("do"        . :do)
    ("break"     . :break)
    ("switch"    . :switch)
    ("case"      . :case)
    ("default"   . :default)
    ("and"       . :and)
    ("or"        . :or)
    ("xor"       . :xor)
    ("program"   . :program)))

(defparameter +operators+
  '(("+"  . :plus)
    ("-"  . :minus)
    ("*"  . :times)
    ("/"  . :divide)
    ("%"  . :modulo)
    ("="  . :eql)
    ("#"  . :neq)
    ("<"  . :lss)
    ("<=" . :leq)
    (">"  . :gtr)
    (">=" . :geq)
    (":=" . :assign)
    (","  . :comma)
    (":"  . :colon)
    (";"  . :semicolon)
    ("."  . :period)
    ("("  . :lparen)
    (")"  . :rparen)))

;;; Slurp source file into a string
(defun read-file (filename)
  (format t "Reading file: ~A...~%" filename)
  (handler-case (uiop:read-file-string filename)
    (file-error (e)
      (format t "~A~%" e) "")))

;;; Predicates to Match Digits and Letters
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

;;; Make a String Token
(defun make-string-token (src pos line nl-pos)
  (let ((start pos))
    (incf pos)
    ;; Scan text
    (loop while (and (< pos (length src))
		     (not (char= (char src pos) #\")))
	  do (incf pos))
    (when (not (char= (char src pos) #\"))
      (error "Scan Error: Did not find end of string beginning at col: ~A, line: ~A~%"
	     (- pos nl-pos) line))
    (incf pos)
    (push (make-token
	   :type :string
	   :lexeme (subseq src start pos)
	   :line line
	   :column (- start nl-pos)) *token-list*))
  pos)

;;; Make a Symbol Token
(defun make-symbol-token (src pos line nl-pos)
  (let ((start pos))
    (loop for c = (char src pos)
	  while (or (letter-p c) (digit-p c))
	  do (incf pos))
    (let* ((lexeme (subseq src start pos))
	   (tok-type
	     (or (cdr (assoc lexeme +keywords+ :test #'string=))
		 :ident)))
      (push (make-token :type tok-type
			:lexeme lexeme
			:line line
			:column (- start nl-pos)) *token-list*)))
  pos)


;;; Make Operator Token
(defun make-operator-token (src pos line nl-pos)
  (let* ((c (char src pos))
	 (next (when (< (1+ pos) (length src))
		(char src (1+ pos))))
	 (col (- pos nl-pos))
	 (lex (string c))
	 (lex-2 (if next (concatenate 'string lex (string next))
		    lex))
	 (typ (cdr (assoc lex +operators+ :test #'string=)))
	 (typ-2 (cdr (assoc lex-2 +operators+ :test #'string=))))
    (cond
      ((not (null typ-2))
       (progn
	 (push
	  (make-token :type typ-2 :lexeme lex-2 :line line :column col) *token-list*)
	 (+ 2 pos)))
      ((not (null typ))
       (progn
	 (push
	  (make-token :type typ :lexeme lex :line line :column col) *token-list*)
	 (+ 1 pos)))
      (t
       (progn
	 (format t "~%lex: ~a (~a)~%" lex typ)
	 (error "Scan Error: Symbol ~a on line ~a and column ~a cannot be resolved.~%" lex line col) 1)))))
      

;;; Skip Comments
(defun skip-comment (src pos len line nl-pos)
  (let* ((x (1+ pos))
	 (c (char src x))
	 (col (- pos nl-pos)))
    (if (not (char= c #\/))
	(error "Scan Error: Symbol ~a on line ~a and column ~a cannot be resolved.~%"
	       (char src pos) line col)
	(loop
	  while (and (< x len)
		     (not (char= (char src x) #\Newline)))
	  do (incf x)))
    x))

;;; Tokenize the string
(defun tokenize (src)
  (format t "Scanning Source File...~%")
  (setf *token-list* nil)
  ;; Tokenizer State
  (let ((len (length src))
	(pos 0)
	(line 1)
	(nl-pos 0))			; position of last NL
    ;; Scanner Functions
    (labels
	((peek () (when (< pos len) (char src pos)))
	 (next () (when (< (+ pos 1) len) (char src (+ pos 1))))
	 (getc () (prog1 (peek) (incf pos))))
      ;; Character Scanner
      (loop while (< pos len) do
	(let ((c (peek))
	      (n (next)))
	  (cond
	    ;; Numbers
	    ((digit-p c)
	     (setf pos (make-number-token src pos line nl-pos)))
	    ;; String
	    ((char= c #\")
	     (setf pos (make-string-token src pos line nl-pos)))
	    ;; Symbols
	    ((letter-p c)
	     (setf pos (make-symbol-token src pos line nl-pos)))
	    ;; Comments
	    ((and (char= c #\/) (char= n #\/))
	     (setf pos (skip-comment src pos len line nl-pos)))
	    ;; Operators
	    ((find c "+-*%/=<>:#,;.()")
	     (setf pos (make-operator-token src pos line nl-pos)))
	    ;; Newline
	    ((char= c #\Newline)
	     (progn (getc) (setf nl-pos pos) (incf line)))
	    ;; Space
	    ((find c " \t\r") (getc))
	    ;; Error
	    (t (getc))))))
    (format t "Scanned ~a lines resulting in ~a tokens ~%"
	    line (length *token-list*)))
  (reverse *token-list*))

