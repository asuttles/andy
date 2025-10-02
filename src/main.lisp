(defpackage :andy.main
  (:use :cl :andy.lexer :andy.parser :andy.ast :andy.ir :andy.wasm)
  (:export :compile-source))

(in-package :andy.main)

(defun compile-source (filename)
  (let* ((source  (read-file filename))
         (tokens  (tokenize source))
         (ast     (parse tokens)))
    (if (analyze-ast ast)
	(format t "Semantic checks passed.~%")
	(format t "Semantic checks failed.~%"))
    (emit-wasm ast))
  (format t "~%Compilation complete.~%"))

