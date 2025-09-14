(defpackage :andy.main
  (:use :cl :andy.lexer :andy.parser :andy.ast :andy.ir :andy.wasm)
  (:export :compile-source))

(in-package :andy.main)

(defun compile-source (filename)
  (let* ((source (read-file filename))
         (tokens (tokenize source))
         (ast    (parse tokens))
	 (ir     (generate-ir ast))
         (output (emit-wasm ir)))
    (format t "~%Final output: ~A~%" output)))

