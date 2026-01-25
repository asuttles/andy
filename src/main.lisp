(defpackage :andy.main
  (:use :cl :uiop :andy.lexer :andy.ast :andy.runtime :andy.parser :andy.analyzer :andy.emitter :andy.generator)
  (:export :compile-source))

(in-package :andy.main)


(defun wat2wasm-available-p ()
  (uiop:run-program "command -v wat2wasm"
                    :ignore-error-status t
		    :output :string
                    :error-output nil))

(defun compile-wat2wasm (watfile)
  (uiop:run-program (format nil "wat2wasm ~A" watfile)))

(defun compile-source-to-wasm (infile)
  (let* ((watfile (concatenate 'string
			       (pathname-name infile) ".wat"))
	 (source  (read-file infile))
         (tokens  (tokenize source))
         (ast     (parse tokens)))
    (when (analyze-ast ast)
      (format t "all checks passed.~%"))
    (emit-wasm ast watfile)
    (if (wat2wasm-available-p)
	(compile-wat2wasm watfile)
	(format t "wat2wasm not found: compilation stopped. ~A saved in local dir.~%"
		watfile)))
  (format t "Compilation complete."))

(defun compile-source-to-lisp (infile writefile-p)
  (let* ((lispfile
	   (and writefile-p
		(concatenate 'string
			     (pathname-name infile) ".lisp")))
	 (source (read-file infile))
	 (tokens (tokenize source))
	 (ast    (analyze-ast (parse tokens)))
	 (ir     (generate-ir ast)))
    (emit-lisp ir lispfile))
  (format t "Compilation complete."))
