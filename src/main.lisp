(defpackage :andy.main
  (:use :cl :uiop
	:andy.lexer :andy.ast :andy.runtime
   :andy.parser :andy.analyzer
   :andy.wasm-emitter :andy.c-emitter)
  (:export :compile-source-to-c :compile-source-to-wasm))

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

(defun compile-source-to-c (infile)
  (let* ((cfile (concatenate 'string
			     (pathname-name infile) ".c"))
	 (source  (read-file infile))
         (tokens  (tokenize source))
         (ast     (parse tokens)))
    (when (analyze-ast ast)
      (format t "all checks passed.~%"))
    (emit-c ast cfile))
  ;; Compile and link c output
  (format t "Compilation complete."))

