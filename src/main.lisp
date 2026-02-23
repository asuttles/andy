;;;; -------------------------------------------------------
;;;; ANDYC - Transpile Andy to C/WASM and Compile
;;;;
;;;; To build the compiler:
;;;;
;;;;   > ./build.sh
;;;;
;;;; -------------------------------------------------------
(defpackage :andy.main
  (:use :cl :uiop
	:andy.lexer :andy.ast :andy.runtime
   :andy.parser :andy.analyzer
   :andy.wasm-emitter :andy.c-emitter)
  (:export :main))

(in-package :andy.main)

;;; Compiler State
(defvar *target* :C)
(defvar *compile-only-p* nil) 		; Compile source or build project
(defvar *files* nil)

;;; C Compiler Parameters
(defvar *CC* nil)
(defvar *cflags* nil)
(defvar *ldflags* nil)


;;; ------------------------------------------------------------------
;;;			     WASM BACKEND
;;; ------------------------------------------------------------------
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

;;; ------------------------------------------------------------------
;;;			      C BACKEND
;;; ------------------------------------------------------------------

(defun detect-c-compiler ()
  "Return the first available C compiler, or signal an error if none are found."
  ;; loop through compilers until 'command -v' returns a 0 exit code...
  (loop for cc in '("clang" "gcc")
          thereis
	  (when
	      (lambda (x) (zerop x)
		(multiple-value-bind (stdout stderr exit-code)
                    (uiop:run-program `("command" "-v" ,cc)
                                      :ignore-error-status t
                                      :output :string
                                      :error-output :string)
		  (declare (ignore stdout stderr))
                  exit-code))
            cc)
        finally (error "No C compiler found. Please install gcc or clang.")))

(defun check-andy-runtime-installed ()
  ;; Ignore stdout and stderr strings, capture exit-code
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program
       '("pkg-config" "--exists" "andy_runtime")
       :ignore-error-status t)
    (declare (ignore stdout stderr))
    (if (zerop exit-code)
	(format t "Andy Runtime Install Check - complete~%")
	(error "Compiler Error: Andy Runtime is not installed."))))

(defun compile-source-to-c (infile)
  (let* ((cfile (concatenate 'string
			     (pathname-name infile) ".c"))
	 (source  (read-file infile))
         (tokens  (tokenize source))
         (ast     (parse tokens)))
    (when (analyze-ast ast)
      (format t "all checks passed.~%"))
    (emit-c ast cfile)
    ;; Compile and link c output
    (format t "C Transpilation complete.~%")
    cfile))

(defun get-andy-runtime-cflags ()
  "Return CFLAGS for andy_runtime."
  (string-right-trim '(#\Space #\Tab #\Newline)
   (uiop:run-program
    '("pkg-config" "--cflags" "andy_runtime")
    :output :string)))

(defun get-andy-runtime-ldflags ()
  "Return LDFLAGS for andy_runtime."
  (string-trim '(#\Space #\Tab #\Newline)
   (uiop:run-program
    '("pkg-config" "--libs" "andy_runtime")
    :output :string)))

(defun compile-c-file (file)
  "Compile the C file produced by the transpiler using pkg-config flags."
  (check-andy-runtime-installed)
  (let ((cfile (compile-source-to-c file)))
    (format t "Wrote ~A~%Compiling ~A to object file....~%" cfile cfile)
    (uiop:run-program
     `(,*CC* ,cfile ,*cflags* ,*ldflags* "-o" ,(pathname-name cfile))
     :output *standard-output*
     :error-output *standard-output*)))

(defun setup-c-compiler ()
  "Find the c compiler and set compiler flags."
  (setf *CC* (detect-c-compiler)
	*cflags* (get-andy-runtime-cflags)
	*ldflags* (get-andy-runtime-ldflags)))

;;; ------------------------------------------------------------------
;;;			     MAIN DRIVER
;;; ------------------------------------------------------------------

(defun parse-args (args)
  "Parse command-line args and set global state vars"
  (loop while args do
    (let ((arg (pop args)))
      (cond
	;; Backend Target: -t TARGET 
	((string= arg "-t")
	 (if args
	     (let ((target (intern (string-upcase (pop args)) :keyword)))
	       (unless (member target '(:C :WASM))
		 (andy-help)))
	     (andy-help)))
	;; Compile Only Predicate: -c
	((string= arg "-c")
	 (setf *compile-only-p* t))
	;; Save Filenames
	(t (push arg *files*)))))
  ;; Set filelist to command-line order
  (if (null *files*)
      (andy-help)
      (setf *files* (reverse *files*))))

(defun andy-help ()
  (write-line "

andyc [options] file.andy [module.o ...]

Options:
  -t              <c|wasm>
  -c              compile only, do not link

")
  (uiop:quit))

(defun main ()
  "Parse command line args and compile files or build project."
  (let ((args (uiop:command-line-arguments))
	(xpiler #'compile-c-file))
    (parse-args args)
    ;; Set the backend
    (if (eq *target* :WASM)
	(setf xpiler #'compile-source-to-wasm)
	(setup-c-compiler))
    ;; Compile Source Files
    (if *compile-only-p*
	(dolist (f *files*)
	  (funcall xpiler f)))))
