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

;;; Compiler Version
(defparameter +andy-version+ "0.9.4")

;;; Compiler State
(defvar *target* :C)
(defvar *compile-only-p* nil) 		; Compile source or build project
(defvar *files* nil)
(defvar *exefile* nil)			; Executable name

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

(defun compile-andy-to-c (infile)
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
  (uiop:split-string
   (string-trim
    '(#\Space #\Tab #\Newline)
    (uiop:run-program
     '("pkg-config" "--cflags" "andy_runtime")
     :output :string))))

(defun get-andy-runtime-ldflags ()
  "Return LDFLAGS for andy_runtime."
  (uiop:split-string 
   (string-trim
    '(#\Space #\Tab #\Newline)
    (uiop:run-program
     '("pkg-config" "--libs" "andy_runtime")
     :output :string))))

(defun compile-c-file (cfile)
  "Compile c file to objfile."
  (check-andy-runtime-installed)  
  (let* ((ofile
	   (concatenate
	    'string (pathname-name cfile) ".o")))
    (format t "Compiling ~A to ~A...~%" cfile ofile)
    (uiop:run-program
     `(,*CC* "-c" ,cfile ,@*cflags* "-o" ,ofile)
     :output *standard-output*
     :error-output *standard-output*)
    ofile))

(defun setup-c-compiler ()
  "Find the c compiler and set compiler flags."
  (setf *CC* (detect-c-compiler)
	*cflags* (get-andy-runtime-cflags)
	*ldflags* (get-andy-runtime-ldflags)))

(defun compile-file-to-obj (file)
  (let ((ext (pathname-type file)))
    (cond
      ;; Object Files
      ((string= ext "o") file)
      ;; C Files
      ((string= ext "c")
       (compile-c-file file))
      ;; Andy Files
      ((string= ext "andy")
       (compile-c-file
	(compile-andy-to-c file)))
      ;; Unknown filetype
      (t (error "Unknown filetype for file: ~A" f)))))

(defun build-c-project ()
  "Build the executable file"
  (let ((ofiles '()))
    (dolist (f *files*)
      (push (compile-file-to-obj f) ofiles))
    (unless *exefile* (setf *exefile* "a.exe"))
    (let ((build `(,*CC* ,@ofiles ,@*ldflags* "-o" ,*exefile*)))
      (format t "Building executable: ~A~%" *exefile*)
      (uiop:run-program build
			:output *standard-output*
			:error-output *standard-output*))))

;;; ------------------------------------------------------------------
;;;			     MAIN DRIVER
;;; ------------------------------------------------------------------

(defun andy-version ()
  "Print the compiler version"
  (format t "andyc version ~A~%" +andy-version+)
  (uiop:quit))

(defun andy-help ()
  "Print a helpful message before bailing..."
  (write-line "

andyc [options] FILE.[andy c o]

Options:
  -o name      name of executable
  -t <c|wasm>  backend target c or wasm
  -c           compile only, do not link
  -v           print version
  -h           print this help message

")
  (uiop:quit))

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
	;; Output filename
	((string= arg "-o")
	 (if args
	     (setf *exefile* (pop args))))
	;; Print Compiler Version
	((string= arg "-v")
	 (andy-version))
	;; Print Help Message
	((string= arg "-h")
	 (andy-help))
	;; Unknown Switch
	((char= (char arg 0) #\-)
	 (progn
	   (format t "Unknown switch: ~A~%" arg)
	   (andy-help)))
	;; Save Filenames
	(t (push arg *files*)))))
  ;; Set filelist to command-line order
  (if (null *files*)
      (andy-help)
      (setf *files* (reverse *files*))))

(defun run-compiler ()
  "Parse command line args and compile files or build project."
  (let ((args (uiop:command-line-arguments))
	(xpiler #'compile-file-to-obj))
    (parse-args args)
    ;; Set the backend target (-t)
    (if (eq *target* :WASM)
	(setf xpiler #'compile-source-to-wasm)
	(setup-c-compiler))
    ;; Compile Source Files (-c)
    (if *compile-only-p*
	(dolist (f *files*)
	  (funcall xpiler f))
	;; Build Project
	(build-c-project))))

(defun main ()
  "Driver for compiler w/error handler"
  ;; Exception Handler
  ;;  - prevents program errors from launching debugger
  (handler-case
      (progn
        (run-compiler)
        (sb-ext:exit :code 0))
    (error (e)
      (format *error-output* "Internal compiler error: ~a~%" e)
      (sb-ext:exit :code 1))))

