(defpackage :andy.runtime
  (:use :cl)
  (:export :initialize-runtime
   :get-runtime
   :allocate-constant-memory
   :allocate-heap-memory
   :import-math))

(in-package :andy.runtime)

;; Initial Runtime State
(defvar *io-runtime* "")
(defvar *math-runtime* "")
(defvar *import-math-p* nil)
(defvar *import-io-p* t)

(defun import-math ()
  (unless *import-math-p*
    (setf *import-math-p* t)))

(defparameter *memory-map*
  '((:const-base        . #x0000)
    (:globals-base      . #x0400)
    (:runtime-headers   . #x0800)
    (:runtime-buffers   . #x0900)
    (:heap-base         . #x1000)
    (:stack-top         . #xF000)))

;; 140 bytes reserved for termainal escape sequences
(defconstant *CONST-MEMORY* 140)
(defvar *constant-offset* *CONST-MEMORY*)
(defvar *heap-offset* 0)

(defun initialize-runtime ()
  "Re-initialize memory to initial conditions."
  (setf *constant-offset* *CONST-MEMORY*
	*heap-offset* 0
	*io-runtime* (uiop:read-file-string "~/programming/lisp/andy/inc/io.wat")
	*math-runtime* (uiop:read-file-string "~/programming/lisp/andy/inc/math.wat")
	*import-math-p* nil
	*import-io-p* t))

(defun get-addr (key)
  (cdr (assoc key *memory-map*)))

(defun get-runtime ()
  (let ((output ""))
    (dolist (runtime
	     '((*import-io-p* *io-runtime*)
	       (*import-math-p* *math-runtime*)))
      (if (symbol-value (car runtime))
	  (setf output
		(concatenate 'string output
			     (symbol-value (cadr runtime))))))
    output))

(defun allocate-constant-memory (size)
  (prog1
      (+ (get-addr :const-base) *constant-offset*)
    (setf *constant-offset* (+ size *constant-offset*))))

(defun allocate-heap-memory (size)
  (prog1
      (+ (get-addr :heap-base) *heap-offset*)
    (setf *heap-offset* (+ size *heap-offset*))))
