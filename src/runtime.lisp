(defpackage :andy.runtime
  (:use :cl)
  (:export :initialize-memory
   :get-runtime
   :allocate-constant-memory
   :allocate-heap-memory
   :import-math))

(in-package :andy.runtime)

(defparameter *io-runtime*
  (uiop:read-file-string "~/programming/lisp/andy/inc/io.wat"))
(defparameter *math-runtime*
  (uiop:read-file-string "~/programming/lisp/andy/inc/math.wat"))

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

(defvar *constant-offset* 1)	; Offset 0 reserved for /n
(defvar *heap-offset* 0)

(defun initialize-memory ()
  "Re-initialize memory to initial conditions."
  (setf *constant-offset* 1
	*heap-offset* 0))

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
