(defpackage :andy.parser
  (:use :cl)
  (:export :parse))

(in-package :andy.parser)

(defun parse (tokens)
  ;; stub for now
  (format t "Parsing tokens: ~A~%" tokens)
  '(:ast-root))
