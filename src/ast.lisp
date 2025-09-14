(defpackage :andy.ast
  (:use :cl)
  (:export :generate-ast))

(in-package :andy.ast)

(defun generate-ast (tokens)
  ;; stub for now
  (format t "Parsing tokens: ~A~%" tokens)
  '(:ast-root))
