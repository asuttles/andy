(defpackage :andy.ir
  (:use :cl)
  (:export :generate-ir))

(in-package :andy.ir)

(defun generate-ir (ast)
  ;; stub for now
  (format t "Generating Intermediate Representation: ~A~%" ast)
  '(:ir))
