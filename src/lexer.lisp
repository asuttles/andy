(defpackage :andy.lexer
  (:use :cl)
  (:export :tokenize :read-file))

(in-package :andy.lexer)

(defun read-file (filename)
  ;; stub for now
  (format t "Reading file: ~A~%" filename)
  '("int" "x" ":=" "5" ";"))

(defun tokenize (source)
  ;; stub for now
  (format t "Tokenizing: ~A~%" source)
  '(:int :identifier :assign :number :semicolon))
