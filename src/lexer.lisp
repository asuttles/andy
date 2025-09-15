(defpackage :andy.lexer
  (:use :cl :uiop)
  (:export :tokenize :read-file))

(in-package :andy.lexer)

;;; token datatype
(defstruct token
  type
  lexeme
  line
  column)

;;; Slurp source file into memory
(defun read-file (filename)
  ;; stub for now
  (format t "Reading file: ~A~%~%" filename)
  (uiop:read-file-string filename))

(defun tokenize (source)
  ;; stub for now
  (format t "Tokenizing: ~A~%" source)
  '(:int :identifier :assign :number :semicolon))
