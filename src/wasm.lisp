(defpackage :andy.wasm
  (:use :cl)
  (:export :emit-wasm))

(in-package :andy.wasm)

(defun emit-wasm (ir)
  ;; stub for now
  (format t "Emitting WASM: ~A~%" ir)
  '(:wasm))
