(defsystem "andy"
  :description "A simple PL/0 compiler targeting WASM"
  :author "Andrew Suttles"
  :license "MIT"
  :version "0.8.0"
  :depends-on (:parse-float)

  ;; Localize the build
  :build-operation asdf:load-op
  :build-pathname "build"

  ;; Source files
  :serial t
  :components
  ((:module "src"
	    :components
	    ((:file "lexer")
	     (:file "ast")
	     (:file "runtime")	     
	     (:file "parser")	     
	     (:file "builtin")	     
	     (:file "analyzer")	     
	     (:file "emitter")
	     (:file "main")))))

