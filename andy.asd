(defsystem "andy"
  :description "A simple PL/0 compiler targeting WASM"
  :author "Andrew Suttles"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()

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
	     (:file "parser")
	     (:file "analyzer")
	     (:file "wasm")
	     (:file "main")))))

