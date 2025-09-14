(defsystem "andy"
  :description "A simple PL/0 compiler targeting WASM"
  :author "Andrew Suttles"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :serial t
  :components
  ((:module "src"
	    :components
	    ((:file "lexer")
	     (:file "parser")
	     (:file "ast")
	     (:file "ir")
	     (:file "wasm")
	     (:file "main")))))

