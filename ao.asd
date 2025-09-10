;;;; ao.asd

(asdf:defsystem #:ao
  :description "An Oberon-0 compiler in Common Lisp"
  :author "Andy Suttles"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:module "src"
                 :serial t
                 :components
                   ((:file "lexer")
                    (:file "parser")
                    (:file "ast")
                    (:file "ir")
                    (:file "wasm")
                    (:file "main")))))

