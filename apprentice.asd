;; apprentice.asd


(asdf:defsystem #:apprentice
  :description "Library for fusing frontier and local coding agents."
  :author "Sai Karnati"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:bordeaux-threads)
  :components ((:file "package")
	       (:file "state")
	       (:file "prompts")
	       (:file "file")
	       (:file "chunk")
	       (:file "helpers")
               (:file "openai-auth")
               (:file "openai-auth-http")
               (:file "openai-auth-store")
	       (:file "embedding")
	       (:file "anchor")
	       (:file "tool")
	       (:file "subagent")
	       (:file "model")
	       (:file "loop")
	       (:file "apprentice")
               (:file "openai-codex"))
  :in-order-to ((test-op (test-op "apprentice/tests"))))

(asdf:defsystem #:apprentice/tests
  :depends-on (#:apprentice)
  :serial t
  :components ((:file "tests/support")
               (:file "tests/openai-auth")
               (:file "tests/openai-auth-http")
               (:file "tests/openai-auth-store")
               (:file "tests/openai-auth-repl")
               (:file "tests/openai-codex"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :apprentice-tests :run-tests)))
