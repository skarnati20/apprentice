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
	       (:file "embedding")
	       (:file "anchor")
	       (:file "tool")
	       (:file "subagent")
	       (:file "model")
	       (:file "loop")
	       (:file "conversations")
	       (:file "apprentice")))
