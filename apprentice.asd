;; apprentice.asd

(asdf:defsystem #:apprentice
  :description "Library for fusing frontier and local coding agents."
  :author "Sai Karnati"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
	       (:file "helpers")
	       (:file "tool")
	       (:file "llama-cpp-chat")
	       (:file "loop")
	       (:file "apprentice")))
