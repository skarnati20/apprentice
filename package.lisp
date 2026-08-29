;;;; package.lisp


(defpackage #:apprentice
  (:use #:cl)
  (:export ;; Conversation
	   #:chat
	   #:clear
	   #:resolve-loop
	   ;; Models
	   #:models
	   #:curr-model
	   #:set-model
	   ;; Permissions
	   #:add-allowed-dir
	   #:clear-allowed-dirs))
