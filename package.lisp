;;;; package.lisp


(defpackage #:apprentice
  (:use #:cl)
  (:export ;; Conversation
	   #:chat
	   #:clear
	   #:drop-turns
	   #:show-turns
	   #:resolve-loop
	   ;; Models
	   #:models
	   #:curr-model
	   #:set-model
	   ;; Anchors
	   #:anchors
	   #:available-anchors
	   #:add-anchor
	   #:clear-anchors
	   #:set-anchor-dir
	   ;; Permissions
	   #:add-allowed-dir
	   #:clear-allowed-dirs
	   ;; Loops
	   #:curr-loop
	   #:set-loop
	   ;; Options
	   #:options
	   #:add-option
	   #:clear-options))
