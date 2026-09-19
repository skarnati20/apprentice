;;;; package.lisp


(defpackage #:apprentice
  (:use #:cl)
  (:export ;; Conversation
   #:chat
   #:clear
   #:drop-turns
   #:set-preview-limit
   #:show-turns
   ;; Models
   #:available-models
   #:model
   #:set-model
   ;; OpenAI device authentication
   #:make-openai-auth-context
   #:openai-login
   #:openai-auth-status
   #:openai-logout
   #:configure-openai-device-model
   ;; Anchors
   #:anchors
   #:available-anchors
   #:add-anchor
   #:clear-anchors
   #:set-anchor-dir
   ;; Permissions
   #:allowed-dirs
   #:add-allowed-dir
   #:clear-allowed-dirs
   ;; Loops
   #:available-loops
   #:current-loop
   #:set-loop
   #:resolve-loop
   ;; Options
   #:options
   #:add-option
   #:clear-options))
