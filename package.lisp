;;;; package.lisp


(defpackage #:apprentice
  (:use #:cl)
  (:export ;; Conversation
   #:chat
   #:clear
   #:drop-turns
   #:set-preview-limit
   #:show-turns
   #:save-chat
   #:load-chat
   #:list-chats
   #:delete-chat
   ;; Presets
   #:save-preset
   #:load-preset
   #:edit-preset
   #:list-preset
   #:apply-preset-data
   ;; Models
   #:available-models
   #:model
   #:set-model
   #:subagent-model
   #:set-subagent-model
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
