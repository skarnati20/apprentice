;;;; state.lisp

(in-package :apprentice)


(defvar *chat-history* nil)
(defvar *allowed-dirs* nil)
(defvar *anchor-dir* nil)
(defvar *anchors* nil)

(defvar *model*)
(defvar *models-list* nil)
(defvar *loop* :standard)
(defvar *options* nil)
(defvar *preview-limit* 100)

(defvar *subagent-model*)
(defvar *subagent-tools*)

(defvar *subagent-loop*)
(defvar *subagent-report-limit*)
(defvar *subagent-brief-limit*)

;; Used by subagent.lisp, defined in loop.lisp.
(defvar *trace-lock*)
