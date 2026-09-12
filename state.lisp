;;;; state.lisp
;;;;
;;;; Harness state is declared here, ahead of the files that use it, so a
;;;; reference from an earlier file is not an undefined-variable warning.
;;;; Variables whose values depend on later files are declared without
;;;; one and initialised in apprentice.lisp; reading one before then is
;;;; an unbound-variable error rather than a silent NIL.

(in-package :apprentice)


(defvar *chat-history* nil)
(defvar *allowed-dirs* nil)
(defvar *anchor-dir* nil)
(defvar *anchors* nil)

(defvar *model*)
(defvar *subagent-model*)
(defvar *subagent-tools*)
