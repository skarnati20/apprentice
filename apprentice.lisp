;;;; apprentice.lisp

(in-package :apprentice)


;;;; Harness State


(defvar *chat-history* nil)
(defvar *allowed-dirs* nil)
(defvar *anchor-dir* nil)

(defparameter *models-list*
  (list *llama-cpp-model*
	*claude-sonnet-5-model*
	*gpt-5.6-terra-model*
	*gemini-3.7-flash-model*
	*openrouter-model*))
(defparameter *model* *llama-cpp-model*
  "Default model for the agent loops.")

(defparameter *anchors-list*
  (list *dense-vector-search-anchor*))
(defvar *anchors* nil)

(defparameter *loop* :standard)


;;;; Model Functions


(defun models ()
  (let ((model-names (loop for model in *models-list*
			   collect (model-name model))))
    model-names))

(defun curr-model ()
  (model-name *model*))

(defun set-model (name)
  (let ((model (find-if (lambda (m) (equalp name (model-name m)))
			*models-list*)))
    (if model
	(progn
	  (format t "Set model to `~a`" name)
	  (setf *model* model))
	(format t "No model named ~a" name))))


;;;; Directory Permissions Functions


(defun add-allowed-dir (dir)
  (let ((path (expand-dir dir)))
    (unless (member path *allowed-dirs* :test #'equal)
      (push path *allowed-dirs*))
    *allowed-dirs*))

(defun clear-allowed-dirs ()
  (setf *allowed-dirs* nil))


;;;; Anchor Functions


(defun set-anchor-dir (dir)
  (let ((new (expand-dir dir)))
    (save-anchors)
    (setf *anchor-dir* new)
    (add-allowed-dir new)
    (dolist (anchor *anchors*)
      (load-anchor anchor new))
    new))

(defun add-anchor (name)
  (let ((anchor (find-if (lambda (a) (equalp name (anchor-name a)))
			 *anchors-list*)))
    (if anchor
	(progn
	  (format t "Added anchor `~a`" name)
	  (unless (member anchor *anchors* :test #'equal)
	    (push anchor *anchors*)))
	(format t "No anchor named ~a" name))))

(defun anchors ()
  (mapcar #'anchor-name *anchors*))

(defun available-anchors ()
  (mapcar #'anchor-name *anchors-list*))

(defun clear-anchors ()
  (setf *anchors* nil))


;;;; Loop Functions


(defun curr-loop ()
  *loop*)

(defun set-loop (loop-value)
  (setf *loop* loop-value))

(defun resolve-loop (loop-symbol)
  "Returns the appropriate loop function symbol or nil if unknown."
  (case loop-symbol
    (:little-coder 'little-coder-loop)
    (:apprentice 'apprentice-loop)
    ((:standard :default nil t) 'standard-loop)
    (otherwise nil)))


;;;; Harness Functions


(defun chat (prompt &optional (loop-symbol *loop*) &rest options)
  "CHAT runs a conversation using the specified loop.
   
  LOOP-SYMBOL can be one of:
    :little-coder - use little-coder-loop
    :standard - use standard-loop
    :default - use standard-loop (default)
    nil or t - use standard-loop (default)"
  (let ((loop-fn (resolve-loop loop-symbol)))
    (if loop-fn
	(progn
	  (handler-case
	      (when *anchor-dir*
		(process-dir *anchors* *anchor-dir*))
	    (error () "Unable to process anchors"))
	  (destructuring-bind (content msgs)
	      (apply loop-fn prompt :history *chat-history* options)
	    (setf *chat-history* msgs)
	    (format t "~a" content)))
	(error "Unknown loop: ~a" loop-symbol))))

(defun clear ()
  (setf *chat-history* nil))

(defun drop-turns (n)
  "Drop the first N turns from *CHAT-HISTORY*, excluding the system
   prompt (the leading turn with role :system), which is always kept."
  (let ((n (max 0 n)))
    (if (and *chat-history* (eq (turn-role (first *chat-history*)) :system))
        (setf *chat-history*
              (cons (first *chat-history*)
                    (nthcdr n (rest *chat-history*))))
        (setf *chat-history* (nthcdr n *chat-history*)))
    *chat-history*))

