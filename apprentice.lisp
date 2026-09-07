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
  nil)
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


(defvar *apprentice-folder-name*
  ".apprentice/")

(defun apprentice-folder (dir)
  (uiop:subpathname (expand-dir dir) *apprentice-folder-name*))

(defun create-apprentice-folder (dir)
  (let ((path (apprentice-folder dir)))
    (ensure-directories-exist path)
    path))

(defun save-anchors (&optional (dir *anchor-dir*))
  (when (and dir *anchors*)
    (let ((folder (create-apprentice-folder dir)))
      (dolist (anchor *anchors*)
	(funcall (anchor-serialize-fn anchor) folder))
      folder)))

(defun load-anchor (anchor dir)
  (setf (anchor-bindings anchor)
	(funcall (anchor-init-bindings-fn anchor)))
  (let ((folder (apprentice-folder dir)))
    (when (uiop:directory-exists-p folder)
      (handler-case (funcall (anchor-deserialize-fn anchor) folder)
	(error (e)
	  (warn "Anchor ~a: could not load state from ~a (~a). Using defaults."
		(anchor-name anchor) folder e)))))
  anchor)

(defun set-anchor-dir (dir)
  (let ((new (expand-dir dir)))
    (save-anchors)
    (setf *anchor-dir* new)
    (add-allowed-dir new)
    (dolist (anchor *anchors*)
      (load-anchor anchor new))
    new))

(defun set-anchors (anchors)
  (setf *anchors* anchors)
  (when *anchor-dir*
    (dolist (anchor *anchors*)
      (load-anchor anchor *anchor-dir*)))
  (mapcar #'anchor-name *anchors*))

(defun anchors ()
  (mapcar #'anchor-name *anchors*))


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
        (destructuring-bind (content msgs)
            (apply loop-fn prompt :history *chat-history* options)
          (setf *chat-history* msgs)
          (format t "~a" content))
	(error "Unknown loop: ~a" loop-symbol))))

(defun clear ()
  (setf *chat-history* nil))
