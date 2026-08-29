;;;; apprentice.lisp

(in-package :apprentice)


;;;; Harness State


(defvar *chat-history* nil)
(defvar *allowed-dirs* nil)

(defvar *models-list*
  (list *llama-cpp-model*
	*claude-sonnet-5-model*
	*gpt-5.6-terra-model*))

(defvar *model* *llama-cpp-model*
  "Default model for the agent loops.")


;;;; Harness Functions


(defun resolve-loop (loop-symbol)
  "Returns the appropriate loop function symbol or nil if unknown."
  (case loop-symbol
    (:little-coder 'little-coder-loop)
    ((:standard :default nil t) 'standard-loop)
    (otherwise nil)))

(defun chat (prompt &optional loop-symbol &rest options)
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

(defun print-models ()
  (let ((model-names (loop for model in *models-list*
			   collect (model-name model))))
    (mapcar (lambda (model) (format t "~a" model)) model-names)
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

(defun add-allowed-dir (dir)
  (push dir *allowed-dirs*))

(defun clear-allowed-dirs ()
  (setf *allowed-dirs* nil))

(defun clear ()
  (setf *chat-history* nil))
