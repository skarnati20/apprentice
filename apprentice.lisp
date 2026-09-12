;;;; apprentice.lisp

(in-package :apprentice)


;;;; Harness State


(defparameter *models-list*
  (list *llama-cpp-model*
	*claude-sonnet-5-model*
	*gpt-5.6-terra-model*
	*gemini-3.7-flash-model*
	*openrouter-model*))
(defparameter *model* *llama-cpp-model*
  "Default model for the agent loops.")
(defparameter *subagent-model* *llama-cpp-model*
  "Model the SUBAGENT tool delegates to.")

(defparameter *anchors-list*
  (list *dense-vector-search-anchor* *file-tree-anchor*))

(defparameter *loop* :standard)

(defparameter *options* nil)


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


;;;; Option Functions


(defun options ()
  *options*)

(defun add-option (key val)
  (setf *options* (remove key *options* :key #'car))
  (push (cons key val) *options*))

(defun clear-options ()
  (setf *options* nil))

(defun unwrap-options (options)
  (if (and options (consp (first options)))
      (loop for (key . val) in options
	    append (list key val))
      options))


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
	      (apply loop-fn prompt :history *chat-history* (or options (unwrap-options *options*)))
	    (setf *chat-history* msgs)
	    (format t "~a" content)))
	(error "Unknown loop: ~a" loop-symbol))))

(defun clear ()
  (setf *chat-history* nil))

(defun drop-turns (&rest specs)
  "Drop the turns of *CHAT-HISTORY* named by SPECS, where a spec is an
   index or an inclusive range, (LO . HI) or (LO HI)."
  (let ((doomed (expand-index-specs specs (length *chat-history*))))
    (setf *chat-history*
	  (loop for turn in *chat-history*
		for i from 0
		unless (member i doomed) collect turn))
    *chat-history*))

(defun turn-body (turn)
  "The content worth showing for TURN. A :tool-results turn carries no
   text at all -- its content is the (ID NAME OUTPUT) triples -- so the
   tool output is what gets shown for one."
  (if (eq (turn-role turn) :tool-results)
      (format nil "~{~a~^ | ~}"
	      (mapcar (lambda (r) (format nil "~a: ~a" (second r) (third r)))
		      (turn-results turn)))
      (or (turn-text turn) "")))

(defparameter *preview-limit* 100
  "Characters of a turn's content SHOW-TURNS prints before cutting it.")

(defun show-turns (&rest specs)
  "Print the turns of *CHAT-HISTORY* named by SPECS, or every turn when
   given none. A spec is an index or an inclusive range, (LO . HI) or
   (LO HI), and a negative index counts from the end. Content is cut to
   *PREVIEW-LIMIT* characters. Read-only: nothing is modified."
  (let* ((n   (length *chat-history*))
	 (idx (if specs
		  (expand-index-specs specs n)
		  (loop for i below n collect i))))
    (dolist (i idx)
      (let ((turn (nth i *chat-history*)))
	(format t "~&~3d  ~14a ~a~a~%"
		i
		(turn-role turn)
		(if (turn-calls turn)
		    (format nil "[~{~a~^ ~}] "
			    (mapcar #'tool-call-name (turn-calls turn)))
		    "")
		(ellipsize (one-line (turn-body turn)) *preview-limit*))))
    (format t "~&~a of ~a turn~:p shown.~%" (length idx) n))
  (values))
