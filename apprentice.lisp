;;;; apprentice.lisp

(in-package :apprentice)


;;;; Harness State


(defparameter *models-list*
  (list *llama-cpp-model*
	*claude-sonnet-5-5-model*
	*gpt-6.2-astra-model*
	*gemini-3.8-flash-model*
	*openrouter-model*))
(defvar *model* *gemini-3.8-flash-model*
  "Default model for the agent loops.")
(defvar *subagent-model* *llama-cpp-model*
  "Model the SUBAGENT tool delegates to.")

(defparameter *anchors-list*
  (list *dense-vector-search-anchor* *file-tree-anchor*))

(defparameter *loops-list*
  '((:standard     . standard-loop)
    (:little-coder . little-coder-loop)
    (:apprentice   . apprentice-loop)))
(defvar *loop* :standard)

(defparameter *convo-stores-list*
  (list *jsonl-convo-store*))
(defvar *convo-store* *jsonl-convo-store*)

(defvar *conversation* (make-conversation)
  "Current conversation object containing chat history and metadata.")

(defvar *options* nil)


;;;; Model Functions


(defun available-models ()
  "Returns available models."
  (let ((model-names (loop for model in *models-list*
			   collect (model-name model))))
    model-names))

(defun model ()
  "Returns current model."
  (model-name *model*))

(defun set-model (name)
  "Sets current model to the one corresponding to NAME."
  (let ((model (find-if (lambda (m) (equalp name (model-name m)))
			*models-list*)))
    (if model
	(progn
	  (format t "Set model to `~a`" name)
	  (setf *model* model))
	(format t "No model named ~a" name))))


;;;; Directory Permissions Functions


(defun allowed-dirs ()
  "Returns allowed directories already set."
  *allowed-dirs*)

(defun add-allowed-dir (dir)
  "Adds an allowed directory to current list."
  (let ((path (expand-dir dir)))
    (unless (member path *allowed-dirs* :test #'equal)
      (push path *allowed-dirs*))
    *allowed-dirs*))

(defun clear-allowed-dirs ()
  "Clears all allowed directories."
  (setf *allowed-dirs* nil))


;;;; Anchor Functions


(defun available-anchors ()
  "Returns available anchors."
  (mapcar #'anchor-name *anchors-list*))

(defun anchors ()
  "Returns currently enabled anchors."
  (mapcar #'anchor-name *anchors*))

(defun set-anchor-dir (dir)
  "Sets the current anchor directory to DIR."
  (let ((new (expand-dir dir)))
    (save-anchors)
    (setf *anchor-dir* new)
    (add-allowed-dir new)
    (dolist (anchor *anchors*)
      (load-anchor anchor new))
    new))

(defun add-anchor (name)
  "Adds an anchor to the current list."
  (let ((anchor (find-if (lambda (a) (equalp name (anchor-name a)))
			 *anchors-list*)))
    (if anchor
	(progn
	  (format t "Added anchor `~a`" name)
	  (unless (member anchor *anchors* :test #'equal)
	    (push anchor *anchors*)))
	(format t "No anchor named ~a" name))))

(defun clear-anchors ()
  "Clears all anchors."
  (setf *anchors* nil))


;;;; Loop Functions


(defun available-loops ()
  "Returns all available coding loops."
  (mapcar #'car *loops-list*))

(defun current-loop ()
  "Returns current coding loop."
  *loop*)

(defun set-loop (loop-value)
  "Sets current coding loop to one corresponding to LOOP-VALUE."
  (setf *loop* loop-value))

(defun resolve-loop (loop-symbol)
  "Resolves a coding loop function from LOOP-SYMBOL."
  (cdr (assoc (if (member loop-symbol '(:default nil t)) :standard loop-symbol)
	      *loops-list*)))


;;;; Option Functions


(defun options ()
  "Currently enabled options passed to coding loop."
  *options*)

(defun add-option (key val)
  "Adds an option through KEY and VAL."
  (setf *options* (remove key *options* :key #'car))
  (push (cons key val) *options*))

(defun clear-options ()
  "Clears all currently set options."
  (setf *options* nil))

(defun unwrap-options (options)
  "Unwraps options to be applied to the coding loop."
  (if (and options (consp (first options)))
      (loop for (key . val) in options
	    append (list key val))
      options))


;;;; Convo Store


(defun available-convo-stores ()
  "Returns all available convo stores."
  (mapcar #'convo-store-name *convo-stores-list*))

(defun set-convo-store (name)
  "Sets convo store to the one corresponding to NAME."
  (let ((convo-store (find-if (lambda (cs) (equalp name (convo-store-name cs)))
			      *convo-stores-list*)))
    (if convo-store
	(progn
	  (format t "Set convo store to `~a`" name)
	  (setf *convo-store* convo-store))
	(format t "No convo store named ~a" name))))


;;;; Conversation Management


(defun ensure-conversation ()
  "Ensures *CONVERSATION* is bound to a conversation struct."
  (if (and (boundp '*conversation*) *conversation*)
      *conversation*
      (setf *conversation* (make-conversation))))

(defun conversation ()
  "Returns the current conversation."
  (ensure-conversation))

(defun format-timestamp (ut)
  "Formats a universal-time timestamp UT into a readable date string."
  (cond ((integerp ut)
	 (multiple-value-bind (sec min hour day month year)
	     (decode-universal-time ut)
	   (declare (ignore sec))
	   (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
		   year month day hour min)))
	((stringp ut) ut)
	(t nil)))

(defun save-chat (&optional name &key description id (from-anchor t) path)
  "Saves the current conversation *CONVERSATION*."
  (let* ((convo (ensure-conversation))
	 (actual-name (or name (conversation-name convo)))
	 (actual-id (or id (conversation-id convo)
			(and actual-name
			     (string-downcase (substitute #\- #\Space actual-name))))))
    (unless actual-name
      (error "Conversation has no name. Please provide one: (save-chat \"name\")"))
    (setf (conversation-name convo) actual-name)
    (setf (conversation-id convo) actual-id)
    (when description
      (setf (conversation-description convo) description))
    (unless (conversation-description convo)
      (setf (conversation-description convo) ""))
    (save-conversation *convo-store* convo :from-anchor from-anchor :path path)
    (format t "Saved conversation `~a` (~a turn~:p)~%"
	    (conversation-name convo)
	    (length (conversation-chat-history convo)))
    convo))

(defun load-chat (id &key (from-anchor t) path)
  "Loads a conversation into *CONVERSATION*."
  (let ((convo (load-conversation *convo-store* id :from-anchor from-anchor :path path)))
    (if convo
	(progn
	  (setf *conversation* convo)
	  (setf *chat-history* (conversation-chat-history convo))
	  (format t "Loaded conversation `~a` (~a turn~:p)~%"
		  (conversation-name convo)
		  (length (conversation-chat-history convo)))
	  *conversation*)
	(error "Conversation not found: ~a" id))))

(defun list-chats (&key (from-anchor t) path)
  "Lists saved conversations."
  (let ((convos (list-conversations *convo-store* :from-anchor from-anchor :path path)))
    (if (null convos)
	(format t "No saved conversations found.~%")
	(dolist (c convos)
	  (let* ((cid   (or (conversation-slim-id c) ""))
		 (name  (or (conversation-slim-name c) ""))
		 (desc  (conversation-slim-description c))
		 (saved (conversation-slim-last-saved c))
		 (time-str (format-timestamp saved)))
	    (format t "~&~a  ~a~@[ [~a]~]~@[ (~a)~]~%"
		    cid
		    name
		    time-str
		    (and desc (plusp (length desc)) desc)))))
    convos))

(defun delete-chat (id &key (from-anchor t) path)
  "Deletes the conversation with corresponding ID."
  (delete-conversation *convo-store* id :from-anchor from-anchor :path path))


;;;; Harness Functions


(defun chat (prompt &optional (loop-symbol *loop*) &rest options)
  "CHAT runs a conversation using the specified loop."
  (let ((loop-fn (resolve-loop loop-symbol))
	(convo (ensure-conversation)))
    (if loop-fn
	(progn
	  (when (not *allowed-dirs*)
	    (format t "Warning: No allowed dirs set. Use `add-allowed-dir` for proper tool use~%"))
	  (handler-case
	      (when *anchor-dir*
		(process-dir *anchors* *anchor-dir*))
	    (error () "Unable to process anchors"))
	  (destructuring-bind (content msgs)
	      (apply loop-fn prompt
		     :history (conversation-chat-history convo)
		     (or options (unwrap-options *options*)))
	    (setf (conversation-chat-history convo) msgs)
	    (setf *chat-history* msgs)
	    (format t "~a" content)))
	(error "Unknown loop: ~a" loop-symbol))))

(defun clear ()
  "Clears the current conversation."
  (setf *conversation* (make-conversation))
  (setf *chat-history* nil)
  *conversation*)

(defun drop-turns (&rest specs)
  "Drop the turns of the current conversation named by SPECS, where a spec is an
   index or an inclusive range, (LO . HI) or (LO HI)."
  (let* ((convo (ensure-conversation))
	 (history (conversation-chat-history convo))
	 (doomed (expand-index-specs specs (length history))))
    (setf (conversation-chat-history convo)
	  (loop for turn in history
		for i from 0
		unless (member i doomed) collect turn))
    (setf *chat-history* (conversation-chat-history convo))
    (conversation-chat-history convo)))

(defun turn-body (turn)
  "The content worth showing for TURN. A :tool-results turn carries no
   text at all -- its content is the (ID NAME OUTPUT) triples -- so the
   tool output is what gets shown for one."
  (if (eq (turn-role turn) :tool-results)
      (format nil "~{~a~^ | ~}"
	      (mapcar (lambda (r) (format nil "~a: ~a" (second r) (third r)))
		      (turn-results turn)))
      (or (turn-text turn) "")))

(defvar *preview-limit* 100
  "Characters of a turn's content SHOW-TURNS prints before cutting it.")

(defun set-preview-limit (val)
  (setf *preview-limit* val))

(defun show-turns (&rest specs)
  "Print the turns of the current conversation named by SPECS, or every turn when
   given none. A spec is an index or an inclusive range, (LO . HI) or
   (LO HI), and a negative index counts from the end."
  (let* ((convo (ensure-conversation))
	 (history (conversation-chat-history convo))
	 (at    (position :limit specs))
	 (limit (or (and at (nth (1+ at) specs)) *preview-limit*))
	 (specs (if at
		    (append (subseq specs 0 at) (nthcdr (+ at 2) specs))
		    specs))
	 (n     (length history))
	 (idx   (if specs
		    (expand-index-specs specs n)
		    (loop for i below n collect i))))
    (dolist (i idx)
      (let ((turn (nth i history)))
	(format t "~&~3d  ~14a ~a~a~%"
		i
		(turn-role turn)
		(if (turn-calls turn)
		    (format nil "[~{~a~^ ~}] "
			    (mapcar #'tool-call-name (turn-calls turn)))
		    "")
		(ellipsize (one-line (turn-body turn)) limit))))
    (format t "~&~a of ~a turn~:p shown.~%" (length idx) n))
  (values))
