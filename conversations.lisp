;;;; conversations.lisp

(in-package :apprentice)


(defstruct conversation
  "A conversation including its name, description, and
   chat history. Also indicates last saved date."
  id
  name
  description
  chat-history
  save-path
  last-saved)

(defstruct conversation-slim
  "A conversation but just the surface details."
  id
  name
  description
  last-saved)

(defstruct convo-store
  "Object for storing and retrieving conversations"
  name
  description
  list-fn
  store-fn
  load-fn
  del-fn)


;;;; JSONL Conversation Storage


(defparameter *convo-save-dir* "~/.apprentice/conversations/")

(defun get-anchor-convo-dir ()
  "The place to save conversations in an anchor directory."
  (if *anchor-dir*
      (expand-dir (uiop:subpathname (uiop:ensure-directory-pathname *anchor-dir*) ".apprentice/conversations/"))
      (error "No anchor dir specified for conversation storage")))

(defun convo-base-dir (&key (path nil) (from-anchor nil))
  "Resolves the base directory for conversations."
  (expand-dir
   (or path
       (if (and from-anchor *anchor-dir*)
           (get-anchor-convo-dir)
           *convo-save-dir*))))

(defun resolve-chat-history-path (path)
  "Resolves PATH to chat-history.jsonl file pathname."
  (let ((p (pathname (expand-dir path))))
    (uiop:subpathname (uiop:ensure-directory-pathname p) "chat-history.jsonl")))

(defun resolve-metadata-path (path)
  "Resolves PATH to metadata.json file pathname."
  (let ((p (pathname (expand-dir path))))
    (uiop:subpathname (uiop:ensure-directory-pathname p) "metadata.json")))

(defun resolve-convo-dir (id-or-path &key (path nil) (from-anchor nil))
  "Resolves ID-OR-PATH to a conversation directory pathname."
  (let* ((base (uiop:ensure-directory-pathname
                (convo-base-dir :path path :from-anchor from-anchor)))
         (as-id (uiop:ensure-directory-pathname
                 (uiop:subpathname base (format nil "~a/" id-or-path))))
         (as-path (uiop:ensure-directory-pathname (expand-dir id-or-path))))
    (cond ((uiop:file-exists-p (resolve-metadata-path as-id)) as-id)
          ((uiop:file-exists-p (resolve-metadata-path as-path)) as-path)
          ((and from-anchor *anchor-dir*
                (let ((global-dir (uiop:ensure-directory-pathname
                                   (uiop:subpathname (uiop:ensure-directory-pathname (expand-dir *convo-save-dir*))
                                                     (format nil "~a/" id-or-path)))))
                  (when (uiop:file-exists-p (resolve-metadata-path global-dir))
                    global-dir))))
          (t as-id))))

(defun load-metadata-file (path)
  (let ((metadata-path (resolve-metadata-path path)))
    (when (uiop:file-exists-p metadata-path)
      (cl-json:decode-json-from-string
       (uiop:read-file-string metadata-path)))))

(defun write-metadata-file (path id name description &optional last-saved)
  (let* ((metadata-path (resolve-metadata-path path))
         (fields (append (j "id" id
                            "name" name
                            "description" (or description ""))
                         (when last-saved
                           (j "last_saved" last-saved))))
         (metadata-body (lisp-to-corrected-json-string fields)))
    (ensure-directories-exist metadata-path)
    (uiop:with-output-file (stream metadata-path
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string metadata-body stream))
    metadata-path))

(defun turn->json (turn)
  "Converts a TURN struct into a JSON-encodable alist."
  (append
   (when (turn-role turn)
     (j "role" (string-downcase (symbol-name (turn-role turn)))))
   (when (turn-text turn)
     (j "text" (turn-text turn)))
   (when (turn-thinking turn)
     (j "thinking" (turn-thinking turn)))
   (when (turn-calls turn)
     (j "calls"
        (mapcar (lambda (c)
                  (j "id" (tool-call-id c)
                     "name" (tool-call-name c)
                     "args" (cond ((hash-table-p (tool-call-args c))
                                   (tool-call-args c))
                                  ((listp (tool-call-args c))
                                   (args-object (tool-call-args c)))
                                  (t (tool-call-args c)))))
                (turn-calls turn))))
   (when (turn-results turn)
     (j "results" (turn-results turn)))
   (when (turn-stop turn)
     (j "stop" (string-downcase (symbol-name (turn-stop turn)))))
   (when (turn-raw turn)
     (j "raw" (turn-raw turn)))))

(defun json->turn (obj)
  "Reconstructs a TURN struct from a decoded JSON object OBJ."
  (let* ((role-str (s obj "role"))
         (role (when role-str (intern (string-upcase role-str) :keyword)))
         (text (s obj "text"))
         (thinking (s obj "thinking"))
         (calls-raw (s obj "calls"))
         (calls (when calls-raw
                  (mapcar (lambda (c)
                            (make-tool-call
                             :id (s c "id")
                             :name (s c "name")
                             :args (s c "args")))
                          calls-raw)))
         (results (s obj "results"))
         (stop-str (s obj "stop"))
         (stop (when stop-str (intern (string-upcase stop-str) :keyword)))
         (raw (s obj "raw")))
    (make-turn
     :role role
     :text text
     :thinking thinking
     :calls calls
     :results results
     :stop stop
     :raw raw)))

(defun write-chat-history-jsonl (path chat-history)
  "Writes CHAT-HISTORY list of turns to chat-history.jsonl in PATH."
  (let ((history-path (resolve-chat-history-path path)))
    (ensure-directories-exist history-path)
    (uiop:with-output-file (stream history-path
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (dolist (turn chat-history)
        (write-line (lisp-to-corrected-json-string (turn->json turn)) stream)))
    history-path))

(defun load-chat-history (path)
  "Loads list of turns from chat-history.jsonl in PATH."
  (let* ((history-path (resolve-chat-history-path path))
         (turns nil))
    (when (uiop:file-exists-p history-path)
      (with-open-file (stream history-path :direction :input
                                           :if-does-not-exist nil)
        (when stream
          (loop for line = (read-line stream nil nil)
                while line
                do (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                     (when (> (length trimmed) 0)
                       (let ((obj (cl-json:decode-json-from-string trimmed)))
                         (when obj
                           (push (json->turn obj) turns)))))))))
    (nreverse turns)))

(defun load-conversation-slim (path)
  "Loads surface details of a conversation into CONVERSATION-SLIM."
  (let* ((dir (uiop:ensure-directory-pathname (expand-dir path)))
         (metadata (load-metadata-file dir))
         (meta-path (resolve-metadata-path dir)))
    (when metadata
      (make-conversation-slim
       :id (s metadata "id")
       :name (s metadata "name")
       :description (s metadata "description")
       :last-saved (or (s metadata "last_saved")
                       (and (uiop:file-exists-p meta-path)
                            (file-write-date meta-path)))))))

(defun load-conversation-from-dir (path)
  "Loads full details of a conversation into CONVERSATION."
  (let* ((dir (uiop:ensure-directory-pathname (expand-dir path)))
         (metadata (load-metadata-file dir))
         (meta-path (resolve-metadata-path dir)))
    (when metadata
      (make-conversation
       :id (s metadata "id")
       :name (s metadata "name")
       :description (s metadata "description")
       :chat-history (load-chat-history dir)
       :save-path dir
       :last-saved (or (s metadata "last_saved")
                       (and (uiop:file-exists-p meta-path)
                            (file-write-date meta-path)))))))

(defun save-jsonl-convo (conversation &key (path nil) (from-anchor nil))
  "Saves a CONVERSATION to disk in JSONL and metadata format."
  (let* ((id (conversation-id conversation))
         (name (conversation-name conversation))
         (description (or (conversation-description conversation) ""))
         (chat-history (conversation-chat-history conversation))
         (base (convo-base-dir :from-anchor from-anchor))
         (write-path (expand-dir
                      (or path
                          (conversation-save-path conversation)
                          (if id
                              (uiop:subpathname (uiop:ensure-directory-pathname base)
                                                (format nil "~a/" id))
                              base))))
         (now (get-universal-time)))
    (write-metadata-file write-path id name description now)
    (write-chat-history-jsonl write-path chat-history)
    (setf (conversation-save-path conversation) write-path)
    (setf (conversation-last-saved conversation) now)
    conversation))

(defun load-jsonl-convo (id-or-path &key (path nil) (from-anchor nil) (slim nil))
  "Loads a conversation from disk. When SLIM is true, returns CONVERSATION-SLIM."
  (let ((dir (resolve-convo-dir id-or-path :path path :from-anchor from-anchor)))
    (if slim
        (load-conversation-slim dir)
        (load-conversation-from-dir dir))))

(defun list-jsonl-convos (&key (path nil) (from-anchor nil))
  "Lists conversations in base directory as CONVERSATION-SLIM structs."
  (let* ((base (convo-base-dir :path path :from-anchor from-anchor))
         (base-dir (uiop:ensure-directory-pathname base))
         (convos nil))
    (when (uiop:directory-exists-p base-dir)
      (dolist (sub (uiop:subdirectories base-dir))
        (let ((slim (load-conversation-slim sub)))
          (when slim
            (push slim convos))))
      (let ((self-slim (load-conversation-slim base-dir)))
        (when (and self-slim (not (member (conversation-slim-id self-slim) convos
                                          :key #'conversation-slim-id
                                          :test #'equal)))
          (push self-slim convos))))
    (nreverse convos)))

(defun flatten-args (args)
  (if (and (consp args) (listp (car args)) (null (cdr args)))
      (car args)
      args))

(defun delete-jsonl-convo (id-or-path &key (path nil) (from-anchor nil))
  "Deletes a conversation directory from disk."
  (let ((dir (resolve-convo-dir id-or-path :path path :from-anchor from-anchor)))
    (when (and (uiop:directory-exists-p dir)
	       (uiop:file-exists-p (resolve-metadata-path dir)))
      (uiop:delete-directory-tree dir
				  :validate t
				  :if-does-not-exist :ignore)
      t)))

(defparameter *jsonl-convo-store*
  (make-convo-store
   :name "jsonl"
   :description "Local JSONL file store for conversations"
   :list-fn (lambda (&rest args) (apply #'list-jsonl-convos (flatten-args args)))
   :store-fn (lambda (convo &rest args) (apply #'save-jsonl-convo convo (flatten-args args)))
   :load-fn (lambda (id &rest args) (apply #'load-jsonl-convo id (flatten-args args)))
   :del-fn (lambda (id &rest args) (apply #'delete-jsonl-convo id (flatten-args args)))))


;;;; Generic Wrappers


(defun list-conversations (convo-store &rest args)
  "Lists conversations using CONVO-STORE."
  (funcall (convo-store-list-fn convo-store) args))

(defun save-conversation (convo-store conversation &rest args)
  "Saves CONVERSATION using CONVO-STORE."
  (funcall (convo-store-store-fn convo-store) conversation args))

(defun load-conversation (convo-store id &rest args)
  "Loads conversation with ID using CONVO-STORE."
  (funcall (convo-store-load-fn convo-store) id args))

(defun delete-conversation (convo-store id &rest args)
  "Deletes a conversation with ID using CONVO-STORE."
  (funcall (convo-store-del-fn convo-store) id args))
