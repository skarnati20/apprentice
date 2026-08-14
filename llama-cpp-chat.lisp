;;;; llama-cpp-chat.lisp

(in-package :apprentice)


(defvar *llama-cpp-endpoint* "http://localhost:8080/v1/chat/completions")
(defvar *model-name* "qwen")

(defun role-to-string (role)
  "Converts ROLE to its string representation."
  (case role
    (:system "system")
    (:user "user")
    (:assistant "assistant")
    (:tool "tool")
    (t (error "Unsupported role: ~s" role))))

(defun format-message (content role)
  "Formats CONTENT and ROLE to proper llama.cpp messsage."
  (list (cons :|role| (role-to-string role))
        (cons :|content| content)))

(defun format-tool (tool)
  "Formats TOOL for llama.cpp request."
  (tool->openai tool))

(defun format-tools (tools)
  "Formats TOOLS for llama.cpp request."
  (mapcar #'format-tool tools))

(defun format-option (key value)
  "Formats KEY and VALUE from options in llama.cpp request."
  (cond
    ((eq key :max-tokens) (list (cons :|max-tokens| value)))
    (t nil)))

(defun format-options (options)
  "Formats remaining OPTIONS in llama.cpp request."
  (loop for (key value) on options by #'cddr
	append
	(format-option key value)))
  
(defun llama-cpp-request (messages &rest options &key (tools nil) &allow-other-keys)
  "JSON request body for a chat completion for MESSAGES."
  (let* ((data (append (list (cons :|model| *model-name*))
		       (list (cons :|messages| messages))
		       (when tools
			 (list (cons :|tools| (format-tools tools))))
		       (format-options options)))
	 (json-data (lisp-to-corrected-json-string data)))
    json-data))

(defun run-llama-cpp-chat-json (json-data)
  "Send JSON-DATE to the ollama chat endpoint and return the response,
   or NIL on error."
  (run-argv (list "curl" "-s" *llama-cpp-endpoint* "--data-binary" "@-")
            :input json-data
            :limit most-positive-fixnum))
			       
(defun run-llama-cpp-chat-raw (msgs &rest options &key (tools nil) &allow-other-keys)
  (json:decode-json-from-string
   (run-llama-cpp-chat-json (apply #'llama-cpp-request msgs :tools tools options))))

(defun run-llama-cpp-chat-one-shot (prompt &rest options &key (tools nil) &allow-other-keys)
  (let ((msg (format-message prompt :user)))
    (apply #'run-llama-cpp-chat-raw (list msg) :tools tools options)))
