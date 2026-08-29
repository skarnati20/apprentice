;;;; model.lisp

(in-package :apprentice)


;;;; Definitions


(defstruct model
  name
  call-fn)

(defstruct tool-call
  id
  name
  args)

(defstruct turn
  role
  text
  thinking
  calls
  results
  stop)


;;;; Model Functions


(defun run-model (model msgs tools &rest options)
  (apply (model-call-fn model) msgs tools options))


;;;; Request Parameters


(defstruct param
  name
  key
  default
  transform)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun param-row-form (row)
  "Expansion-time: one DEFMODEL :PARAMS row to a form building a PARAM."
  (let* ((name (first row))
	 (rest (rest row))
	 (key (if (stringp (first rest))
		   (pop rest)
		   (substitute #\_ #\- (string-downcase (symbol-name name)))))
	 (default (getf rest :default :none))
	 (as (getf rest :as)))
    `(make-param :name ,(intern (symbol-name name) :keyword)
		 :key ,key
		 :default ',default
		 :transform ,(when as
			       `(lambda (value)
				  (declare (ignorable value))
				  ,as))))))

(defun param-pair (param options)
  "The JSON key and value PARAM contributes, or NIL to omit it."
  (let ((value (getf options (param-name param) (param-default param))))
    (unless (eq value :none)
      (list (cons (intern (param-key param) :keyword)
		  (if (param-transform param)
		      (funcall (param-transform param) value)
		      value))))))

(defun check-options (params options)
  (loop for (key nil) on options by #'cddr
	unless (find key params :key #'param-name)
	  do (error "Unknown option ~s. This model accepts: ~{~s~^, ~}"
		    key (mapcar #'param-name params))))

(defun format-messages (msgs formatter)
  "Accumulates (values MESSAGES TOP-LEVEL-FIELDS). A formatter returns a
   list of messages and, optionally, fields for the top level of the
   request body -- which is how a provider lifts the system prompt out
   of the message array."
  (let ((out nil) (top-level-fields nil))
    (dolist (msg msgs (values out top-level-fields))
      (multiple-value-bind (ms fs) (funcall formatter msg)
	(setf out    (append out ms))
	(setf top-level-fields (append top-level-fields fs))))))

(defun build-request-json (params options msgs top-level-fields tools messages-key)
  (lisp-to-json-string
   (append (loop for p in params append (param-pair p options))
	   top-level-fields
	   (list (cons (intern messages-key :keyword) msgs))
	   (when tools (list (cons :|tools| tools))))))

(defun http-post (endpoint headers json)
  (run-argv (append (list "curl" "-s" "--max-time" "120" endpoint)
		    (loop for (name value) in headers
			  when value
			    append (list "-H" (format nil "~a: ~a" name value)))
		    (list "--data-binary" "@-"))
	    :input json
	    :limit most-positive-fixnum))


;;;; Model Macro


(defmacro defmodel (name &key endpoint
			   headers
			   params
			   format-message
			   messages-key
			   format-tool
			   parse)
  "FORMAT-MESSAGE, FORMAT-TOOL and PARSE are bodies, not functions:
   they see MSG, TOOL and RAW. FORMAT-MESSAGE returns a LIST, so one
   record can become none, one, or several messages."
  (let ((var (intern (format nil "*~:@(~a~)-MODEL*" name))))
    `(defparameter ,var
       (make-model
	:name ,(string-downcase (symbol-name name))
	:call-fn
	(let ((params (list ,@(mapcar #'param-row-form params))))
	  (lambda (msgs tools &rest options)
	    (declare (ignorable msgs tools options))
	    (check-options params options)
	    (multiple-value-bind (wire top-level-fields)
		(format-messages msgs (lambda (msg)
					(declare (ignorable msg))
					,format-message))
	      (let* ((json (build-request-json
			    params
			    options
			    wire
			    top-level-fields
			    (mapcar (lambda (tool)
				      (declare (ignorable tool))
				      ,format-tool)
				    tools)
			    (or ,messages-key "messages")))
		     (raw (json:decode-json-from-string
			   (http-post ,endpoint
				      (list ,@(mapcar (lambda (h) `(list ,@h)) headers))
				      json))))
		(declare (ignorable raw))
		,parse))))))))


;;;; OpenAI Wire Format
;;;;
;;;; Shared by llama.cpp, vLLM, LM Studio, OpenRouter and OpenAI.


(defun openai-format-message (turn)
  (case (turn-role turn)
    (:system (list (j "role" "system" "content" (turn-text turn))))
    (:user   (list (j "role" "user"   "content" (turn-text turn))))
    (:tool-results
     (mapcar (lambda (r)
	       (j "role"         "tool"
		  "tool_call_id" (car r)
		  "content"      (cdr r)))
	     (turn-results turn)))
    (:assistant
     (list (append (j "role" "assistant" "content" (turn-text turn))
		   (when (turn-calls turn)
		     (j "tool_calls"
			(mapcar (lambda (c)
				  (j "id"   (tool-call-id c)
				     "type" "function"
				     "function"
				     (j "name" (tool-call-name c)
					"arguments" (lisp-to-json-string
						     (tool-call-args c)))))
				(turn-calls turn)))))))))

(defun openai-parse (raw)
  (let ((err (s raw "error")))
    (if err
	(make-turn :role :assistant :stop :error
		   :text (format nil "API error: ~a" (s err "message")))
	(let* ((choice (first (s raw "choices")))
	       (msg    (s choice "message"))
	       (calls  (s msg "tool_calls")))
	  (make-turn
	   :role :assistant
	   :text (s msg "content")
	   :thinking (s msg "reasoning_content")
	   :calls
	   (mapcar (lambda (c)
		     (make-tool-call :id   (s c "id")
				:name (s c "function" "name")
				:args (handler-case
					  (json:decode-json-from-string
					   (s c "function" "arguments"))
					(error () :malformed))))
		   calls)
	   :stop
	   (cond ((null choice)                               :error)
		 ((equal (s choice "finish_reason") "length") :overflow)
		 (calls                                       :tool-use)
		 (t                                           :end)))))))


;;;; Anthropic Wire Format


(defun tool->anthropic (tool)
  (j "name"         (tool-name tool)
     "description"  (tool-description tool)
     "input_schema" (tool-schema tool)))

(defun anthropic-format-message (turn)
  (case (turn-role turn)
    (:system (values nil (j "system" (turn-text turn))))
    (:user   (list (j "role" "user" "content" (turn-text turn))))
    (:tool-results
     (list (j "role" "user"
	      "content" (mapcar (lambda (r)
				  (j "type"        "tool_result"
				     "tool_use_id" (car r)
				     "content"     (cdr r)))
				(turn-results turn)))))
    (:assistant
     (list (j "role" "assistant"
	      "content"
	      (append (when (turn-text turn)
			(list (j "type" "text" "text" (turn-text turn))))
		      (mapcar (lambda (c)
				(j "type"  "tool_use"
				   "id"    (tool-call-id c)
				   "name"  (tool-call-name c)
				   "input" (tool-call-args c)))
			      (turn-calls turn))))))))

(defun anthropic-parse (raw)
  (let ((err (s raw "error")))
    (if err
	(make-turn :role :assistant :stop :error
		   :text (format nil "API error: ~a" (s err "message")))
	(let* ((blocks (s raw "content"))
	       (stop   (s raw "stop_reason"))
	       (uses   (remove-if-not (lambda (b) (equal (s b "type") "tool_use"))
				      blocks))
	       (texts  (loop for b in blocks
			     when (equal (s b "type") "text")
			       collect (s b "text"))))
	  (make-turn
	   :role :assistant
	   :text (when texts (format nil "~{~a~}" texts))
	   :calls
	   (mapcar (lambda (b)
		     (make-tool-call :id   (s b "id")
				     :name (s b "name")
				     :args (s b "input")))
		   uses)
	   :stop
	   (cond ((null blocks)             :error)
		 ((equal stop "max_tokens") :overflow)
		 ((equal stop "refusal")    :refusal)
		 (uses                      :tool-use)
		 (t                         :end)))))))


;;;; OpenAI Responses Wire Format


(defun tool->openai-responses (tool)
  (j "type"        "function"
     "name"        (tool-name tool)
     "description" (tool-description tool)
     "parameters"  (tool-schema tool)))

(defun openai-responses-format-message (turn)
  (case (turn-role turn)
    (:system (values nil (j "instructions" (turn-text turn))))
    (:user   (list (j "role" "user"
		      "content" (list (j "type" "input_text"
					 "text" (turn-text turn))))))
    (:tool-results
     (mapcar (lambda (r)
	       (j "type"    "function_call_output"
		  "call_id" (car r)
		  "output"  (cdr r)))
	     (turn-results turn)))
    (:assistant
     (append (when (turn-text turn)
	       (list (j "type" "message" "role" "assistant"
			"content" (list (j "type" "output_text"
					   "text" (turn-text turn))))))
	     (mapcar (lambda (c)
		       (j "type"      "function_call"
			  "call_id"   (tool-call-id c)
			  "name"      (tool-call-name c)
			  "arguments" (lisp-to-json-string (tool-call-args c))))
		     (turn-calls turn))))))

(defun openai-responses-parse (raw)
  (let ((err (s raw "error")))
    (if err
	(make-turn :role :assistant :stop :error
		   :text (format nil "API error: ~a" (s err "message")))
	(let* ((items (s raw "output"))
	       (status (s raw "status"))
	       (reason (s raw "incomplete_details" "reason"))
	       (calls (remove-if-not
		       (lambda (i) (equal (s i "type") "function_call"))
		       items))
	       (texts (loop for i in items
			    when (equal (s i "type") "message")
			      append (loop for c in (s i "content")
					   when (equal (s c "type") "output_text")
					     collect (s c "text")))))
	  (make-turn
	   :role :assistant
	   :text (when texts (format nil "~{~a~}" texts))
	   :calls
	   (mapcar (lambda (i)
		     (make-tool-call
		      :id   (s i "call_id")
		      :name (s i "name")
		      :args (handler-case
				(json:decode-json-from-string (s i "arguments"))
			      (error () :malformed))))
		   calls)
	   :stop
	   (cond ((null items)                      :error)
		 ((equal reason "max_output_tokens") :overflow)
		 ((equal status "incomplete")        :overflow)
		 (calls                              :tool-use)
		 (t                                  :end)))))))


;;;; Available Models

(defmodel llama-cpp
  :endpoint "http://localhost:8080/v1/chat/completions"
  :headers (("Content-Type" "application/json"))
  :params ((model       :default "qwen")
	   (max-tokens  :default 4096)
	   (temperature :default 0.2)
	   (top-p       :default 0.95)
	   (seed)
	   (stop)
	   (stream      :default nil :as (if value t :false))
	   (thinking    "chat_template_kwargs"
			:as (j "enable_thinking" (if value t :false))))
  :format-message (openai-format-message msg)
  :format-tool    (tool->openai tool)
  :parse          (openai-parse raw))


(defmodel claude-sonnet-5
  :endpoint "https://api.anthropic.com/v1/messages"
  :headers (("Content-Type"      "application/json")
	    ("anthropic-version" "2023-06-01")
	    ("x-api-key"         (uiop:getenv "ANTHROPIC_API_KEY"))
	    ("anthropic-workspace-id" (uiop:getenv "ANTHROPIC_WORKSPACE_ID")))
  :params ((model      :default "claude-sonnet-5")
	   (max-tokens :default 16000)
	   (stop       "stop_sequences")
	   (stream     :default nil :as (if value t :false))
	   (thinking   :as (j "type" (if value "adaptive" "disabled")))
	   (effort     "output_config" :as (j "effort" value)))
  :format-message (anthropic-format-message msg)
  :format-tool    (tool->anthropic tool)
  :parse          (anthropic-parse raw))


(defmodel gpt-5.6-terra
  :endpoint "https://api.openai.com/v1/responses"
  :headers (("Content-Type"  "application/json")
	    ("Authorization" (format nil "Bearer ~a" (uiop:getenv "OPENAI_API_KEY"))))
  :messages-key "input"
  :params ((model      :default "gpt-5.6-terra")
	   (max-tokens "max_output_tokens" :default 16000)
	   (stream     :default nil :as (if value t :false))
	   (effort     "reasoning" :default "medium" :as (j "effort" value)))
  :format-message (openai-responses-format-message msg)
  :format-tool    (tool->openai-responses tool)
  :parse          (openai-responses-parse raw))
