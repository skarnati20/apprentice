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


;;;; Model Functions


(defun run-model (model msgs tools &rest options)
  "MSGS holds neutral records -- (:system text), (:user text),
   (:tool-results ((id . output) ...)) -- and raw assistant messages,
   which pass back unchanged. Returns (values CONTENT CALLS STOP MSG);
   STOP is :END :TOOL-USE :OVERFLOW :REFUSAL or :ERROR."
  (apply (model-call-fn model) msgs tools options))

;;;; Request Parameters


(defstruct param
  name
  wire
  default    ; :NONE means omit the key unless the caller supplies it
  transform)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun param-row-form (row)
  "Expansion-time: one DEFMODEL :PARAMS row to a form building a PARAM."
  (let* ((name (first row))
	 (rest (rest row))
	 (wire (if (stringp (first rest))
		   (pop rest)
		   (substitute #\_ #\- (string-downcase (symbol-name name)))))
	 (default (getf rest :default :none))
	 (as (getf rest :as)))
    `(make-param :name ,(intern (symbol-name name) :keyword)
		 :wire ,wire
		 :default ',default
		 :transform ,(when as
			       `(lambda (value)
				  (declare (ignorable value))
				  ,as))))))

(defun param-pair (param options)
  "The wire key and value PARAM contributes, or NIL to omit it."
  (let ((value (getf options (param-name param) (param-default param))))
    (unless (eq value :none)
      (list (cons (intern (param-wire param) :keyword)
		  (if (param-transform param)
		      (funcall (param-transform param) value)
		      value))))))

(defun check-options (params options)
  (loop for (key nil) on options by #'cddr
	unless (find key params :key #'param-name)
	  do (error "Unknown option ~s. This model accepts: ~{~s~^, ~}"
		    key (mapcar #'param-name params))))

(defun format-messages (msgs formatter)
  "Accumulates (values MESSAGES FIELDS). A formatter returns a list of
   messages and, optionally, top-level request fields -- which is how a
   provider lifts the system prompt out of the message array."
  (let ((wire nil) (fields nil))
    (dolist (msg msgs (values wire fields))
      (multiple-value-bind (ms fs) (funcall formatter msg)
	(setf wire   (append wire ms))
	(setf fields (append fields fs))))))

(defun build-request-json (params options msgs fields tools messages-key)
  (lisp-to-json-string
   (append (loop for p in params append (param-pair p options))
	   fields
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
	    (multiple-value-bind (wire fields)
		(format-messages msgs (lambda (msg)
					(declare (ignorable msg))
					,format-message))
	      (let* ((json (build-request-json
			    params
			    options
			    wire
			    fields
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


(defun openai-format-message (msg)
  (case (and (consp msg) (keywordp (first msg)) (first msg))
    (:system (list (j "role" "system" "content" (second msg))))
    (:user   (list (j "role" "user"   "content" (second msg))))
    (:tool-results
     (mapcar (lambda (r)
	       (j "role"         "tool"
		  "tool_call_id" (car r)
		  "content"      (cdr r)))
	     (second msg)))
    (t (list msg))))

(defun openai-parse (raw)
  (let ((err (s raw "error")))
    (if err
	(values (format nil "API error: ~a" (s err "message")) nil :error nil)
	(let* ((choice (first (s raw "choices")))
	       (msg    (s choice "message"))
	       (calls  (s msg "tool_calls")))
	  (values
	   (s msg "content")
	   (mapcar (lambda (c)
		     (make-tool-call :id   (s c "id")
				:name (s c "function" "name")
				:args (handler-case
					  (json:decode-json-from-string
					   (s c "function" "arguments"))
					(error () :malformed))))
		   calls)
	   (cond ((null choice)                               :error)
		 ((equal (s choice "finish_reason") "length") :overflow)
		 (calls                                       :tool-use)
		 (t                                           :end))
	   msg)))))


;;;; Anthropic Wire Format


(defun tool->anthropic (tool)
  (j "name"         (tool-name tool)
     "description"  (tool-description tool)
     "input_schema" (tool-schema tool)))

(defun anthropic-format-message (msg)
  (case (and (consp msg) (keywordp (first msg)) (first msg))
    (:system (values nil (j "system" (second msg))))
    (:user   (list (j "role" "user" "content" (second msg))))
    (:tool-results
     (list (j "role" "user"
	      "content" (mapcar (lambda (r)
				  (j "type"        "tool_result"
				     "tool_use_id" (car r)
				     "content"     (cdr r)))
				(second msg)))))
    (t (list msg))))

(defun anthropic-parse (raw)
  (let ((err (s raw "error")))
    (if err
	(values (format nil "API error: ~a" (s err "message")) nil :error nil)
	(let* ((blocks (s raw "content"))
	       (stop   (s raw "stop_reason"))
	       (uses   (remove-if-not (lambda (b) (equal (s b "type") "tool_use"))
				      blocks))
	       (texts  (loop for b in blocks
			     when (equal (s b "type") "text")
			       collect (s b "text"))))
	  (values
	   (when texts (format nil "~{~a~}" texts))
	   (mapcar (lambda (b)
		     (make-tool-call :id   (s b "id")
				     :name (s b "name")
				     :args (s b "input")))
		   uses)
	   (cond ((null blocks)             :error)
		 ((equal stop "max_tokens") :overflow)
		 ((equal stop "refusal")    :refusal)
		 (uses                      :tool-use)
		 (t                         :end))
	   (j "role" "assistant" "content" blocks))))))


;;;; OpenAI Responses Wire Format


(defparameter *openai-responses-array-fields* '("summary" "content")
  "Fields the Responses API requires to be arrays. CL-JSON decodes an
   empty JSON array to NIL and re-encodes NIL as null, so they have to
   be restored when items are echoed back.")

(defun fix-openai-responses-item (item)
  (if (alist-p item)
      (loop for (k . v) in item
	    collect (cons k (if (and (null v)
				     (member (symbol-name k)
					     *openai-responses-array-fields*
					     :test #'string-equal))
				#()
				v)))
      item))

(defun tool->openai-responses (tool)
  (j "type"        "function"
     "name"        (tool-name tool)
     "description" (tool-description tool)
     "parameters"  (tool-schema tool)))

(defun openai-responses-format-message (msg)
  (case (and (consp msg) (keywordp (first msg)) (first msg))
    (:system (values nil (j "instructions" (second msg))))
    (:user   (list (j "role" "user"
		      "content" (list (j "type" "input_text"
					 "text" (second msg))))))
    (:tool-results
     (mapcar (lambda (r)
	       (j "type"    "function_call_output"
		  "call_id" (car r)
		  "output"  (cdr r)))
	     (second msg)))
    (:items (mapcar #'fix-openai-responses-item (second msg)))
    (t (list msg))))

(defun openai-responses-parse (raw)
  (let ((err (s raw "error")))
    (if err
	(values (format nil "API error: ~a" (s err "message")) nil :error nil)
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
	  (values
	   (when texts (format nil "~{~a~}" texts))
	   (mapcar (lambda (i)
		     (make-tool-call
		      :id   (s i "call_id")
		      :name (s i "name")
		      :args (handler-case
				(json:decode-json-from-string (s i "arguments"))
			      (error () :malformed))))
		   calls)
	   (cond ((null items)                      :error)
		 ((equal reason "max_output_tokens") :overflow)
		 ((equal status "incomplete")        :overflow)
		 (calls                              :tool-use)
		 (t                                  :end))
	   (list :items items))))))


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
