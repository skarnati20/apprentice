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

(defun build-request-json (params options msgs fields tools)
  (lisp-to-json-string
   (append (loop for p in params append (param-pair p options))
	   fields
	   (list (cons :|messages| msgs))
	   (when tools (list (cons :|tools| tools))))))

(defun http-post (endpoint headers json)
  (run-argv (append (list "curl" "-s" "--max-time" "120" endpoint)
		    (loop for (name value) in headers
			  append (list "-H" (format nil "~a: ~a" name value)))
		    (list "--data-binary" "@-"))
	    :input json
	    :limit most-positive-fixnum))


;;;; Model Macro


(defmacro defmodel (name &key endpoint
			   headers
			   params
			   format-message
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
				    tools)))
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


;;;; Models List


(defvar *models-list*
  '(*llama-cpp-model*))

(defparameter *model* *llama-cpp-model*
  "Default model for the agent loops.")
