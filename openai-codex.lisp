;;;; openai-codex.lisp
;;;;
;;;; ChatGPT/Codex inference over device credentials (Designs A/E).
;;;;
;;;; A context-bound model named "openai-codex" sends completed-turn
;;;; HTTP/SSE requests to the verified Codex origin with the caller's
;;;; explicit model slug. Secrets travel only through curl standard input
;;;; and a 0600 curl configuration file; they never appear in argv, the
;;;; environment, turn history, or conditions. Completed message,
;;;; function_call and encrypted reasoning items are preserved in
;;;; turn-raw and replayed before matching tool outputs, so the existing
;;;; standard loop works unchanged. HTTP 401 yields re-login guidance
;;;; without touching credentials; 403 and other failures stay distinct.

(in-package #:apprentice)


;;;; Verified contract constants

(defparameter +openai-codex-url+
  "https://chatgpt.com/backend-api/codex/responses")

(defparameter +openai-codex-user-agent+ "apprentice/0.1.0")

(defparameter +openai-codex-originator+ "apprentice")

(defparameter +openai-codex-default-effort+ "medium")

(defconstant +openai-codex-sse-max-bytes+ 1048576)

(defconstant +openai-codex-sse-max-events+ 4096)


;;;; Safe conditions

(define-condition codex-http-error (auth-status-error) ()
  (:report (lambda (condition stream)
             (format stream "OpenAI Codex request failed (HTTP ~d); [REDACTED]"
                     (auth-error-status condition)))))

(define-condition codex-sse-error (openai-auth-error)
  ((category :initarg :category :reader auth-error-category))
  (:report (lambda (condition stream)
             (format stream "OpenAI Codex response failed (~(~a~)); [REDACTED]"
                     (auth-error-category condition)))))

(define-condition codex-transport-failure (openai-auth-error)
  ((category :initarg :category :reader auth-error-category))
  (:report (lambda (condition stream)
             (format stream "OpenAI Codex transport failed (~(~a~)); [REDACTED]"
                     (auth-error-category condition)))))

(defmethod print-object ((condition codex-sse-error) stream)
  (print-unreadable-object (condition stream :type t)
    (format stream "~(~a~) [REDACTED]"
            (auth-error-category condition))))

(defmethod print-object ((condition codex-transport-failure) stream)
  (print-unreadable-object (condition stream :type t)
    (format stream "~(~a~) [REDACTED]"
            (auth-error-category condition))))


;;;; Unpadded base64url (JWT payloads; no new dependency)

(defun base64url-value (character)
  (let ((code (char-code character)))
    (cond ((<= (char-code #\A) code (char-code #\Z))
           (- code (char-code #\A)))
          ((<= (char-code #\a) code (char-code #\z))
           (+ (- code (char-code #\a)) 26))
          ((<= (char-code #\0) code (char-code #\9))
           (+ (- code (char-code #\0)) 52))
          ((char= character #\-) 62)
          ((char= character #\_) 63)
          (t nil))))

(defun base64url-decode (string)
  "Decode unpadded base64url STRING to an octet vector, or NIL when the
input is empty, misaligned, or outside the URL-safe alphabet."
  (when (and (stringp string) (plusp (length string)))
    (let ((values (mapcar #'base64url-value (coerce string 'list))))
      (when (and (not (member nil values))
                 (/= (mod (length values) 4) 1))
        (let ((bits 0)
              (count 0)
              (octets nil))
          (dolist (value values)
            (setf bits (+ (ash bits 6) value))
            (incf count)
            (when (= count 4)
              (push (ldb (byte 8 16) bits) octets)
              (push (ldb (byte 8 8) bits) octets)
              (push (ldb (byte 8 0) bits) octets)
              (setf bits 0 count 0)))
          (cond ((= count 2)
                 (push (ash bits -4) octets))
                ((= count 3)
                 (push (ldb (byte 8 8) (ash bits -2)) octets)
                 (push (ldb (byte 8 0) (ash bits -2)) octets)))
          (coerce (nreverse octets) '(vector (unsigned-byte 8))))))))

(defun base64url-to-string (string)
  "Decode unpadded base64url STRING to a string, or NIL when invalid."
  (let ((octets (base64url-decode string)))
    (when octets
      (map 'string #'code-char octets))))


;;;; Unverified routing metadata from the ID token

(defun jwt-payload-alist (token)
  "The decoded JSON payload of TOKEN as an alist, or NIL unless TOKEN
is a well-formed three-part JWT carrying a JSON object payload."
  (when (stringp token)
    (let ((first-dot (position #\. token))
          (last-dot (position #\. token :from-end t)))
      (when (and first-dot last-dot (/= first-dot last-dot))
        (let ((payload (base64url-to-string
                        (subseq token (1+ first-dot) last-dot))))
          (when payload
            (let ((data (ignore-errors
                          (json:decode-json-from-string payload))))
              (when (and (consp data) (consp (first data)))
                data))))))))

(defun decode-codex-routing-headers (id-token)
  "Routing headers derived from ID-TOKEN's nested auth claims, or NIL.
Decoded values are unverified routing metadata only: a missing or
malformed claim is never an error and the request is sent without it."
  (let ((payload (ignore-errors (jwt-payload-alist id-token))))
    (when payload
      (let ((account (ignore-errors
                       (s payload "https://api.openai.com/auth.chatgpt_account_id")))
            (fedramp (ignore-errors
                       (s payload "chatgpt_account_is_fedramp"))))
        (append (when (and (stringp account) (plusp (length account)))
                  (list (cons "ChatGPT-Account-ID" account)))
                (when (eq fedramp t)
                  (list (cons "X-OpenAI-Fedramp" "true"))))))))

(defun codex-request-headers (tokens)
  "The full header alist as (NAME . VALUE) string pairs: honest client
identity plus bearer access token and conditional routing metadata."
  (append (list (cons "Content-Type" "application/json")
                (cons "Accept" "text/event-stream")
                (cons "User-Agent" +openai-codex-user-agent+)
                (cons "originator" +openai-codex-originator+))
          (list (cons "Authorization"
                      (concatenate 'string "Bearer "
                                   (secret-value
                                    (oauth-tokens-access-token tokens)))))
          (decode-codex-routing-headers
           (secret-value (oauth-tokens-id-token tokens)))))


;;;; Codex-specific request formatting with raw-item replay

(defun codex-format-tool (tool)
  (j "type" "function"
     "name" (tool-name tool)
     "description" (tool-description tool)
     "strict" :false
     "parameters" (tool-schema tool)))

(defun codex-format-input (turns)
  "Format TURNS for the Codex Responses input. Returns the input item list and top-level instructions (or NIL when no
system text exists). A previous Codex assistant turn replays its raw
completed items verbatim; foreign assistant turns rebuild message and
function_call items without IDs or reasoning content."
  (let ((input nil)
        (systems nil))
    (dolist (turn turns)
      (ecase (turn-role turn)
        (:system
         (when (and (turn-text turn) (plusp (length (turn-text turn))))
           (push (turn-text turn) systems)))
        (:user
         (push (j "type" "message" "role" "user"
                  "content" (list (j "type" "input_text"
                                     "text" (turn-text turn))))
               input))
        (:assistant
         (if (turn-raw turn)
             (dolist (item (turn-raw turn))
               (push item input))
             (setf input
                   (append (when (turn-text turn)
                             (list (j "type" "message" "role" "assistant"
                                      "content" (list (j "type" "output_text"
                                                         "text" (turn-text turn))))))
                           (mapcar
                            (lambda (call)
                              (j "type" "function_call"
                                 "call_id" (tool-call-id call)
                                 "name" (tool-call-name call)
                                 "arguments" (lisp-to-json-string
                                              (args-object
                                               (tool-call-args call)))))
                            (turn-calls turn))
                           input))))
        (:tool-results
         (dolist (result (turn-results turn))
           (let ((output (third result)))
             (push (j "type" "function_call_output"
                      "call_id" (first result)
                      "output" (if (stringp output)
                                   output
                                   (format nil "~a" output)))
                   input))))))
    (values (nreverse input)
            (when systems
              (format nil "~{~a~^~%~%~}" (nreverse systems))))))

(defun codex-request-alist (model-id instructions input tools effort parallel)
  "The pinned minimum Codex request document as an alist. INSTRUCTIONS
and TOOLS are omitted when empty; there is no default model slug."
  (unless (and (stringp model-id) (plusp (length model-id)))
    (error 'invalid-auth-config :category :codex-model-id))
  (unless (and (stringp effort) (plusp (length effort)))
    (error 'invalid-auth-config :category :codex-options))
  (append (j "model" model-id)
          (when (and (stringp instructions) (plusp (length instructions)))
            (j "instructions" instructions))
          (list (cons :input (or input #())))
          (when tools
            (list (cons :|tools| tools)))
          (j "tool_choice" "auto"
             "parallel_tool_calls" (if parallel t :false)
             "reasoning" (j "effort" effort)
             "store" :false
             "stream" t
             "include" (list "reasoning.encrypted_content"))))


;;;; Bounded SSE parsing until response.completed

(defun split-sse-frames (text)
  "TEXT split into frames of raw lines on blank-line boundaries."
  (let ((frames nil)
        (current nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let ((stripped (string-right-trim '(#\Return) line)))
        (if (string= stripped "")
            (progn (when current
                     (push (nreverse current) frames)
                     (setf current nil)))
            (push stripped current))))
    (when current
      (push (nreverse current) frames))
    (nreverse frames)))

(defun parse-sse-frame (lines)
  "LINES of one frame as (EVENT DATA-LINES) with one optional space
trimmed after each colon."
  (let ((event nil)
        (data nil))
    (dolist (line lines)
      (cond ((and (> (length line) 6)
                  (string= "event:" line :end2 6))
             (setf event (string-left-trim " " (subseq line 6))))
            ((and (> (length line) 5)
                  (string= "data:" line :end2 5))
             (push (string-left-trim " " (subseq line 5)) data))))
    (values event (nreverse data))))

(defun validate-codex-item (item)
  "ITEM as a replayable completed output item, NIL for ignorable
unknown types; malformed required items signal CODEX-SSE-ERROR."
  (unless (consp item)
    (error 'codex-sse-error :category :malformed-item))
  (let ((type (s item "type")))
    (cond ((equal type "message")
           (unless (listp (s item "content"))
             (error 'codex-sse-error :category :malformed-item))
           item)
          ((equal type "function_call")
           (unless (and (stringp (s item "call_id"))
                        (stringp (s item "name"))
                        (stringp (s item "arguments")))
             (error 'codex-sse-error :category :malformed-item))
           item)
          ((equal type "reasoning")
           item)
          (t nil))))

(defun parse-codex-sse (text)
  "TEXT parsed to validated completed output items. Requires the
response.completed terminal; deltas and unknown events are ignored
within the same bounds. Every failure mode is a CODEX-SSE-ERROR."
  (unless (stringp text)
    (error 'codex-sse-error :category :malformed))
  (when (> (length text) +openai-codex-sse-max-bytes+)
    (error 'codex-sse-error :category :oversize))
  (let ((items nil)
        (completed nil)
        (events 0))
    (dolist (frame (split-sse-frames text))
      (incf events)
      (when (> events +openai-codex-sse-max-events+)
        (error 'codex-sse-error :category :oversize))
      (multiple-value-bind (event data) (parse-sse-frame frame)
        (cond ((null event)
               nil)
              ((string= event "response.output_item.done")
               (let ((payload (handler-case
                                  (json:decode-json-from-string
                                   (format nil "~{~a~^~%~%~}" data))
                                (error ()
                                  (error 'codex-sse-error
                                         :category :malformed)))))
                 (let ((item (validate-codex-item (s payload "item"))))
                   (when item
                     (push item items)))))
              ((string= event "response.completed")
               (setf completed t))
              ((string= event "response.failed")
               (error 'codex-sse-error :category :failed))
              ((string= event "response.incomplete")
               (error 'codex-sse-error :category :incomplete))
              (t nil))))
    (unless completed
      (error 'codex-sse-error :category :incomplete-response))
    (nreverse items)))

(defun codex-items-turn (items)
  "ITEMS as an Apprentice assistant turn: text from completed message
items, calls from function_call items, raw items preserved for replay."
  (if (null items)
      (make-turn :role :assistant :stop :error)
      (let ((texts nil)
            (calls nil))
        (dolist (item items)
          (let ((type (s item "type")))
            (cond ((equal type "message")
                   (dolist (content (s item "content"))
                     (when (equal (s content "type") "output_text")
                       (push (s content "text") texts))))
                  ((equal type "function_call")
                   (push (make-tool-call
                          :id (s item "call_id")
                          :name (s item "name")
                          :args (handler-case
                                    (json:decode-json-from-string
                                     (s item "arguments"))
                                  (error () :malformed)))
                         calls))
                  (t nil))))
        (make-turn :role :assistant
                   :text (when texts
                           (format nil "~{~a~}" (nreverse texts)))
                   :calls (nreverse calls)
                   :raw items
                   :stop (cond (calls :tool-use)
                               (t :end))))))

(defun codex-response-turn (text)
  "SSE TEXT as the completed assistant turn."
  (codex-items-turn (parse-codex-sse text)))


;;;; Dedicated supervised SSE transport

(defun codex-config-escape (string)
  (with-output-to-string (out)
    (loop for character across string
          do (when (or (char= character #\")
                       (char= character #\\))
               (write-char #\\ out))
             (write-char character out))))

(defun make-private-codex-config-file (directory headers)
  "A 0600 curl -K file carrying secret HEADERS as header lines."
  #+sbcl
  (handler-case
      (let ((path
              (string-right-trim
               '(#\Newline #\Return)
               (uiop:run-program
                (list "mktemp"
                      (namestring (merge-pathnames "config.XXXXXXXX" directory)))
                :output :string :error-output nil))))
        (unless (plusp (length path))
          (error 'codex-transport-failure :category :temporary-config))
        (sb-posix:chmod path #o600)
        (with-open-file (stream path :direction :output
                                     :if-exists :supersede
                                     :external-format :utf-8)
          (dolist (header headers)
            (format stream "header = \"~a: ~a\"~%"
                    (car header) (codex-config-escape (cdr header)))))
        (finish-output)
        path)
    (codex-transport-failure (condition) (error condition))
    (error ()
      (error 'codex-transport-failure :category :temporary-config)))
  #-sbcl
  (error 'codex-transport-failure :category :unsupported-platform))

(defun codex-public-header-p (header)
  (member (car header) '("Content-Type" "Accept" "User-Agent" "originator")
          :test #'string=))

(defun codex-curl-command (curl-program config-path header-path timeout
                           url public-headers)
  (append (list curl-program
                "--disable"
                "--silent"
                "--show-error"
                "--request" "POST"
                "--config" config-path)
          (loop for header in public-headers
                append (list "--header"
                             (format nil "~a: ~a" (car header) (cdr header))))
          (list "--data-binary" "@-"
                "--output" "-"
                "--dump-header" header-path
                "--max-time" (format nil "~,3f" (coerce timeout 'double-float))
                "--connect-timeout" (format nil "~,3f" (coerce timeout 'double-float))
                "--max-redirs" "0"
                "--proto" "=https"
                "--proto-redir" "=https"
                "--tlsv1.2"
                "--noproxy" "*"
                url)))

(defun %supervised-codex-request (curl-program url headers body timeout
                                  max-response-bytes)
  (unless (and (stringp curl-program) (plusp (length curl-program))
               (stringp url) (plusp (length url))
               (stringp body)
               (positive-auth-timeout-p timeout)
               (and (integerp max-response-bytes) (plusp max-response-bytes)))
    (error 'invalid-auth-config :category :transport-options))
  (let ((directory nil)
        (header-path nil)
        (config-path nil)
        (process nil)
        (reader nil))
    (unwind-protect
         (progn
           (setf directory (make-private-auth-directory)
                 header-path (make-private-auth-header-file directory)
                 config-path (make-private-codex-config-file
                              directory
                              (remove-if #'codex-public-header-p headers)))
           (setf process
                 (uiop:launch-program
                  (codex-curl-command curl-program config-path header-path
                                      timeout url
                                      (remove-if-not #'codex-public-header-p
                                                     headers))
                  :input :stream :output :stream :error-output nil
                  :external-format :utf-8))
           (let ((input (uiop:process-info-input process))
                 (output (uiop:process-info-output process))
                 (state (list :done nil :overflow nil :reader-failed nil :body ""))
                 (lock (bordeaux-threads:make-lock "codex-curl-reader"))
                 (ready (bordeaux-threads:make-condition-variable))
                 (started-at (seconds-now)))
             (unwind-protect
                  (progn
                    ;; Secrets travel only through this pipe and the 0600
                    ;; curl configuration file, never argv or environment.
                    (write-string body input)
                    (finish-output input)
                    (close input)
                    (setf reader
                          (bordeaux-threads:make-thread
                           (lambda ()
                             (collect-bounded-auth-output
                              output max-response-bytes state lock ready))
                           :name "apprentice-codex-curl-reader"))
                    (loop
                      (when (getf state :overflow)
                        (stop-and-reap-auth-process process reader)
                        (error 'codex-transport-failure
                               :category :response-too-large))
                      (when (getf state :reader-failed)
                        (stop-and-reap-auth-process process reader)
                        (error 'codex-transport-failure
                               :category :response-read-failed))
                      (when (not (uiop:process-alive-p process))
                        (return))
                      (when (>= (- (seconds-now) started-at) timeout)
                        (stop-and-reap-auth-process process reader)
                        (error 'codex-transport-failure
                               :category :request-timed-out))
                      (bordeaux-threads:with-lock-held (lock)
                        (unless (getf state :done)
                          (bordeaux-threads:condition-wait ready lock :timeout 0.02))))
                    (let ((exit-code (uiop:wait-process process)))
                      (when reader
                        (bordeaux-threads:join-thread reader)
                        (setf reader nil))
                      (when (getf state :overflow)
                        (error 'codex-transport-failure
                               :category :response-too-large))
                      (when (getf state :reader-failed)
                        (error 'codex-transport-failure
                               :category :response-read-failed))
                      (unless (zerop exit-code)
                        (error 'codex-transport-failure :category :curl-failed))
                      (make-auth-http-response
                       :status (auth-header-status header-path)
                       :body (getf state :body))))
               (when process
                 (stop-and-reap-auth-process process reader)
                 (setf reader nil))
               (when reader
                 (ignore-errors (bordeaux-threads:join-thread reader))))))
      (delete-private-auth-directory directory))))

(defun supervised-codex-request (curl-program url headers body timeout
                                 max-response-bytes)
  "POST BODY to the Codex URL with HEADERS and return the bounded
AUTH-HTTP-RESPONSE. Secret headers travel in a 0600 curl configuration
file; status framing stays separate from the body. Unexpected
subprocess failures map to safe CODEX-TRANSPORT-FAILURE."
  (handler-case
      (%supervised-codex-request curl-program url headers body
                                 timeout max-response-bytes)
    (codex-transport-failure (condition)
      (error condition))
    (auth-http-failure (condition)
      (error 'codex-transport-failure
             :category (auth-error-category condition)))
    (openai-auth-error (condition)
      (error condition))
    (error ()
      (error 'codex-transport-failure :category :subprocess-failed))))

(defun default-codex-request (url headers body)
  "The concrete inference request honoring model-request timeouts."
  (supervised-codex-request "curl" url headers body *http-timeout*
                            +openai-codex-sse-max-bytes+))


;;;; Context-bound model registration and invocation

(defun codex-call-params ()
  (list (make-param :name :effort :key "effort"
                    :default +openai-codex-default-effort+ :transform nil)
        (make-param :name :parallel-tool-calls :key "parallel_tool_calls"
                    :default nil :transform nil)))

(defun strip-codex-seam-options (options)
  (loop for (key value) on options by #'cddr
        unless (eq key :request-fn)
        append (list key value)))

(defun make-openai-codex-call (context model-id &key request-fn)
  "A model call function bound to CONTEXT and the explicit MODEL-ID.
REQUEST-FN is an injectable (URL HEADERS BODY) request used by tests;
production calls use the supervised curl transport. Missing or
known-expired credentials raise re-login guidance before any request;
HTTP 401 does the same without touching stored credentials, while 403
and other statuses stay distinct inference failures."
  (unless (openai-auth-context-p context)
    (error 'invalid-auth-config :category :auth-context))
  (unless (and (stringp model-id) (plusp (length model-id)))
    (error 'invalid-auth-config :category :codex-model-id))
  (let ((real-request (or request-fn #'default-codex-request)))
    (lambda (msgs tools &rest options)
      (declare (ignorable msgs tools options))
      (let ((per-call-request (getf options :request-fn)))
        (check-options (codex-call-params) (strip-codex-seam-options options))
        (let* ((store (openai-auth-context-store context))
               (config (openai-auth-context-config context))
               (profile (openai-auth-context-profile context))
               (tokens (load-openai-credentials store config profile))
               (now (get-universal-time)))
          (unless tokens
            (error 'reauthentication-required))
          (let ((expires (oauth-tokens-expires-at tokens)))
            (when (and expires (<= expires now))
              (error 'reauthentication-required)))
          (multiple-value-bind (input instructions)
              (codex-format-input msgs)
            (let* ((effort (getf options :effort +openai-codex-default-effort+))
                   (parallel (getf options :parallel-tool-calls nil))
                   (body (lisp-to-json-string
                          (codex-request-alist
                           model-id instructions input
                           (mapcar #'codex-format-tool tools)
                           effort parallel)))
                   (response (funcall (or per-call-request real-request)
                                      +openai-codex-url+
                                      (codex-request-headers tokens)
                                      body))
                   (status (auth-http-response-status response)))
              (declare (ignorable response status))
              (cond ((successful-http-status-p status)
                     (codex-response-turn
                      (auth-http-response-body response)))
                    ((= status 401)
                     (error 'reauthentication-required))
                    (t
                     (error 'codex-http-error :status status))))))))))

(defun configure-openai-device-model (context model-id &key request-fn)
  "Register (or replace) the model named \"openai-codex\" bound to
CONTEXT and the explicit nonempty MODEL-ID slug, and return the model
name. There is no default slug: obtain a current one from the
authenticated Codex catalog or model picker. The selected model,
gpt-5.6-terra and the subagent model are left untouched; select the new
model explicitly with (set-model \"openai-codex\"). REQUEST-FN is a
testing seam for the inference request."
  (unless (openai-auth-context-p context)
    (error 'invalid-auth-config :category :auth-context))
  (unless (and (stringp model-id) (plusp (length model-id)))
    (error 'invalid-auth-config :category :codex-model-id))
  (let ((model (make-model :name "openai-codex"
                           :call-fn (make-openai-codex-call
                                     context model-id
                                     :request-fn request-fn))))
    (setf *models-list*
          (append (remove "openai-codex" *models-list*
                          :key #'model-name :test #'string=)
                  (list model)))
    "openai-codex"))
