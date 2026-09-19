;;;; tests/openai-codex.lisp

(in-package #:apprentice-tests)

;;;; SSE fixtures shaped by the pinned Codex contract

(defun codex-sse-frame (event data)
  (format nil "event: ~a~%data: ~a~%~%" event data))

(defun codex-message-item (text)
  (format nil "{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":~a}]}"
          (apprentice::lisp-to-json-string text)))

(defun codex-call-item (id name args-json)
  (format nil "{\"type\":\"function_call\",\"call_id\":~a,\"name\":~a,\"arguments\":~a}"
          (apprentice::lisp-to-json-string id)
          (apprentice::lisp-to-json-string name)
          (apprentice::lisp-to-json-string args-json)))

(defun codex-reasoning-item ()
  "{\"type\":\"reasoning\",\"id\":\"rs_1\",\"summary\":[],\"encrypted_content\":\"opaque-sentinel\"}")

(defun codex-done-data (item-json)
  (format nil "{\"type\":\"response.output_item.done\",\"item\":~a}" item-json))

(defun codex-completed-frame ()
  (codex-sse-frame "response.completed"
                   "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"))

(defun codex-text-sse (text)
  (concatenate 'string
               (codex-sse-frame "response.output_item.done"
                                (codex-done-data (codex-message-item text)))
               (codex-completed-frame)))

(defun codex-call-sse ()
  (concatenate 'string
               (codex-sse-frame "response.output_item.done"
                                (codex-done-data (codex-reasoning-item)))
               (codex-sse-frame "response.output_text.delta"
                                "{\"type\":\"response.output_text.delta\",\"delta\":\"partial \"}")
               (codex-sse-frame "response.output_item.done"
                                (codex-done-data
                                 (codex-call-item "call_1" "read" "{\"path\":\"/tmp/a\"}")))
               (codex-completed-frame)))

;;;; Credential and request fixtures

(defparameter +codex-claims-payload+
  "eyJodHRwczovL2FwaS5vcGVuYWkuY29tL2F1dGguY2hhdGdwdF9hY2NvdW50X2lkIjoiYWNjdC1zZW50aW5lbC0xIiwiY2hhdGdwdF9hY2NvdW50X2lzX2ZlZHJhbXAiOnRydWV9")

(defparameter +codex-empty-payload+ "e30")

(defparameter +codex-blank-payload+
  "eyJodHRwczovL2FwaS5vcGVuYWkuY29tL2F1dGguY2hhdGdwdF9hY2NvdW50X2lkIjoiIiwiY2hhdGdwdF9hY2NvdW50X2lzX2ZlZHJhbXAiOmZhbHNlfQ")

(defun codex-jwt (payload)
  (format nil "header.~a.signature" payload))

(defun save-codex-creds (context &key (access "access-sentinel")
                                       (refresh "refresh-sentinel")
                                       (id "id-sentinel")
                                       (obtained 1000)
                                       (expires nil))
  (apprentice::save-openai-credentials
   (apprentice::openai-auth-context-store context)
   (apprentice::openai-auth-context-config context)
   (apprentice::openai-auth-context-profile context)
   (make-store-test-tokens :access access :refresh refresh :id id
                           :obtained-at obtained :expires-at expires)))

(defun capturing-codex-request (&key (status 200) bodies record-fn)
  (let ((remaining (copy-list bodies)))
    (lambda (url headers body)
      (when record-fn
        (funcall record-fn (list :url url :headers headers :body body)))
      (apprentice::make-auth-http-response
       :status status
       :body (if (rest remaining) (pop remaining) (first remaining))))))

(defmacro record-into (place)
  `(lambda (call) (push call ,place)))

(defun codex-test-tool ()
  (apprentice::make-tool :name "read" :description "Read a file"
                         :schema (let ((schema (make-hash-table :test #'equal)))
                                   (setf (gethash "type" schema) "object")
                                   schema)
                         :fn (lambda (args)
                               (declare (ignore args))
                               "contents")))

(defun find-codex-model ()
  (find "openai-codex" apprentice::*models-list*
        :key #'apprentice::model-name :test #'string=))

(defun call-with-codex-model (context model-id function &key request-fn)
  (let ((old-models apprentice::*models-list*))
    (unwind-protect
         (progn
           (apprentice::configure-openai-device-model
            context model-id :request-fn request-fn)
           (funcall function))
      (setf apprentice::*models-list* old-models))))

;;;; AC1: registration and verified wire format

(deftest codex-configure-registers-context-bound-model
  (call-with-temp-project-root
   (lambda (root)
     (let ((old-models apprentice::*models-list*)
           (old-model apprentice::*model*)
           (old-sub apprentice::*subagent-model*))
       (unwind-protect
            (let ((context (apprentice::make-openai-auth-context root)))
              (is-equal "openai-codex"
                        (apprentice::configure-openai-device-model
                         context "gpt-6-astra"))
              (is (member "openai-codex" (apprentice::available-models)
                          :test #'string=))
              (is (eq old-model apprentice::*model*))
              (is (eq old-sub apprentice::*subagent-model*))
              (flet ((codex-count ()
                       (count "openai-codex"
                              (mapcar #'apprentice::model-name
                                      apprentice::*models-list*)
                              :test #'string=)))
                (is-equal 1 (codex-count))
                (apprentice::configure-openai-device-model
                 context "gpt-5.6-sol")
                (is-equal 1 (codex-count)))
              (signals apprentice::invalid-auth-config
                (apprentice::configure-openai-device-model context ""))
              (signals apprentice::invalid-auth-config
                (apprentice::configure-openai-device-model "nope" "x")))
         (setf apprentice::*models-list* old-models))))))

(deftest codex-request-uses-verified-origin-headers-body
  (call-with-temp-project-root
   (lambda (root)
     (let ((calls nil)
           (context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context :id (codex-jwt +codex-claims-payload+))
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let ((turn (apprentice::run-model
                       (find-codex-model)
                       (list (apprentice::make-turn :role :system :text "Be terse.")
                             (apprentice::make-turn :role :user :text "Use the read tool"))
                       (list (codex-test-tool))
                       :request-fn (capturing-codex-request
                                    :bodies (list (codex-text-sse "done"))
                                    :record-fn (record-into calls)))))
            (is-equal "done" (apprentice::turn-text turn)))))
       (is-equal 1 (length calls))
       (let* ((request (first calls))
              (url (getf request :url))
              (headers (getf request :headers))
              (body (getf request :body))
              (header (lambda (name)
                        (cdr (assoc name headers :test #'string=)))))
         (is-equal "https://chatgpt.com/backend-api/codex/responses" url)
         (is-equal "Bearer access-sentinel" (funcall header "Authorization"))
         (is-equal "acct-sentinel-1" (funcall header "ChatGPT-Account-ID"))
         (is-equal "true" (funcall header "X-OpenAI-Fedramp"))
         (is-equal "application/json" (funcall header "Content-Type"))
         (is-equal "text/event-stream" (funcall header "Accept"))
         (is (contains-p "apprentice" (funcall header "User-Agent")))
         (is (not (contains-p "codex_cli" (funcall header "User-Agent"))))
         (is-equal "apprentice" (funcall header "originator"))
         (let ((decoded (json:decode-json-from-string body)))
           (is-equal "gpt-6-astra" (apprentice::s decoded "model"))
           (is-equal "Be terse." (apprentice::s decoded "instructions"))
           (is-equal "auto" (apprentice::s decoded "tool_choice"))
           (is-equal "medium" (apprentice::s decoded "reasoning" "effort")))
         (is (contains-p "\"strict\":false" body))
         (is (contains-p "\"store\":false" body))
         (is (contains-p "\"stream\":true" body))
         (is (contains-p "reasoning.encrypted_content" body))
         (is (contains-p "\"parallel_tool_calls\":false" body))
         (is (not (contains-p "api.openai.com/v1" url))))))))

(deftest codex-omits-optional-body-fields
  (call-with-temp-project-root
   (lambda (root)
     (let ((calls nil)
           (context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context)
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (apprentice::run-model
           (find-codex-model)
           (list (apprentice::make-turn :role :user :text "hi"))
           nil
           :effort "high" :parallel-tool-calls t
           :request-fn (capturing-codex-request
                        :bodies (list (codex-text-sse "hi"))
                        :record-fn (record-into calls)))))
       (let ((decoded (json:decode-json-from-string
                       (getf (first calls) :body))))
         (is (null (apprentice::s decoded "instructions")))
         (is (null (apprentice::s decoded "tools")))
         (is-equal "high" (apprentice::s decoded "reasoning" "effort"))
         (is (eq t (apprentice::s decoded "parallel_tool_calls"))))))))

;;;; AC1 transport secrecy with a fake curl subprocess

(defun call-with-fake-codex-curl (function &key (status 200) (response "")
                                                 delay-seconds)
  (let* ((directory (make-test-directory))
         (root (uiop:ensure-directory-pathname directory))
         (script (merge-pathnames "fake-curl" root))
         (arguments (merge-pathnames "argv" root))
         (environment (merge-pathnames "environment" root))
         (input (merge-pathnames "stdin" root))
         (header-path (merge-pathnames "header-path" root))
         (config-capture (merge-pathnames "config" root)))
    (unwind-protect
         (progn
           (with-open-file (stream script :direction :output :if-exists :error)
             (format stream "#!/bin/sh~%")
             (format stream "printf '%s\\n' \"$@\" > ~a~%"
                     (shell-quote (namestring arguments)))
             (format stream "env > ~a~%" (shell-quote (namestring environment)))
             (format stream "cat > ~a~%" (shell-quote (namestring input)))
             (format stream "header=\"\"; config=\"\"~%")
             (format stream "while [ \"$#\" -gt 0 ]; do~%")
             (format stream "  if [ \"$1\" = --dump-header ]; then header=$2; shift 2~%")
             (format stream "  elif [ \"$1\" = --config ]; then config=$2; shift 2~%")
             (format stream "  else shift; fi~%")
             (format stream "done~%")
             (format stream "printf '%s' \"$header\" > ~a~%"
                     (shell-quote (namestring header-path)))
             (format stream "if [ -n \"$config\" ]; then cat \"$config\" > ~a; fi~%"
                     (shell-quote (namestring config-capture)))
             (when delay-seconds
               (format stream "exec sleep ~a~%" delay-seconds))
             (format stream "printf 'HTTP/1.1 ~d Fixture\\r\\nContent-Type: text/event-stream\\r\\n\\r\\n' > \"$header\"~%"
                     status)
             (format stream "printf %s ~a~%" (shell-quote response)))
           (uiop:run-program (list "chmod" "700" (namestring script)))
           (funcall function script arguments environment input
                    header-path config-capture))
      (uiop:delete-directory-tree root :validate t))))

(deftest codex-transport-keeps-secrets-out-of-argv
  (call-with-fake-codex-curl
   (lambda (script arguments environment input header-path config-capture)
     (declare (ignore header-path))
     (let* ((headers '(("Content-Type" . "application/json")
                       ("Accept" . "text/event-stream")
                       ("Authorization" . "Bearer access-sentinel")
                       ("ChatGPT-Account-ID" . "acct-sentinel-1")))
            (response (apprentice::supervised-codex-request
                       (namestring script)
                       "https://chatgpt.com/backend-api/codex/responses"
                       headers "{\"model\":\"m\"}" 10 65536))
            (argv (uiop:read-file-string arguments))
            (env (uiop:read-file-string environment))
            (config (uiop:read-file-string config-capture)))
       (is-equal 200 (apprentice::auth-http-response-status response))
       (is-equal (codex-text-sse "done")
                 (apprentice::auth-http-response-body response))
       (is-equal "--disable"
                 (first (uiop:split-string argv :separator '(#\Newline))))
       (is (not (contains-p "access-sentinel" argv)))
       (is (not (contains-p "acct-sentinel-1" argv)))
       (is (not (contains-p "access-sentinel" env)))
       (is-equal "{\"model\":\"m\"}" (uiop:read-file-string input))
       (is (contains-p "Bearer access-sentinel" config))
       (is (contains-p "acct-sentinel-1" config))))
   :response (codex-text-sse "done")))

(deftest codex-base64url-decodes-jwt-payloads
  (is-equal "f" (apprentice::base64url-to-string "Zg"))
  (is-equal "foo" (apprentice::base64url-to-string "Zm9v"))
  (is-equal "foobar" (apprentice::base64url-to-string "Zm9vYmFy"))
  (is (null (apprentice::base64url-to-string "!!!")))
  (is (null (apprentice::base64url-to-string ""))))

(deftest codex-routing-headers-are-conditional
  (dolist (case (list (list (codex-jwt +codex-claims-payload+) "acct-sentinel-1" "true")
                      (list (codex-jwt +codex-empty-payload+) nil nil)
                      (list (codex-jwt +codex-blank-payload+) nil nil)
                      (list "not-a-jwt" nil nil)
                      (list "id-sentinel" nil nil)))
    (destructuring-bind (id-token expected-account expected-fedramp) case
      (call-with-temp-project-root
       (lambda (root)
         (let ((calls nil)
               (context (apprentice::make-openai-auth-context root)))
           (save-codex-creds context :id id-token)
           (call-with-codex-model
            context "gpt-6-astra"
            (lambda ()
              (let* ((model (find-codex-model))
                     (turn (apprentice::run-model
                            model
                            (list (apprentice::make-turn :role :user :text "hi"))
                            nil
                            :request-fn (capturing-codex-request
                                         :bodies (list (codex-text-sse "hi"))
                                         :record-fn (record-into calls)))))
                (is-equal "hi" (apprentice::turn-text turn)))))
           (let ((headers (getf (first calls) :headers)))
             (is-equal expected-account
                       (cdr (assoc "ChatGPT-Account-ID" headers :test #'string=)))
             (is-equal expected-fedramp
                       (cdr (assoc "X-OpenAI-Fedramp" headers :test #'string=))))))))))

;;;; AC2: SSE parsing and turn derivation

(deftest codex-text-reply-becomes-turn-without-delta-duplication
  (call-with-temp-project-root
   (lambda (root)
     (let ((context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context)
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let* ((sse (concatenate 'string
                                   (codex-sse-frame "response.output_text.delta"
                                                    "{\"type\":\"response.output_text.delta\",\"delta\":\"partial \"}")
                                   (codex-text-sse "done")))
                 (turn (apprentice::run-model
                        (find-codex-model)
                        (list (apprentice::make-turn :role :user :text "hi"))
                        nil
                        :request-fn (capturing-codex-request
                                     :bodies (list sse)))))
            (is-equal "done" (apprentice::turn-text turn))
            (is-equal :end (apprentice::turn-stop turn))
            (is (null (apprentice::turn-calls turn)))
            (is-equal 1 (length (apprentice::turn-raw turn))))))))))

(deftest codex-function-call-becomes-call-with-preserved-raw
  (call-with-temp-project-root
   (lambda (root)
     (let ((context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context)
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let ((turn (apprentice::run-model
                       (find-codex-model)
                       (list (apprentice::make-turn :role :user :text "read it"))
                       (list (codex-test-tool))
                       :request-fn (capturing-codex-request
                                    :bodies (list (codex-call-sse))))))
            (is-equal :tool-use (apprentice::turn-stop turn))
            (is-equal 1 (length (apprentice::turn-calls turn)))
            (let ((call (first (apprentice::turn-calls turn))))
              (is-equal "call_1" (apprentice::tool-call-id call))
              (is-equal "read" (apprentice::tool-call-name call))
              (is-equal "/tmp/a" (apprentice::s (apprentice::tool-call-args call)
                                                "path")))
            (is-equal 2 (length (apprentice::turn-raw turn)))
            (let ((reasoning (find "reasoning" (apprentice::turn-raw turn)
                                   :key (lambda (item)
                                          (apprentice::s item "type"))
                                   :test #'equal)))
              (is (not (null reasoning)))
              (is-equal "opaque-sentinel"
                        (apprentice::s reasoning "encrypted_content"))))))))))

(deftest codex-replays-raw-items-before-tool-output
  (call-with-temp-project-root
   (lambda (root)
     (let ((calls nil)
           (context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context)
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let* ((first-turn (apprentice::run-model
                              (find-codex-model)
                              (list (apprentice::make-turn :role :user
                                                          :text "Use the read tool"))
                              (list (codex-test-tool))
                              :request-fn (capturing-codex-request
                                           :bodies (list (codex-call-sse)))))
                 (history (list (apprentice::make-turn :role :user
                                                      :text "Use the read tool")
                                first-turn
                                (apprentice::make-turn :role :tool-results
                                                      :results '(("call_1" "read" "contents")))))
                 (second-turn (apprentice::run-model
                               (find-codex-model) history (list (codex-test-tool))
                               :request-fn (capturing-codex-request
                                            :bodies (list (codex-text-sse "read it"))
                                            :record-fn (record-into calls)))))
            (is-equal "read it" (apprentice::turn-text second-turn))
            (let* ((input (apprentice::s (json:decode-json-from-string
                                          (getf (first calls) :body))
                                         "input")))
              (is-equal 4 (length input))
              (is-equal "message" (apprentice::s (first input) "type"))
              (is-equal "reasoning" (apprentice::s (second input) "type"))
              (is-equal "opaque-sentinel"
                        (apprentice::s (second input) "encrypted_content"))
              (is-equal "function_call" (apprentice::s (third input) "type"))
              (is-equal "function_call_output"
                        (apprentice::s (fourth input) "type"))
              (is-equal "contents" (apprentice::s (fourth input) "output"))))))))))

(deftest codex-bad-streams-fail-safely
  (dolist (case (list (list (concatenate 'string
                                         (codex-sse-frame "response.output_item.done"
                                                          "not-json{{{sentinel}")
                                         (codex-completed-frame))
                            "malformed")
                      (list (codex-sse-frame "response.output_item.done"
                                             (codex-done-data
                                              (codex-message-item "partial-sentinel")))
                            "truncated")
                      (list (codex-sse-frame "response.failed"
                                             "{\"type\":\"response.failed\"}")
                            "failed")
                      (list (codex-sse-frame "response.incomplete"
                                             "{\"type\":\"response.incomplete\"}")
                            "incomplete")
                      (list (codex-sse-frame "response.output_item.done"
                                             (codex-done-data
                                              (codex-message-item (make-string 1100000 :initial-element #\x))))
                            "oversize")))
    (destructuring-bind (body label) case
      (declare (ignore label))
      (call-with-temp-project-root
       (lambda (root)
         (let ((context (apprentice::make-openai-auth-context root)))
           (save-codex-creds context)
           (call-with-codex-model
            context "gpt-6-astra"
            (lambda ()
              (let* ((model (find-codex-model))
                     (condition
                       (signals apprentice::codex-sse-error
                         (apprentice::run-model
                          model
                          (list (apprentice::make-turn :role :user :text "hi"))
                          nil
                          :request-fn (capturing-codex-request
                                       :bodies (list body))))))
                (is (not (contains-p "sentinel" (printed condition))))
                (is (not (contains-p "partial-sentinel" (printed condition)))))))))))))

(deftest codex-standard-loop-flows-without-real-tools
  (call-with-temp-project-root
   (lambda (root)
     (let ((context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context)
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let ((model (find-codex-model))
                (responses (list (codex-call-sse) (codex-text-sse "after-tool")))
                (calls nil))
            (destructuring-bind (content msgs)
                (apprentice::standard-loop
                 "Use the read tool" :model model :tools (list (codex-test-tool))
                 :max-turns 3
                 :request-fn (capturing-codex-request :bodies responses
                                                      :record-fn (record-into calls)))
              (declare (ignore msgs))
              (is-equal "after-tool" content))
            (is-equal 2 (length calls))
            (let ((second-input (apprentice::s (json:decode-json-from-string
                                                (getf (first calls) :body))
                                               "input")))
              (is (find "function_call_output" second-input
                        :key (lambda (item) (apprentice::s item "type"))
                        :test #'equal))))))))))

;;;; AC3: credential gating and isolation

(deftest codex-missing-and-expired-never-request
  (call-with-temp-project-root
   (lambda (root)
     (let ((calls nil)
           (context (apprentice::make-openai-auth-context root)))
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let ((model (find-codex-model))
                (request (capturing-codex-request
                          :bodies (list (codex-text-sse "hi"))
                          :record-fn (record-into calls))))
            (signals apprentice::reauthentication-required
              (apprentice::run-model
               model (list (apprentice::make-turn :role :user :text "hi")) nil
               :request-fn request))
            (is (null calls))
            (save-codex-creds context :obtained 1 :expires 2)
            (signals apprentice::reauthentication-required
              (apprentice::run-model
               model (list (apprentice::make-turn :role :user :text "hi")) nil
               :request-fn request))
            (is (null calls))
            (save-codex-creds context :obtained 1 :expires nil)
            (let ((turn (apprentice::run-model
                         model (list (apprentice::make-turn :role :user :text "hi")) nil
                         :request-fn request)))
              (is-equal "hi" (apprentice::turn-text turn))
              (is-equal 1 (length calls))))))))))

(deftest codex-401-versus-403
  (call-with-temp-project-root
   (lambda (root)
     (let ((context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context)
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let ((model (find-codex-model)))
            (signals apprentice::reauthentication-required
              (apprentice::run-model
               model (list (apprentice::make-turn :role :user :text "hi")) nil
               :request-fn (capturing-codex-request :status 401 :bodies '(""))))
            (is (apprentice::oauth-tokens-p
                  (apprentice::load-openai-credentials
                   (apprentice::openai-auth-context-store context)
                   (apprentice::openai-auth-context-config context)
                   "default")))
            (let ((condition
                    (signals apprentice::codex-http-error
                      (apprentice::run-model
                       model (list (apprentice::make-turn :role :user :text "hi")) nil
                       :request-fn (capturing-codex-request :status 403 :bodies '(""))))))
              (is-equal 403 (apprentice::auth-error-status condition))
              (is (apprentice::oauth-tokens-p
                    (apprentice::load-openai-credentials
                     (apprentice::openai-auth-context-store context)
                     (apprentice::openai-auth-context-config context)
                     "default")))))))))))

(deftest codex-logout-and-replacement-affect-future-calls
  (call-with-temp-project-root
   (lambda (root)
     (let ((calls nil)
           (context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context :access "access-alpha")
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let ((model (find-codex-model))
                (request (capturing-codex-request
                          :bodies (list (codex-text-sse "one")
                                        (codex-text-sse "two"))
                          :record-fn (record-into calls))))
            (apprentice::run-model
             model (list (apprentice::make-turn :role :user :text "hi")) nil
             :request-fn request)
            (is-equal :logged-out (apprentice::openai-logout context))
            (signals apprentice::reauthentication-required
              (apprentice::run-model
               model (list (apprentice::make-turn :role :user :text "hi")) nil
               :request-fn request))
            (save-codex-creds context :access "access-beta")
            (apprentice::run-model
             model (list (apprentice::make-turn :role :user :text "hi")) nil
             :request-fn request))))
       (is-equal '("Bearer access-beta" "Bearer access-alpha")
                 (mapcar (lambda (call)
                           (cdr (assoc "Authorization" (getf call :headers)
                                       :test #'string=)))
                         calls))))))

(deftest codex-contexts-stay-bound-across-threads
  (call-with-temp-project-root
   (lambda (root)
     (let ((context-a (apprentice::make-openai-auth-context
                       root :profile "alpha"))
           (context-b (apprentice::make-openai-auth-context
                       root :profile "beta")))
       (save-codex-creds context-a :access "access-alpha")
       (save-codex-creds context-b :access "access-beta")
       (let ((calls-a nil) (calls-b nil) (errors nil)
             (model-a nil) (model-b nil))
         (call-with-codex-model
          context-a "model-a"
          (lambda ()
            (setf model-a (find-codex-model))
            (call-with-codex-model
             context-b "model-b"
             (lambda ()
               (setf model-b (find-codex-model))
               (let ((thread-a (bordeaux-threads:make-thread
                                (lambda ()
                                  (handler-case
                                      (apprentice::run-model
                                       model-a
                                       (list (apprentice::make-turn :role :user
                                                                   :text "from-a"))
                                       nil
                                       :request-fn (capturing-codex-request
                                                    :bodies (list (codex-text-sse "reply-a"))
                                                    :record-fn (record-into calls-a)))
                                    (error (e) (push e errors))))))
                     (thread-b (bordeaux-threads:make-thread
                                (lambda ()
                                  (handler-case
                                      (apprentice::run-model
                                       model-b
                                       (list (apprentice::make-turn :role :user
                                                                   :text "from-b"))
                                       nil
                                       :request-fn (capturing-codex-request
                                                    :bodies (list (codex-text-sse "reply-b"))
                                                    :record-fn (record-into calls-b)))
                                    (error (e) (push e errors)))))))
                 (bordeaux-threads:join-thread thread-a)
                 (bordeaux-threads:join-thread thread-b))))))
         (is (null errors))
         (is-equal "Bearer access-alpha"
                   (cdr (assoc "Authorization" (getf (first calls-a) :headers)
                               :test #'string=)))
         (is-equal "Bearer access-beta"
                   (cdr (assoc "Authorization" (getf (first calls-b) :headers)
                               :test #'string=)))
         (is (not (contains-p "access-alpha"
                              (getf (first calls-b) :body))))
         (is (not (contains-p "access-beta"
                              (getf (first calls-a) :body)))))))))

(deftest codex-configure-is-publicly-exported
  (multiple-value-bind (symbol status)
      (find-symbol "CONFIGURE-OPENAI-DEVICE-MODEL" :apprentice)
    (is (eq status :external))
    (is (fboundp symbol))))

;;;; AC4: existing-provider regression snapshot

(deftest codex-existing-provider-snapshot-unchanged
  (let* ((source-file (merge-pathnames "model.lisp"
                                       (asdf:system-source-directory :apprentice)))
         (source (uiop:read-file-string source-file)))
    (is (contains-p "https://api.openai.com/v1/responses" source))
    (is (contains-p "OPENAI_API_KEY" source))
    (is (not (contains-p "chatgpt.com/backend-api" source)))
    (is (member "gpt-5.6-terra" (apprentice::available-models) :test #'string=))
    (is (member "llama-cpp" (apprentice::available-models) :test #'string=))
    (is-equal "llama-cpp" (apprentice::model))
    (dolist (name '("llama-cpp" "claude-sonnet-5" "gpt-5.6-terra"
                    "gemini-3.7-flash" "openrouter"))
      (is (member name (apprentice::available-models) :test #'string=)))))

(deftest codex-responses-format-parse-regression
  (let ((parsed (apprentice::openai-responses-parse
                 (json:decode-json-from-string
                  "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"hello\"}]},{\"type\":\"function_call\",\"call_id\":\"c1\",\"name\":\"read\",\"arguments\":\"{\\\"path\\\":\\\"/a\\\"}\"}],\"status\":\"completed\"}"))))
    (is-equal "hello" (apprentice::turn-text parsed))
    (is-equal :tool-use (apprentice::turn-stop parsed))
    (is-equal "read" (apprentice::tool-call-name
                      (first (apprentice::turn-calls parsed)))))
  (let ((failed (apprentice::openai-responses-parse
                 (json:decode-json-from-string "{\"error\":{\"message\":\"boom\"}}"))))
    (is-equal :error (apprentice::turn-stop failed))
    (is (contains-p "boom" (apprentice::turn-text failed))))
  (let ((overflow (apprentice::openai-responses-parse
                   (json:decode-json-from-string "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[]}],\"status\":\"incomplete\",\"incomplete_details\":{\"reason\":\"max_output_tokens\"}}"))))
    (is-equal :overflow (apprentice::turn-stop overflow)))
  (let ((empty (apprentice::openai-responses-parse
                (json:decode-json-from-string "{\"output\":[],\"status\":\"completed\"}"))))
    (is-equal :error (apprentice::turn-stop empty)))
  (let ((formatted (apprentice::openai-responses-format-message
                    (apprentice::make-turn :role :user :text "hi"))))
    (is-equal "user" (apprentice::s (first formatted) "role")))
  (let ((instructions (nth-value
                       1 (apprentice::openai-responses-format-message
                          (apprentice::make-turn :role :system :text "sys")))))
    (is-equal "sys" (apprentice::s instructions "instructions")))
  (signals error
    (apprentice::check-options
     (list (apprentice::make-param :name :a :key "a" :default :none :transform nil))
     '(:bogus 1))))

(deftest codex-unknown-option-rejected-without-request
  (call-with-temp-project-root
   (lambda (root)
     (let ((calls nil)
           (context (apprentice::make-openai-auth-context root)))
       (save-codex-creds context)
       (call-with-codex-model
        context "gpt-6-astra"
        (lambda ()
          (let ((model (find-codex-model)))
            (signals error
              (apprentice::run-model
               model (list (apprentice::make-turn :role :user :text "hi")) nil
               :bogus-option 1
               :request-fn (capturing-codex-request
                            :bodies (list (codex-text-sse "hi"))
                            :record-fn (record-into calls))))
            (is (null calls)))))))))
