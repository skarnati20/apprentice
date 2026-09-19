;;;; tests/openai-auth-repl.lisp

(in-package #:apprentice-tests)

(defun make-scripted-transport (script &key on-request)
  "SCRIPT is consumed one entry per request: (STATUS BODY) succeeds and
(:ERROR MESSAGE) signals. ON-REQUEST receives (:KIND KIND :URL URL)."
  (let ((remaining (copy-list script)))
    (lambda (kind url fields timeout max-response-bytes cancel-p)
      (declare (ignore fields timeout max-response-bytes cancel-p))
      (when on-request (funcall on-request (list :kind kind :url url)))
      (unless remaining
        (error "unexpected auth request"))
      (let ((entry (pop remaining)))
        (if (and (consp entry) (eq (first entry) :error))
            (error "~a" (second entry))
            (apprentice::make-auth-http-response
             :status (first entry) :body (second entry)))))))

(defun repl-success-script ()
  (mapcar (lambda (step)
            (list 200 (scripted-step-body step)))
          (default-script)))

(defun repl-pending-script (count)
  (cons (list 200 (scripted-step-body (first (default-script))))
        (loop for i below count collect (list 403 "{\"pending\":true}"))))

(deftest repl-context-construction-performs-no-login-or-io
  (call-with-temp-project-root
   (lambda (root)
     (let ((context (apprentice::make-openai-auth-context
                     root :profile "default")))
       (is (apprentice::openai-auth-context-p context))
       (is (null (apprentice::load-openai-credentials
                   (apprentice::openai-auth-context-store context)
                   (apprentice::openai-auth-context-config context)
                   "default")))
       (is (not (uiop:directory-exists-p
                 (merge-pathnames ".apprentice/"
                                  (uiop:ensure-directory-pathname root)))))))))

(deftest repl-login-persists-and-returns-authenticated
  (call-with-temp-project-root
   (lambda (root)
     (let* ((calls nil)
            (context (apprentice::make-openai-auth-context root))
            (stream (make-string-output-stream))
            (result (apprentice::openai-login
                     context :stream stream
                     :transport (make-scripted-transport
                                 (repl-success-script)
                                 :on-request (lambda (call)
                                               (push call calls))))))
       (is-equal :authenticated result)
       (let ((loaded (apprentice::load-openai-credentials
                       (apprentice::openai-auth-context-store context)
                       (apprentice::openai-auth-context-config context)
                       "default")))
         (is (apprentice::oauth-tokens-p loaded))
         (is-equal "access-secret"
                   (apprentice::secret-value
                    (apprentice::oauth-tokens-access-token loaded))))
       (is-equal '(:json :json :form) (mapcar (lambda (c) (getf c :kind))
                                               (reverse calls)))))))

(deftest repl-login-transport-failure-persists-nothing
  (call-with-temp-project-root
   (lambda (root)
     (let* ((context (apprentice::make-openai-auth-context root))
            (script (list (list 200 (scripted-step-body
                                     (first (default-script))))
                          (list :error "poll-transport-sentinel")))
            (condition
              (signals apprentice::transport-failure
                (apprentice::openai-login
                 context :stream (make-string-output-stream)
                 :transport (make-scripted-transport script)))))
       (is-equal :poll (apprentice::auth-error-stage condition))
       (is (not (contains-p "sentinel" (printed condition))))
       (is (null (apprentice::load-openai-credentials
                   (apprentice::openai-auth-context-store context)
                   (apprentice::openai-auth-context-config context)
                   "default")))
       (is-equal :missing
                 (apprentice::openai-auth-status context))))))

(deftest repl-login-storage-failure-leaves-previous-intact
  (call-with-temp-project-root
   (lambda (root)
     (let* ((context (apprentice::make-openai-auth-context root))
            (store (apprentice::openai-auth-context-store context))
            (config (apprentice::openai-auth-context-config context)))
       (apprentice::save-openai-credentials
        store config "default"
        (make-store-test-tokens :access "original-secret"))
       (sb-posix:chmod (apprentice::openai-auth-store-directory store) #o500)
       (unwind-protect
            (signals apprentice::credential-storage-failure
              (apprentice::openai-login
               context :stream (make-string-output-stream)
               :transport (make-scripted-transport (repl-success-script))))
         (sb-posix:chmod (apprentice::openai-auth-store-directory store) #o700))
       (is-equal "original-secret"
                 (apprentice::secret-value
                  (apprentice::oauth-tokens-access-token
                   (apprentice::load-openai-credentials
                    store config "default"))))))))

(deftest repl-invalid-config-fails-before-network
  (call-with-temp-project-root
   (lambda (root)
     (let ((calls nil))
       (signals apprentice::invalid-auth-config
         (apprentice::make-openai-auth-context
          root :issuer "http://auth.example"))
       (signals apprentice::invalid-auth-config
         (apprentice::make-openai-auth-context root :profile ""))
       (is (null calls))))))

(deftest repl-prompt-shows-url-code-expiry-and-warning
  (call-with-temp-project-root
   (lambda (root)
     (let* ((context (apprentice::make-openai-auth-context
                      root :issuer "https://auth.example/"))
            (stream (make-string-output-stream)))
       (apprentice::openai-login
        context :stream stream
        :transport (make-scripted-transport (repl-success-script)))
       (let ((text (get-output-stream-string stream))
             (url "https://auth.example/codex/device"))
         (is (contains-p url text))
         (is (contains-p "ABCD-EFGH" text))
         (is (contains-p "started this login" text))
         ;; The one-time code is displayed, never embedded in the URL.
         (is (not (contains-p "ABCD-EFGH"
                               (subseq text (search url text)
                                       (+ (search url text)
                                          (length url)))))))))))

(deftest repl-cancel-during-wait-unwinds-without-persisting
  (call-with-temp-project-root
   (lambda (root)
     (let* ((context (apprentice::make-openai-auth-context root))
            (waiting nil)
            (cancel-p (lambda () waiting)))
       (signals apprentice::openai-auth-cancelled
         (apprentice::openai-login
          context :stream (make-string-output-stream)
          :cancel-p cancel-p
          :transport (make-scripted-transport (repl-pending-script 5))
          :wait (lambda (seconds cp)
                  (declare (ignore cp))
                  (setf waiting t)
                  (apprentice::interruptible-auth-wait seconds cancel-p))))
       (is (null (apprentice::load-openai-credentials
                   (apprentice::openai-auth-context-store context)
                   (apprentice::openai-auth-context-config context)
                   "default")))))))

(deftest repl-deadline-expiry-unwinds-without-persisting
  (call-with-temp-project-root
   (lambda (root)
     (let ((context (apprentice::make-openai-auth-context
                     root :poll-timeout 1 :request-timeout 1)))
       (signals apprentice::device-auth-timed-out
         (apprentice::openai-login
          context :stream (make-string-output-stream)
          :transport (make-scripted-transport (repl-pending-script 5))))
       (is (null (apprentice::load-openai-credentials
                   (apprentice::openai-auth-context-store context)
                   (apprentice::openai-auth-context-config context)
                   "default")))))))

(deftest repl-status-logout-lifecycle-uses-local-state-only
  (call-with-temp-project-root
   (lambda (root)
     (let* ((context (apprentice::make-openai-auth-context root))
            (store (apprentice::openai-auth-context-store context))
            (config (apprentice::openai-auth-context-config context)))
       (is-equal :missing (apprentice::openai-auth-status context))
       (apprentice::openai-login
        context :stream (make-string-output-stream)
        :transport (make-scripted-transport (repl-success-script)))
       (is-equal :present (apprentice::openai-auth-status context))
       (apprentice::save-openai-credentials
        store config "expired"
        (make-store-test-tokens :obtained-at 1 :expires-at 2))
       (let ((expired-context (apprentice::make-openai-auth-context
                               root :profile "expired")))
         (is-equal :expired (apprentice::openai-auth-status expired-context)))
       (is-equal :logged-out (apprentice::openai-logout context))
       (is-equal :logged-out (apprentice::openai-logout context))
       (is-equal :missing (apprentice::openai-auth-status context))
       (is (null (apprentice::load-openai-credentials
                   store config "default")))
       ;; Other profiles survive the scoped logout.
       (is-equal :expired
                 (apprentice::openai-auth-status
                  (apprentice::make-openai-auth-context
                   root :profile "expired")))))))

(deftest repl-namespace-immune-to-anchor-cwd-changes
  (call-with-temp-project-root
   (lambda (root)
     (call-with-temp-project-root
      (lambda (elsewhere)
        (let* ((context (apprentice::make-openai-auth-context root))
               (old-anchor-dir apprentice::*anchor-dir*)
               (old-allowed apprentice::*allowed-dirs*)
               (old-cwd (uiop:getcwd)))
          (unwind-protect
               (progn
                 (setf apprentice::*anchor-dir* elsewhere)
                 (setf apprentice::*allowed-dirs* (list elsewhere))
                 (sb-posix:chdir elsewhere)
                 (apprentice::openai-login
                  context :stream (make-string-output-stream)
                  :transport (make-scripted-transport (repl-success-script)))
                 (let ((fresh (apprentice::make-openai-auth-store root)))
                   (is (apprentice::oauth-tokens-p
                         (apprentice::load-openai-credentials
                          fresh
                          (apprentice::openai-auth-context-config context)
                          "default"))))
                 (is (null (apprentice::load-openai-credentials
                             (apprentice::make-openai-auth-store elsewhere)
                             (apprentice::openai-auth-context-config context)
                             "default"))))
            (setf apprentice::*anchor-dir* old-anchor-dir)
            (setf apprentice::*allowed-dirs* old-allowed)
            (sb-posix:chdir (namestring old-cwd)))))))))

(deftest repl-auth-functions-are-publicly-exported
  (dolist (name '(apprentice::make-openai-auth-context
                   apprentice::openai-login
                   apprentice::openai-auth-status
                   apprentice::openai-logout))
    (multiple-value-bind (symbol status)
        (find-symbol (symbol-name name) :apprentice)
      (is (eq symbol name))
      (is (eq status :external))
      (is (fboundp symbol)))))

(deftest repl-auth-functions-are-not-model-tools
  (dolist (bundle (list apprentice::*standard-tools*
                        apprentice::*little-coder-tools*
                        apprentice::*subagent-tools*
                        apprentice::*apprentice-tools*))
    (dolist (tool bundle)
      (let ((name (apprentice::tool-name tool)))
        (is (not (search "login" name)))
        (is (not (search "logout" name)))
        (is (not (search "openai" name)))
        (is (not (search "auth" name)))))))

(deftest repl-login-leaks-no-secrets
  (call-with-temp-project-root
   (lambda (root)
     (let* ((history-before apprentice::*chat-history*)
            (context (apprentice::make-openai-auth-context root))
            (stream (make-string-output-stream))
            (result (apprentice::openai-login
                     context :stream stream
                     :transport (make-scripted-transport (repl-success-script))))
            (prompt-text (get-output-stream-string stream)))
       (is-equal :authenticated result)
       ;; The one-time user code is the intended display; nothing else
       ;; secret-bearing may appear on the terminal.
       (is (contains-p "ABCD-EFGH" prompt-text))
       (is (not (contains-p "device-secret" prompt-text)))
       (is (not (contains-p "authorization-secret" prompt-text)))
       (is (not (contains-p "verifier-secret" prompt-text)))
       (is (not (contains-p "access-secret" prompt-text)))
       ;; Returned values, printed context and chat history stay clean.
       (is (not (contains-p "secret" (printed result))))
       (is (not (contains-p "secret" (printed context))))
       (is (eq history-before apprentice::*chat-history*))))))
