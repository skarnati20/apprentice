;;;; openai-auth.lisp

(in-package #:apprentice)


;;;; Redacting protocol values

(defun print-secret (secret stream depth)
  (declare (ignore secret depth))
  (write-string "[REDACTED]" stream))

(defstruct (secret
            (:constructor make-secret (value))
            (:print-function print-secret))
  (value "" :type string :read-only t))

(defstruct device-code
  verification-url
  user-code
  device-auth-id
  interval-seconds)

(defstruct authorization-code
  authorization-code
  code-challenge
  code-verifier)

(defstruct oauth-tokens
  access-token
  refresh-token
  id-token
  obtained-at
  expires-at)

(defstruct auth-http-response
  status
  body)


;;;; Safe conditions

(define-condition openai-auth-error (error) ())

(define-condition invalid-auth-config (openai-auth-error)
  ((category :initarg :category :reader auth-error-category))
  (:report (lambda (condition stream)
             (format stream "Invalid OpenAI authentication configuration (~(~a~))"
                     (auth-error-category condition)))))

(define-condition device-auth-unavailable (openai-auth-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "OpenAI device authentication is unavailable" stream))))

(define-condition device-auth-timed-out (openai-auth-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "OpenAI device authentication timed out" stream))))

(define-condition openai-auth-cancelled (openai-auth-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "OpenAI device authentication was cancelled" stream))))

(define-condition auth-status-error (openai-auth-error)
  ((status :initarg :status :reader auth-error-status)))

(define-condition user-code-request-failed (auth-status-error) ()
  (:report (lambda (condition stream)
             (format stream "OpenAI user-code request failed (HTTP ~d)"
                     (auth-error-status condition)))))

(define-condition polling-failed (auth-status-error) ()
  (:report (lambda (condition stream)
             (format stream "OpenAI device authorization polling failed (HTTP ~d)"
                     (auth-error-status condition)))))

(define-condition token-exchange-failed (auth-status-error) ()
  (:report (lambda (condition stream)
             (format stream "OpenAI token exchange failed (HTTP ~d)"
                     (auth-error-status condition)))))

(define-condition staged-auth-error (openai-auth-error)
  ((stage :initarg :stage :reader auth-error-stage)
   (category :initarg :category :reader auth-error-category)))

(define-condition invalid-response (staged-auth-error) ()
  (:report (lambda (condition stream)
             (format stream "Invalid OpenAI auth response at ~(~a~) (~(~a~)); [REDACTED]"
                     (auth-error-stage condition)
                     (auth-error-category condition)))))

(define-condition transport-failure (staged-auth-error) ()
  (:report (lambda (condition stream)
             (format stream "OpenAI auth transport failed at ~(~a~) (~(~a~)); [REDACTED]"
                     (auth-error-stage condition)
                     (auth-error-category condition)))))

(define-condition auth-http-failure (error)
  ((category :initarg :category :reader auth-error-category))
  (:report (lambda (condition stream)
             (format stream "OpenAI auth HTTP transport failed (~(~a~)); [REDACTED]"
                     (auth-error-category condition)))))

(define-condition credential-storage-failure (openai-auth-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "OpenAI credential storage failed; [REDACTED]" stream))))

(define-condition reauthentication-required (openai-auth-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "OpenAI authentication must be repeated" stream))))

(defmethod print-object ((condition openai-auth-error) stream)
  (print-unreadable-object (condition stream :type t)
    (write-string "[REDACTED]" stream)))

(defmethod print-object ((condition invalid-auth-config) stream)
  (print-unreadable-object (condition stream :type t)
    (format stream "~(~a~) [REDACTED]" (auth-error-category condition))))

(defmethod print-object ((condition auth-status-error) stream)
  (print-unreadable-object (condition stream :type t)
    (format stream "HTTP ~d [REDACTED]" (auth-error-status condition))))

(defmethod print-object ((condition staged-auth-error) stream)
  (print-unreadable-object (condition stream :type t)
    (format stream "~(~a~) ~(~a~) [REDACTED]"
            (auth-error-stage condition)
            (auth-error-category condition))))


;;;; Configuration

(defconstant +openai-auth-response-limit+ 65536)
(defconstant +openai-auth-max-poll-interval+ 60)

(defstruct (openai-auth-config
            (:constructor %make-openai-auth-config))
  issuer
  client-id
  poll-timeout
  request-timeout
  default-poll-interval)

(defun normalize-auth-issuer (issuer)
  (unless (and (stringp issuer)
               (starts-with-p issuer "https://"))
    (error 'invalid-auth-config :category :issuer))
  (when (or (find #\? issuer) (find #\# issuer))
    (error 'invalid-auth-config :category :issuer))
  (let* ((authority-start (length "https://"))
         (authority-end (or (position #\/ issuer :start authority-start)
                            (length issuer)))
         (authority (subseq issuer authority-start authority-end))
         (normalized (string-right-trim "/" issuer)))
    (when (or (zerop (length authority))
              (find #\@ authority)
              (string= normalized "https:"))
      (error 'invalid-auth-config :category :issuer))
    normalized))

(defun positive-duration-p (value)
  (and (realp value) (> value 0)))

(defun make-openai-auth-config (&key
                                  (issuer "https://auth.openai.com")
                                  (client-id "app_EMoamEEZ73f0CkXaXp7hrann")
                                  (poll-timeout 900)
                                  (request-timeout 30)
                                  (default-poll-interval 5))
  (unless (and (stringp client-id) (plusp (length client-id)))
    (error 'invalid-auth-config :category :client-id))
  (unless (positive-duration-p poll-timeout)
    (error 'invalid-auth-config :category :poll-timeout))
  (unless (positive-duration-p request-timeout)
    (error 'invalid-auth-config :category :request-timeout))
  (unless (and (integerp default-poll-interval)
               (<= 1 default-poll-interval +openai-auth-max-poll-interval+))
    (error 'invalid-auth-config :category :poll-interval))
  (%make-openai-auth-config
   :issuer (normalize-auth-issuer issuer)
   :client-id client-id
   :poll-timeout poll-timeout
   :request-timeout request-timeout
   :default-poll-interval default-poll-interval))

(defun auth-url (config path)
  (concatenate 'string (openai-auth-config-issuer config) path))


;;;; Protocol helpers

(defun successful-http-status-p (status)
  (and (integerp status) (<= 200 status 299)))

(defun ensure-auth-active (deadline monotonic-now cancel-p)
  (when (funcall cancel-p)
    (error 'openai-auth-cancelled))
  (let ((remaining (- deadline (funcall monotonic-now))))
    (when (<= remaining 0)
      (error 'device-auth-timed-out))
    remaining))

(defun call-auth-transport (transport stage kind url fields timeout cancel-p)
  (handler-case
      (let ((response
              (funcall transport kind url fields timeout
                       +openai-auth-response-limit+ cancel-p)))
        (unless (and (auth-http-response-p response)
                     (integerp (auth-http-response-status response))
                     (stringp (auth-http-response-body response)))
          (error 'invalid-response :stage stage :category :unexpected-shape))
        response)
    (openai-auth-error (condition)
      (error condition))
    (auth-http-failure (condition)
      (error 'transport-failure :stage stage
             :category (auth-error-category condition)))
    (error ()
      (error 'transport-failure :stage stage :category :request-failed))))

(defun decode-auth-response (response stage)
  (let ((body (auth-http-response-body response)))
    (when (> (length body) +openai-auth-response-limit+)
      (error 'invalid-response :stage stage :category :response-too-large))
    (handler-case
        (json:decode-json-from-string body)
      (error ()
        (error 'invalid-response :stage stage :category :invalid-json)))))

(defun required-auth-string (body field stage)
  (let ((value (s body field)))
    (unless (and (stringp value) (plusp (length value)))
      (error 'invalid-response :stage stage :category :unexpected-shape))
    value))

(defun parse-integer-string (value)
  (handler-case
      (multiple-value-bind (number position)
          (parse-integer value :junk-allowed t)
        (and number (= position (length value)) number))
    (error () nil)))

(defun normalize-poll-interval (value config)
  (let ((interval
          (cond ((null value)
                 (openai-auth-config-default-poll-interval config))
                ((and (integerp value) (zerop value))
                 (openai-auth-config-default-poll-interval config))
                ((integerp value) value)
                ((stringp value) (parse-integer-string value))
                (t nil))))
    (unless (and (integerp interval)
                 (<= 1 interval +openai-auth-max-poll-interval+))
      (error 'invalid-response :stage :user-code :category :invalid-interval))
    interval))

(defun request-device-code (config transport deadline monotonic-now cancel-p)
  (let* ((remaining (ensure-auth-active deadline monotonic-now cancel-p))
         (response
           (call-auth-transport
            transport :user-code :json
            (auth-url config "/api/accounts/deviceauth/usercode")
            (j "client_id" (openai-auth-config-client-id config))
            (min (openai-auth-config-request-timeout config) remaining)
            cancel-p)))
    (ensure-auth-active deadline monotonic-now cancel-p)
    (let ((status (auth-http-response-status response)))
      (cond ((= status 404)
             (error 'device-auth-unavailable))
            ((not (successful-http-status-p status))
             (error 'user-code-request-failed :status status))))
    (let* ((body (decode-auth-response response :user-code))
           (user-code (or (s body "user_code") (s body "usercode"))))
      (unless (and (stringp user-code) (plusp (length user-code)))
        (error 'invalid-response :stage :user-code :category :unexpected-shape))
      (make-device-code
       :verification-url (auth-url config "/codex/device")
       :user-code (make-secret user-code)
       :device-auth-id
       (make-secret (required-auth-string body "device_auth_id" :user-code))
       :interval-seconds
       (normalize-poll-interval (s body "interval") config)))))

(defun wait-for-next-poll (wait seconds cancel-p)
  (handler-case
      (funcall wait seconds cancel-p)
    (openai-auth-error (condition)
      (error condition))
    (error ()
      (error 'transport-failure :stage :poll :category :wait-failed))))

(defun poll-for-authorization (config device transport deadline
                               monotonic-now wait cancel-p)
  (loop
    (let* ((remaining (ensure-auth-active deadline monotonic-now cancel-p))
           (response
             (call-auth-transport
              transport :poll :json
              (auth-url config "/api/accounts/deviceauth/token")
              (j "device_auth_id" (secret-value (device-code-device-auth-id device))
                 "user_code" (secret-value (device-code-user-code device)))
              (min (openai-auth-config-request-timeout config) remaining)
              cancel-p)))
      (setf remaining (ensure-auth-active deadline monotonic-now cancel-p))
      (let ((status (auth-http-response-status response)))
        (cond
          ((successful-http-status-p status)
           (let ((body (decode-auth-response response :poll)))
             (return
               (make-authorization-code
                :authorization-code
                (make-secret
                 (required-auth-string body "authorization_code" :poll))
                :code-challenge
                (make-secret
                 (required-auth-string body "code_challenge" :poll))
                :code-verifier
                (make-secret
                 (required-auth-string body "code_verifier" :poll))))))
          ((member status '(403 404))
           (wait-for-next-poll
            wait
            (min (device-code-interval-seconds device) remaining)
            cancel-p))
          (t
           (error 'polling-failed :status status)))))))

(defun parse-expires-in (body)
  (let ((expires-in (s body "expires_in")))
    (cond ((null expires-in) nil)
          ((and (integerp expires-in) (plusp expires-in)) expires-in)
          (t (error 'invalid-response
                    :stage :exchange :category :invalid-expiry)))))

(defun exchange-authorization-code (config authorization transport deadline
                                    monotonic-now wall-time cancel-p)
  (let* ((remaining (ensure-auth-active deadline monotonic-now cancel-p))
         (response
           (call-auth-transport
            transport :exchange :form
            (auth-url config "/oauth/token")
            (j "grant_type" "authorization_code"
               "code" (secret-value
                         (authorization-code-authorization-code authorization))
               "redirect_uri" (auth-url config "/deviceauth/callback")
               "client_id" (openai-auth-config-client-id config)
               "code_verifier" (secret-value
                                 (authorization-code-code-verifier authorization)))
            (min (openai-auth-config-request-timeout config) remaining)
            cancel-p)))
    (ensure-auth-active deadline monotonic-now cancel-p)
    (let ((status (auth-http-response-status response)))
      (unless (successful-http-status-p status)
        (error 'token-exchange-failed :status status)))
    (let* ((body (decode-auth-response response :exchange))
           (expires-in (parse-expires-in body))
           (obtained-at (funcall wall-time))
           (tokens
             (make-oauth-tokens
              :access-token
              (make-secret (required-auth-string body "access_token" :exchange))
              :refresh-token
              (make-secret (required-auth-string body "refresh_token" :exchange))
              :id-token
              (make-secret (required-auth-string body "id_token" :exchange))
              :obtained-at obtained-at
              :expires-at (and expires-in (+ obtained-at expires-in)))))
      (ensure-auth-active deadline monotonic-now cancel-p)
      tokens)))


;;;; Complete protocol operation

(defun present-device-code (presenter device remaining)
  (handler-case
      (funcall presenter
               (device-code-verification-url device)
               (secret-value (device-code-user-code device))
               remaining
               t)
    (openai-auth-error (condition)
      (error condition))
    (error ()
      (error 'transport-failure
             :stage :presentation :category :presenter-failed))))

;;;; Explicit REPL lifecycle (Design A public contract)
;;
;; A context captures an existing canonical project root, a normalized
;; config and a profile name. It never holds raw tokens: login persists
;; them through the protected store and returns only :authenticated.
;; Status reports local lifecycle state and permitted timestamps, never
;; remote validity or token values. Logout removes exactly this context's
;; three tokens and is idempotent. None of these functions is a model
;; tool, performs implicit login, refreshes automatically, or launches a
;; browser; the one-time user code is shown only on the caller's stream.

(defstruct (openai-auth-context
            (:constructor %make-openai-auth-context)
            (:print-function print-auth-context))
  config
  store
  profile)

(defun print-auth-context (context stream depth)
  (declare (ignore depth))
  (print-unreadable-object (context stream :type t)
    (format stream "~a [REDACTED]"
            (openai-auth-context-profile context))))

(defun make-openai-auth-context (project-dir &key (profile "default")
                                  (issuer "https://auth.openai.com")
                                  (client-id "app_EMoamEEZ73f0CkXaXp7hrann")
                                  (poll-timeout 900)
                                  (request-timeout 30))
  "Capture an explicit device-login namespace for PROJECT-DIR.
PROFILE selects the credential slot; ISSUER, CLIENT-ID, POLL-TIMEOUT and
REQUEST-TIMEOUT normalize exactly like MAKE-OPENAI-AUTH-CONFIG. No
network access and no login happen here, and no credential directory is
created; invalid configuration fails before any request or storage."
  (let ((config (make-openai-auth-config :issuer issuer :client-id client-id
                                         :poll-timeout poll-timeout
                                         :request-timeout request-timeout))
        (clean-profile (normalize-auth-profile profile)))
    (%make-openai-auth-context
     :config config
     :store (make-openai-auth-store project-dir)
     :profile clean-profile)))

(defun interruptible-auth-wait (seconds cancel-p)
  "Sleep SECONDS in small slices so CANCEL-P is honored promptly.
Cancellation raises OPENAI-AUTH-CANCELLED; expiry simply returns."
  (let ((deadline (+ (seconds-now) seconds)))
    (loop
      (when (funcall cancel-p)
        (error 'openai-auth-cancelled))
      (let ((left (- deadline (seconds-now))))
        (when (<= left 0)
          (return))
        (sleep (min 0.05 left))))))

(defun present-auth-code (stream)
  "Return the device-code presenter that writes only to STREAM."
  (lambda (url code remaining warning)
    (declare (ignore warning))
    (format stream "~&OpenAI device login~%To authorize this machine, visit:~%~a~%Enter code: ~a~%The code expires in ~d second~:p.~%Continue only if you started this login in Apprentice.~%"
            url code (max 1 (round remaining)))
    (finish-output stream)))

(defun openai-login (context &key (stream *query-io*) cancel-p
                               transport monotonic-now wall-time wait)
  "Perform the explicit interactive device login for CONTEXT.
The verification URL, one-time code, remaining expiry and an
initiated-by-you warning are shown only on STREAM; the URL never
embeds the code. TRANSPORT, MONOTONIC-NOW, WALL-TIME and WAIT default
to the concrete supervised-curl, monotonic-clock, wall-clock and
interruptible-sleep adapters and exist as seams for tests. Returns
:AUTHENTICATED only after the tokens are atomically persisted; any
protocol, transport, storage, timeout or cancellation failure leaves
previous credentials intact and raises a typed safe condition, and
interruption before persistence stores nothing."
  (unless (openai-auth-context-p context)
    (error 'invalid-auth-config :category :auth-context))
  (let* ((config (openai-auth-context-config context))
         (store (openai-auth-context-store context))
         (profile (openai-auth-context-profile context))
         (cancel (or cancel-p (lambda () nil)))
         (real-transport (or transport
                               (make-openai-auth-http-transport config)))
         (monotonic (or monotonic-now #'seconds-now))
         (wall (or wall-time #'get-universal-time))
         (waiter (or wait #'interruptible-auth-wait))
         (tokens (run-openai-device-auth
                  config
                  :transport real-transport
                  :monotonic-now monotonic
                  :wall-time wall
                  :wait waiter
                  :presenter (present-auth-code stream)
                  :cancel-p cancel)))
    (save-openai-credentials store config profile tokens)
    :authenticated))

(defun openai-auth-status (context)
  "Report the local lifecycle state for CONTEXT without any network.
The primary value is one of :MISSING, :PRESENT or :EXPIRED; the second
value is nonsecret timestamp metadata ((:OBTAINED-AT :EXPIRES-AT)) or
NIL when nothing is stored. This never claims remote validity and
never returns token values. Malformed or unsafe storage raises a safe
storage condition."
  (unless (openai-auth-context-p context)
    (error 'invalid-auth-config :category :auth-context))
  (let ((tokens (load-openai-credentials (openai-auth-context-store context)
                                         (openai-auth-context-config context)
                                         (openai-auth-context-profile context))))
    (cond ((null tokens)
           (values :missing nil))
          (t
           (let ((obtained (oauth-tokens-obtained-at tokens))
                 (expires (oauth-tokens-expires-at tokens)))
             (values (if (and expires (<= expires (get-universal-time)))
                         :expired
                         :present)
                     (list :obtained-at obtained :expires-at expires)))))))

(defun openai-logout (context)
  "Delete exactly CONTEXT's access, refresh and ID tokens and return
:LOGGED-OUT. Scoped to the captured project/issuer/client/profile
tuple, idempotent, and silent about server-side revocation."
  (unless (openai-auth-context-p context)
    (error 'invalid-auth-config :category :auth-context))
  (delete-openai-credentials (openai-auth-context-store context)
                             (openai-auth-context-config context)
                             (openai-auth-context-profile context))
  :logged-out)

(defun run-openai-device-auth (config &key transport monotonic-now wall-time
                                        wait presenter cancel-p)
  "Run the pure device-login protocol and return redacting OAUTH-TOKENS.
TRANSPORT performs bounded requests, MONOTONIC-NOW and WALL-TIME supply time,
WAIT sleeps interruptibly, PRESENTER deliberately receives the one-time code,
and CANCEL-P is checked before every operation. All adapters are required."
  (unless (and (openai-auth-config-p config)
               transport monotonic-now wall-time wait presenter cancel-p)
    (error 'invalid-auth-config :category :adapters))
  (let* ((started-at (funcall monotonic-now))
         (deadline (+ started-at (openai-auth-config-poll-timeout config)))
         (device
           (request-device-code config transport deadline monotonic-now cancel-p)))
    (let ((remaining (ensure-auth-active deadline monotonic-now cancel-p)))
      (present-device-code presenter device remaining))
    (let ((authorization
            (poll-for-authorization config device transport deadline
                                    monotonic-now wait cancel-p)))
      (exchange-authorization-code config authorization transport deadline
                                   monotonic-now wall-time cancel-p))))
