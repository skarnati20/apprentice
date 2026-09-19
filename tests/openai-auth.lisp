;;;; tests/openai-auth.lisp

(in-package #:apprentice-tests)

(setf *tests* nil)

(deftest request-normalizes-issuer-and-sends-only-client-id
  (let* ((fixture (make-fixture :script (default-script)))
         (tokens (run-fixture fixture :issuer "https://auth.openai.com///"
                                      :client-id "client-123"))
         (calls (fixture-calls-in-order fixture))
         (first-call (first calls)))
    (is-equal "https://auth.openai.com/api/accounts/deviceauth/usercode"
              (getf first-call :url))
    (is-equal '((:|client_id| . "client-123")) (getf first-call :fields))
    (is-equal "{\"client_id\":\"client-123\"}"
              (apprentice::lisp-to-json-string (getf first-call :fields)))
    (is-equal 65536 (getf first-call :max-response-bytes))
    (is-equal "access-secret"
              (apprentice::secret-value
               (apprentice::oauth-tokens-access-token tokens)))))

(deftest prompt-is-the-only-display-of-the-user-code
  (let ((fixture (make-fixture :script (default-script))))
    (run-fixture fixture)
    (is-equal '(("https://auth.openai.com/codex/device"
                 "ABCD-EFGH" 900 t))
              (fixture-prompts-in-order fixture))))

(deftest prompt-expiry-is-the-remaining-total-deadline
  (let ((fixture
          (make-fixture
           :script (list (step :json 200
                               (scripted-step-body (first (default-script)))
                               :advance 12)
                         (second (default-script))
                         (third (default-script))))))
    (run-fixture fixture)
    (is-equal 888 (third (first (fixture-prompts-in-order fixture))))))

(deftest presenter-failures-are-safe-and-stop-the-flow
  (let* ((fixture
           (make-fixture
            :presenter-error-message "presenter-sentinel-secret"
            :script (list (first (default-script)))))
         (condition (signals apprentice::transport-failure
                      (run-fixture fixture))))
    (is-equal :presentation (apprentice::auth-error-stage condition))
    (is-equal :presenter-failed (apprentice::auth-error-category condition))
    (is-equal 1 (length (fixture-calls fixture)))
    (is (not (contains-p "presenter-sentinel-secret" (printed condition))))))

(deftest user-code-alias-and-interval-normalization
  (dolist (case '(("{\"device_auth_id\":\"d\",\"usercode\":\"u\"}" 5)
                  ("{\"device_auth_id\":\"d\",\"user_code\":\"u\",\"interval\":null}" 5)
                  ("{\"device_auth_id\":\"d\",\"user_code\":\"u\",\"interval\":0}" 5)
                  ("{\"device_auth_id\":\"d\",\"user_code\":\"u\",\"interval\":7}" 7)
                  ("{\"device_auth_id\":\"d\",\"user_code\":\"u\",\"interval\":\"8\"}" 8)))
    (destructuring-bind (body expected-wait) case
      (let ((fixture
              (make-fixture
               :script (list (step :json 200 body)
                             (step :json 403 "{\"pending\":true}")
                             (second (default-script))
                             (third (default-script))))))
        (run-fixture fixture)
        (is-equal (list expected-wait) (fixture-waits-in-order fixture))))))

(deftest invalid-user-code-intervals-fail-safely
  (dolist (value '("-1" "1.5" "\"abc\"" "61"))
    (let* ((body (format nil
                         "{\"device_auth_id\":\"device-secret\",\"user_code\":\"code-secret\",\"interval\":~a}"
                         value))
           (fixture (make-fixture :script (list (step :json 200 body))))
           (condition (signals apprentice::invalid-response
                        (run-fixture fixture))))
      (is-equal :user-code (apprentice::auth-error-stage condition))
      (is-equal :invalid-interval (apprentice::auth-error-category condition))
      (is (not (contains-p "code-secret" (printed condition)))))))

(deftest missing-or-empty-user-code-fields-fail-safely
  (dolist (body '("{\"user_code\":\"u\"}"
                  "{\"device_auth_id\":\"\",\"user_code\":\"u\"}"
                  "{\"device_auth_id\":\"d\"}"
                  "{\"device_auth_id\":\"d\",\"user_code\":\"\"}"))
    (let* ((fixture (make-fixture :script (list (step :json 200 body))))
           (condition (signals apprentice::invalid-response
                        (run-fixture fixture))))
      (is-equal :user-code (apprentice::auth-error-stage condition))
      (is-equal :unexpected-shape (apprentice::auth-error-category condition)))))

(deftest user-code-statuses-map-to-typed-conditions
  (let ((unavailable (make-fixture :script (list (step :json 404 "disabled"))))
        (failed (make-fixture :script (list (step :json 503 "secret-body")))))
    (signals apprentice::device-auth-unavailable (run-fixture unavailable))
    (let ((condition (signals apprentice::user-code-request-failed
                       (run-fixture failed))))
      (is-equal 503 (apprentice::auth-error-status condition))
      (is (not (contains-p "secret-body" (printed condition)))))))

(deftest malformed-and-oversized-user-code-bodies-are-safe
  (dolist (body (list "not-json-secret" (make-string 65537 :initial-element #\x)))
    (let* ((fixture (make-fixture :script (list (step :json 200 body))))
           (condition (signals apprentice::invalid-response
                        (run-fixture fixture))))
      (is (member (apprentice::auth-error-category condition)
                  '(:invalid-json :response-too-large)))
      (is (not (contains-p "not-json-secret" (printed condition)))))))

(deftest polling-is-immediate-and-pending-responses-sleep-afterward
  (let ((fixture
          (make-fixture
           :script (list (first (default-script))
                         (step :json 403 "pending")
                         (step :json 404 "pending")
                         (second (default-script))
                         (third (default-script))))))
    (run-fixture fixture)
    (is-equal '((:request :json) (:prompt "https://auth.openai.com/codex/device")
                (:request :json) (:wait 5) (:request :json) (:wait 5)
                (:request :json) (:request :form))
              (fixture-events-in-order fixture))
    (is-equal '(5 5) (fixture-waits-in-order fixture))
    (is-equal 1 (fixture-max-active fixture))
    (is (null (fixture-script fixture)))))

(deftest unexpected-poll-status-stops-without-retry
  (let* ((fixture (make-fixture
                   :script (list (first (default-script))
                                 (step :json 429 "rate-secret"))))
         (condition (signals apprentice::polling-failed
                      (run-fixture fixture))))
    (is-equal 429 (apprentice::auth-error-status condition))
    (is-equal 2 (length (fixture-calls fixture)))
    (is (null (fixture-waits fixture)))
    (is (not (contains-p "rate-secret" (printed condition))))))

(deftest polling-requires-all-authorization-fields
  (dolist (body '("{\"code_challenge\":\"c\",\"code_verifier\":\"v\"}"
                  "{\"authorization_code\":\"a\",\"code_verifier\":\"v\"}"
                  "{\"authorization_code\":\"a\",\"code_challenge\":\"c\"}"
                  "{\"authorization_code\":\"\",\"code_challenge\":\"c\",\"code_verifier\":\"v\"}"))
    (let ((fixture (make-fixture
                    :script (list (first (default-script))
                                  (step :json 200 body)))))
      (let ((condition (signals apprentice::invalid-response
                         (run-fixture fixture))))
        (is-equal :poll (apprentice::auth-error-stage condition))
        (is-equal :unexpected-shape
                  (apprentice::auth-error-category condition))))))

(deftest one-deadline-covers-initial-poll-and-exchange
  (dolist (case
           (list
            (list (list (step :json 200 "{}" :advance 900)) 1)
            (list (list (first (default-script))
                        (step :json 200 (scripted-step-body (second (default-script)))
                              :advance 900))
                  2)
            (list (list (first (default-script))
                        (second (default-script))
                        (step :form 200 (scripted-step-body (third (default-script)))
                              :advance 900))
                  3)))
    (destructuring-bind (script expected-calls) case
      (let ((fixture (make-fixture :script script)))
        (signals apprentice::device-auth-timed-out (run-fixture fixture))
        (is-equal expected-calls (length (fixture-calls fixture)))))))

(deftest final-sleep-is-capped-by-the-remaining-deadline
  (let ((fixture
          (make-fixture
           :script (list (step :json 200
                               (scripted-step-body (first (default-script)))
                               :advance 898)
                         (step :json 403 "pending")))))
    (signals apprentice::device-auth-timed-out (run-fixture fixture))
    (is-equal '(2) (fixture-waits-in-order fixture))
    (is-equal 2 (length (fixture-calls fixture)))))

(deftest every-request-timeout-is-capped-by-the-remaining-deadline
  (let* ((fixture
           (make-fixture
            :script (list (step :json 200
                                (scripted-step-body (first (default-script)))
                                :advance 898)
                          (second (default-script))
                          (third (default-script)))))
         (tokens (run-fixture fixture))
         (timeouts (mapcar (lambda (call) (getf call :timeout))
                           (fixture-calls-in-order fixture))))
    (is (apprentice::oauth-tokens-p tokens))
    (is-equal '(30 2 2) timeouts)))

(deftest cancellation-prevents-each-next-operation
  (let ((before (make-fixture :cancelled t :script (default-script)))
        (after-user (make-fixture
                     :script (list (step :json 200
                                         (scripted-step-body (first (default-script)))
                                         :cancel-after t))))
        (during-wait (make-fixture
                      :cancel-on-wait t
                      :script (list (first (default-script))
                                    (step :json 403 "pending"))))
        (after-poll (make-fixture
                     :script (list (first (default-script))
                                   (step :json 200
                                         (scripted-step-body (second (default-script)))
                                         :cancel-after t))))
        (after-exchange (make-fixture
                         :script (list (first (default-script))
                                       (second (default-script))
                                       (step :form 200
                                             (scripted-step-body (third (default-script)))
                                             :cancel-after t)))))
    (dolist (case (list (cons before 0) (cons after-user 1)
                        (cons during-wait 2) (cons after-poll 2)
                        (cons after-exchange 3)))
      (signals apprentice::openai-auth-cancelled (run-fixture (car case)))
      (is-equal (cdr case) (length (fixture-calls (car case)))))))

(deftest exchange-uses-exact-five-fields-and-derived-redirect
  (let* ((fixture (make-fixture :script (default-script)))
         (tokens (run-fixture fixture :issuer "https://issuer.example/"
                                      :client-id "client id+&"))
         (call (third (fixture-calls-in-order fixture))))
    (is-equal :form (getf call :kind))
    (is-equal "https://issuer.example/oauth/token" (getf call :url))
    (is-equal '((:|grant_type| . "authorization_code")
                (:|code| . "authorization-secret")
                (:|redirect_uri| . "https://issuer.example/deviceauth/callback")
                (:|client_id| . "client id+&")
                (:|code_verifier| . "verifier-secret"))
              (getf call :fields))
    (is-equal 1000 (apprentice::oauth-tokens-obtained-at tokens))
    (is-equal 4600 (apprentice::oauth-tokens-expires-at tokens))))

(deftest exchange-requires-three-tokens
  (dolist (body '("{\"refresh_token\":\"r\",\"id_token\":\"i\"}"
                  "{\"access_token\":\"a\",\"id_token\":\"i\"}"
                  "{\"access_token\":\"a\",\"refresh_token\":\"r\"}"
                  "{\"access_token\":\"a\",\"refresh_token\":\"r\",\"id_token\":\"\"}"))
    (let ((fixture (make-fixture
                    :script (default-script :token-body body))))
      (let ((condition (signals apprentice::invalid-response
                         (run-fixture fixture))))
        (is-equal :exchange (apprentice::auth-error-stage condition))
        (is-equal :unexpected-shape
                  (apprentice::auth-error-category condition))))))

(deftest expiry-is-optional-but-must-be-a-positive-integer
  (let* ((unknown (make-fixture
                   :script (default-script
                            :token-body "{\"access_token\":\"a\",\"refresh_token\":\"r\",\"id_token\":\"i\"}")))
         (tokens (run-fixture unknown)))
    (is (null (apprentice::oauth-tokens-expires-at tokens))))
  (dolist (expires '(0 -1 1.5 "\"3600\""))
    (let ((fixture
            (make-fixture
             :script
             (default-script
              :token-body
              (format nil
                      "{\"access_token\":\"a\",\"refresh_token\":\"r\",\"id_token\":\"i\",\"expires_in\":~a}"
                      expires)))))
      (let ((condition (signals apprentice::invalid-response
                         (run-fixture fixture))))
        (is-equal :invalid-expiry
                  (apprentice::auth-error-category condition))))))

(deftest non-successful-exchange-maps-status-without-secrets
  (let* ((fixture (make-fixture
                   :script (list (first (default-script))
                                 (second (default-script))
                                 (step :form 400 "token-error-secret"))))
         (condition (signals apprentice::token-exchange-failed
                      (run-fixture fixture))))
    (is-equal 400 (apprentice::auth-error-status condition))
    (is (not (contains-p "token-error-secret" (printed condition))))))

(deftest secret-record-and-condition-printing-is-redacted
  (let* ((secret (apprentice::make-secret "sentinel-secret"))
         (device (apprentice::make-device-code
                  :verification-url "https://auth.example/device"
                  :user-code secret
                  :device-auth-id (apprentice::make-secret "device-sentinel")
                  :interval-seconds 5))
         (authorization (apprentice::make-authorization-code
                         :authorization-code
                         (apprentice::make-secret "authorization-sentinel")
                         :code-challenge
                         (apprentice::make-secret "challenge-sentinel")
                         :code-verifier
                         (apprentice::make-secret "verifier-sentinel")))
         (tokens (apprentice::make-oauth-tokens
                  :access-token secret
                  :refresh-token (apprentice::make-secret "refresh-sentinel")
                  :id-token (apprentice::make-secret "id-sentinel")
                  :obtained-at 1
                  :expires-at nil))
         (condition (make-condition 'apprentice::invalid-response
                                    :stage :poll :category :invalid-json)))
    (dolist (object (list secret device authorization tokens condition))
      (let ((text (printed object)))
        (is (contains-p "REDACTED" text))
        (is (not (contains-p "sentinel" text)))))))

(deftest transport-errors-at-every-phase-are-safe
  (dolist (case
           (list
            (list :user-code
                  (list (step :json nil nil
                              :error-message "user-transport-sentinel")))
            (list :poll
                  (list (first (default-script))
                        (step :json nil nil
                              :error-message "poll-transport-sentinel")))
            (list :exchange
                  (list (first (default-script))
                        (second (default-script))
                        (step :form nil nil
                              :error-message "exchange-transport-sentinel")))))
    (destructuring-bind (expected-stage script) case
      (let* ((fixture (make-fixture :script script))
             (condition (signals apprentice::transport-failure
                          (run-fixture fixture))))
        (is-equal expected-stage (apprentice::auth-error-stage condition))
        (is-equal :request-failed (apprentice::auth-error-category condition))
        (is (not (contains-p "sentinel" (printed condition))))))))

(deftest wait-failures-are-safe-and-stop-polling
  (let* ((fixture
           (make-fixture
            :wait-error-message "wait-sentinel-secret"
            :script (list (first (default-script))
                          (step :json 403 "pending"))))
         (condition (signals apprentice::transport-failure
                      (run-fixture fixture))))
    (is-equal :poll (apprentice::auth-error-stage condition))
    (is-equal :wait-failed (apprentice::auth-error-category condition))
    (is-equal 2 (length (fixture-calls fixture)))
    (is (not (contains-p "wait-sentinel-secret" (printed condition))))))

(deftest invalid-production-config-fails-before-network
  (dolist (args '((:issuer "http://auth.openai.com")
                  (:issuer "https://user@auth.openai.com")
                  (:issuer "https://auth.openai.com?x=1")
                  (:issuer "https://auth.openai.com#fragment")
                  (:client-id "")
                  (:poll-timeout 0)
                  (:request-timeout -1)))
    (let ((fixture (make-fixture :script (default-script))))
      (signals apprentice::invalid-auth-config
        (apply #'run-fixture fixture args))
      (is (null (fixture-calls fixture))))))
