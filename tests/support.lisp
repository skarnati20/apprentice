;;;; tests/support.lisp

(defpackage #:apprentice-tests
  (:use #:cl)
  (:shadow #:step)
  (:export #:run-tests))

(in-package #:apprentice-tests)

(define-condition test-failure (error)
  ((message :initarg :message :reader test-failure-message))
  (:report (lambda (condition stream)
             (write-string (test-failure-message condition) stream))))

(defvar *tests* nil)

(defmacro deftest (name &body body)
  `(push (cons ',name (lambda () ,@body)) *tests*))

(defmacro is (form &optional (message nil message-p))
  `(unless ,form
     (error 'test-failure
            :message ,(if message-p
                          message
                          `(format nil "Assertion failed: ~s" ',form)))))

(defmacro is-equal (expected form &key (test '#'equal))
  `(let ((expected-value ,expected)
         (actual-value ,form))
     (unless (funcall ,test expected-value actual-value)
       (error 'test-failure
              :message (format nil "Expected ~s, got ~s from ~s"
                               expected-value actual-value ',form)))))

(defmacro signals (condition-type &body body)
  `(handler-case
       (progn
         ,@body
         (error 'test-failure
                :message (format nil "Expected condition ~s" ',condition-type)))
     (,condition-type (condition) condition)))

(defun contains-p (needle haystack)
  (and (search needle haystack :test #'char-equal) t))

(defun printed (object)
  (with-output-to-string (stream)
    (prin1 object stream)))

(defstruct scripted-step
  kind
  status
  body
  (advance 0)
  cancel-after
  error-message)

(defun step (kind status body &key (advance 0) cancel-after error-message)
  (make-scripted-step :kind kind
                      :status status
                      :body body
                      :advance advance
                      :cancel-after cancel-after
                      :error-message error-message))

(defstruct fixture
  script
  (calls nil)
  (waits nil)
  (prompts nil)
  (events nil)
  (monotonic 0)
  (wall 1000)
  cancelled
  cancel-on-wait
  wait-error-message
  presenter-error-message
  (active 0)
  (max-active 0))

(defun fixture-transport (fixture kind url fields timeout max-response-bytes cancel-p)
  (declare (ignore cancel-p))
  (let ((step (pop (fixture-script fixture))))
    (unless step
      (error 'test-failure :message "Unexpected transport request"))
    (unless (eq kind (scripted-step-kind step))
      (error 'test-failure
             :message (format nil "Expected ~s transport, got ~s"
                              (scripted-step-kind step) kind)))
    (incf (fixture-active fixture))
    (setf (fixture-max-active fixture)
          (max (fixture-max-active fixture) (fixture-active fixture)))
    (unwind-protect
         (progn
           (push (list :kind kind :url url :fields fields :timeout timeout
                       :max-response-bytes max-response-bytes)
                 (fixture-calls fixture))
           (push (list :request kind) (fixture-events fixture))
           (incf (fixture-monotonic fixture) (scripted-step-advance step))
           (when (scripted-step-cancel-after step)
             (setf (fixture-cancelled fixture) t))
           (when (scripted-step-error-message step)
             (error "~a" (scripted-step-error-message step)))
           (apprentice::make-auth-http-response
            :status (scripted-step-status step)
            :body (scripted-step-body step)))
      (decf (fixture-active fixture)))))

(defun fixture-wait (fixture seconds cancel-p)
  (declare (ignore cancel-p))
  (push seconds (fixture-waits fixture))
  (push (list :wait seconds) (fixture-events fixture))
  (when (fixture-wait-error-message fixture)
    (error "~a" (fixture-wait-error-message fixture)))
  (incf (fixture-monotonic fixture) seconds)
  (when (fixture-cancel-on-wait fixture)
    (setf (fixture-cancelled fixture) t)))

(defun fixture-present (fixture url code expires warning)
  (push (list url code expires warning) (fixture-prompts fixture))
  (push (list :prompt url) (fixture-events fixture))
  (when (fixture-presenter-error-message fixture)
    (error "~a" (fixture-presenter-error-message fixture))))

(defun fixture-calls-in-order (fixture)
  (reverse (fixture-calls fixture)))

(defun fixture-events-in-order (fixture)
  (reverse (fixture-events fixture)))

(defun fixture-waits-in-order (fixture)
  (reverse (fixture-waits fixture)))

(defun fixture-prompts-in-order (fixture)
  (reverse (fixture-prompts fixture)))

(defun default-script (&key
                         (user-body "{\"device_auth_id\":\"device-secret\",\"user_code\":\"ABCD-EFGH\",\"interval\":5}")
                         (poll-body "{\"authorization_code\":\"authorization-secret\",\"code_challenge\":\"challenge-secret\",\"code_verifier\":\"verifier-secret\"}")
                         (token-body "{\"access_token\":\"access-secret\",\"refresh_token\":\"refresh-secret\",\"id_token\":\"id-secret\",\"expires_in\":3600}"))
  (list (step :json 200 user-body)
        (step :json 200 poll-body)
        (step :form 200 token-body)))

(defun run-fixture (fixture &key
                              (issuer "https://auth.openai.com")
                              (client-id "test-client")
                              (poll-timeout 900)
                              (request-timeout 30))
  (let ((config (apprentice::make-openai-auth-config
                 :issuer issuer
                 :client-id client-id
                 :poll-timeout poll-timeout
                 :request-timeout request-timeout)))
    (apprentice::run-openai-device-auth
     config
     :transport (lambda (kind url fields timeout max-response-bytes cancel-p)
                  (fixture-transport fixture kind url fields timeout
                                     max-response-bytes cancel-p))
     :monotonic-now (lambda () (fixture-monotonic fixture))
     :wall-time (lambda () (fixture-wall fixture))
     :wait (lambda (seconds cancel-p)
             (fixture-wait fixture seconds cancel-p))
     :presenter (lambda (url code expires warning)
                  (fixture-present fixture url code expires warning))
     :cancel-p (lambda () (fixture-cancelled fixture)))))

(defun run-tests ()
  (let ((passed 0)
        (failed nil))
    (dolist (entry (reverse *tests*))
      (handler-case
          (progn
            (funcall (cdr entry))
            (incf passed)
            (format t "PASS ~a~%" (car entry)))
        (error (condition)
          (push (cons (car entry) condition) failed)
          (format t "FAIL ~a: ~a~%" (car entry) condition))))
    (format t "~%~d passed, ~d failed~%" passed (length failed))
    (when failed
      (error "Test failures: ~{~a~^, ~}" (mapcar #'car (reverse failed))))
    t))
