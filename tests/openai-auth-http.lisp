;;;; tests/openai-auth-http.lisp

(in-package #:apprentice-tests)

(deftest auth-form-encoding-preserves-reserved-and-unicode-values
  (is-equal
   "space_key=a+b%2Bc%26d%3De%2Ff%25&unicode=%E2%98%83"
   (apprentice::encode-auth-form
    '((:|space_key| . "a b+c&d=e/f%")
      (:|unicode| . "☃")))))

(defun shell-quote (string)
  (format nil "'~a'"
          (apprentice::substitute-subseq string "'" "'\"'\"'")))

(defun make-test-directory ()
  (uiop:run-program '("mktemp" "-d" "-t" "apprentice-auth-http.XXXXXXXX")
                    :output '(:string :stripped t)))

(defun call-with-fake-curl (function &key
                                        (response-body "{\"ok\":true}")
                                        (status 201)
                                        delay-seconds)
  (let* ((directory (make-test-directory))
         (script (merge-pathnames "fake-curl" (uiop:ensure-directory-pathname directory)))
         (arguments (merge-pathnames "argv" (uiop:ensure-directory-pathname directory)))
         (environment (merge-pathnames "environment" (uiop:ensure-directory-pathname directory)))
         (input (merge-pathnames "stdin" (uiop:ensure-directory-pathname directory))))
    (unwind-protect
         (progn
           (with-open-file (stream script :direction :output :if-exists :error)
             (format stream "#!/bin/sh~%")
             (format stream "printf '%s\\n' \"$@\" > ~a~%"
                     (shell-quote (namestring arguments)))
             (format stream "env > ~a~%" (shell-quote (namestring environment)))
             (format stream "cat > ~a~%" (shell-quote (namestring input)))
             (format stream "while [ \"$#\" -gt 0 ]; do~%")
             (format stream "  if [ \"$1\" = --dump-header ]; then header=$2; shift 2; else shift; fi~%")
             (format stream "done~%")
             (format stream "printf '%s' \"$header\" > ~a~%"
                     (shell-quote (namestring
                                   (merge-pathnames "header-path"
                                                    (uiop:ensure-directory-pathname directory)))))
             (format stream "stat -c '%a' \"$(dirname \"$header\")\" > ~a~%"
                     (shell-quote (namestring
                                   (merge-pathnames "header-directory-mode"
                                                    (uiop:ensure-directory-pathname directory)))))
             (format stream "stat -c '%a' \"$header\" > ~a~%"
                     (shell-quote (namestring
                                   (merge-pathnames "header-mode"
                                                    (uiop:ensure-directory-pathname directory)))))
             (when delay-seconds
               (format stream "exec sleep ~a~%" delay-seconds))
             (format stream "printf 'HTTP/1.1 ~d Fixture\\r\\nContent-Type: application/json\\r\\n\\r\\n' > \"$header\"~%" status)
             (format stream "printf %s ~a~%" (shell-quote response-body)))
           (uiop:run-program (list "chmod" "700" (namestring script)))
           (funcall function script arguments environment input))
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname directory)
                                  :validate t))))

(deftest auth-http-transport-keeps-json-secrets-out-of-argv-and-environment
  (call-with-fake-curl
   (lambda (script arguments environment input)
     (let* ((sentinel "device-auth-sentinel+/&=")
            (config (apprentice::make-openai-auth-config
                     :issuer "https://auth.example"))
            (transport (apprentice::make-openai-auth-http-transport
                        config :curl-program (namestring script)))
            (response (funcall transport :json
                               "https://auth.example/api/accounts/deviceauth/token"
                               (list (cons :|device_auth_id| sentinel))
                               10 65536 (lambda () nil)))
            (argv (uiop:read-file-string arguments))
            (env (uiop:read-file-string environment)))
       (is-equal 201 (apprentice::auth-http-response-status response))
       (is-equal "{\"ok\":true}" (apprentice::auth-http-response-body response))
       (is-equal "{\"device_auth_id\":\"device-auth-sentinel+\\/&=\"}"
                 (uiop:read-file-string input))
       (is-equal "--disable"
                 (first (uiop:split-string argv :separator '(#\Newline))))
       (let ((argv-lines (uiop:split-string argv :separator '(#\Newline))))
         (dolist (argument '("--proto" "--proto-redir" "--max-redirs"
                             "--noproxy" "=https" "0" "*"))
           (is (member argument argv-lines :test #'string=)))
         (is (not (member "--insecure" argv-lines :test #'string=))))
       (is (not (contains-p "--insecure" argv)))
       (is (not (contains-p "--location" argv)))
       (is (not (apprentice-tests::contains-p sentinel argv)))
       (is (not (apprentice-tests::contains-p sentinel env)))))))

(deftest auth-http-transport-uses-private-temporary-files-and-cleans-them-up
  (call-with-fake-curl
   (lambda (script arguments environment input)
     (declare (ignore environment input))
     (let* ((fixture-directory (uiop:pathname-directory-pathname arguments))
            (header-path-file (merge-pathnames "header-path" fixture-directory))
            (directory-mode-file (merge-pathnames "header-directory-mode"
                                                fixture-directory))
            (header-mode-file (merge-pathnames "header-mode" fixture-directory))
            (config (apprentice::make-openai-auth-config
                     :issuer "https://auth.example"))
            (transport (apprentice::make-openai-auth-http-transport
                        config :curl-program (namestring script))))
       (funcall transport :json "https://auth.example/device"
                '((:|client_id| . "client")) 10 65536 (lambda () nil))
       (is-equal (format nil "700~%")
                 (uiop:read-file-string directory-mode-file))
       (is-equal (format nil "600~%")
                 (uiop:read-file-string header-mode-file))
       (is (not (probe-file (uiop:read-file-string header-path-file))))))))

(deftest auth-http-transport-returns-non-success-status-separately
  (call-with-fake-curl
   (lambda (script arguments environment input)
     (declare (ignore arguments environment input))
     (let* ((config (apprentice::make-openai-auth-config
                     :issuer "https://auth.example"))
            (transport (apprentice::make-openai-auth-http-transport
                        config :curl-program (namestring script)))
            (response (funcall transport :json "https://auth.example/device"
                               '((:|client_id| . "client"))
                               10 65536 (lambda () nil))))
       (is-equal 403 (apprentice::auth-http-response-status response))
       (is-equal "{\"error\":\"pending\"}"
                 (apprentice::auth-http-response-body response))))
   :status 403 :response-body "{\"error\":\"pending\"}"))

(deftest auth-http-transport-cancels-a-stalled-curl-process
  (call-with-fake-curl
   (lambda (script arguments environment input)
     (declare (ignore arguments environment input))
     (let* ((checks 0)
            (started-at (get-internal-real-time))
            (config (apprentice::make-openai-auth-config
                     :issuer "https://auth.example"))
            (transport (apprentice::make-openai-auth-http-transport
                        config :curl-program (namestring script))))
       (signals apprentice::openai-auth-cancelled
         (funcall transport :json "https://auth.example/device"
                  '((:|client_id| . "client")) 10 65536
                  (lambda ()
                    (incf checks)
                    (>= checks 3))))
       (is (< (/ (- (get-internal-real-time) started-at)
                 internal-time-units-per-second)
              0.5))))
   :delay-seconds 2))

(deftest auth-http-transport-rejects-untrusted-or-insecure-urls-before-spawn
  (call-with-fake-curl
   (lambda (script arguments environment input)
     (declare (ignore environment input))
     (let* ((config (apprentice::make-openai-auth-config
                     :issuer "https://auth.example"))
            (transport (apprentice::make-openai-auth-http-transport
                        config :curl-program (namestring script))))
       (dolist (url '("http://auth.example/device"
                      "https://auth.example.evil/device"
                      "https://user@auth.example/device"
                      "https://auth.example/device?redirect=https://evil.example"
                      "https://auth.example/device#fragment"))
         (signals apprentice::invalid-auth-config
           (funcall transport :json url '((:|client_id| . "client"))
                    10 65536 (lambda () nil))))
       (is (not (probe-file arguments)))))))

(deftest auth-http-transport-sends-form-data-through-stdin
  (call-with-fake-curl
   (lambda (script arguments environment input)
     (declare (ignore environment))
     (let* ((config (apprentice::make-openai-auth-config
                     :issuer "https://auth.example"))
            (transport (apprentice::make-openai-auth-http-transport
                        config :curl-program (namestring script))))
       (funcall transport :form "https://auth.example/oauth/token"
                '((:|code| . "a b+c&d=e/f%") (:|emoji| . "☃"))
                10 65536 (lambda () nil))
       (is-equal "code=a+b%2Bc%26d%3De%2Ff%25&emoji=%E2%98%83"
                 (uiop:read-file-string input))
       (is (contains-p "application/x-www-form-urlencoded; charset=utf-8"
                       (uiop:read-file-string arguments)))))))

(deftest auth-http-transport-enforces-the-response-cap-while-reading
  (call-with-fake-curl
   (lambda (script arguments environment input)
     (declare (ignore arguments environment input))
     (let* ((config (apprentice::make-openai-auth-config
                     :issuer "https://auth.example"))
            (transport (apprentice::make-openai-auth-http-transport
                        config :curl-program (namestring script)))
            (condition (signals apprentice::auth-http-failure
                         (funcall transport :json "https://auth.example/device"
                                  '((:|client_id| . "client"))
                                  10 16 (lambda () nil)))))
       (is-equal :response-too-large
                 (apprentice::auth-error-category condition))))
   :response-body "0123456789abcdefg"))

(deftest auth-http-transport-maps-subprocess-launch-failure-safely
  (let* ((config (apprentice::make-openai-auth-config
                  :issuer "https://auth.example"))
         (transport (apprentice::make-openai-auth-http-transport
                     config :curl-program "/definitely/missing/curl"))
         (condition (signals apprentice::auth-http-failure
                      (funcall transport :json "https://auth.example/device"
                               '((:|client_id| . "client"))
                               10 65536 (lambda () nil)))))
    (is-equal :subprocess-failed (apprentice::auth-error-category condition))))

(deftest auth-http-transport-times-out-a-stalled-curl-process
  (call-with-fake-curl
   (lambda (script arguments environment input)
     (declare (ignore arguments environment input))
     (let* ((config (apprentice::make-openai-auth-config
                     :issuer "https://auth.example"))
            (transport (apprentice::make-openai-auth-http-transport
                        config :curl-program (namestring script)))
            (condition (signals apprentice::auth-http-failure
                         (funcall transport :json "https://auth.example/device"
                                  '((:|client_id| . "client"))
                                  0.05 65536 (lambda () nil)))))
       (is-equal :request-timed-out
                 (apprentice::auth-error-category condition))))
   :delay-seconds 2))
