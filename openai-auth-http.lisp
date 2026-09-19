;;;; openai-auth-http.lisp

(in-package #:apprentice)


;;;; HTTP request encoding

(defun utf8-octets (string)
  "Return STRING encoded as a list of UTF-8 octets."
  (loop for character across string
        for code = (char-code character)
        append (cond
                 ((<= code #x7f) (list code))
                 ((<= code #x7ff)
                  (list (logior #xc0 (ash code -6))
                        (logior #x80 (logand code #x3f))))
                 ((<= code #xffff)
                  (list (logior #xe0 (ash code -12))
                        (logior #x80 (logand (ash code -6) #x3f))
                        (logior #x80 (logand code #x3f))))
                 (t
                  (list (logior #xf0 (ash code -18))
                        (logior #x80 (logand (ash code -12) #x3f))
                        (logior #x80 (logand (ash code -6) #x3f))
                        (logior #x80 (logand code #x3f)))))))

(defun form-unreserved-octet-p (octet)
  (or (<= (char-code #\A) octet (char-code #\Z))
      (<= (char-code #\a) octet (char-code #\z))
      (<= (char-code #\0) octet (char-code #\9))
      (member octet '(45 46 95 42))))

(defun encode-auth-form-component (value)
  (unless (stringp value)
    (error 'invalid-auth-config :category :form-value))
  (with-output-to-string (stream)
    (dolist (octet (utf8-octets value))
      (cond ((= octet (char-code #\Space))
             (write-char #\+ stream))
            ((form-unreserved-octet-p octet)
             (write-char (code-char octet) stream))
            (t
             (format stream "%~2,'0X" octet))))))

(defun form-field-name (field)
  (if (symbolp field)
      (json-key-name field)
      (error 'invalid-auth-config :category :form-field)))

(defun encode-auth-form (fields)
  "Encode an alist as UTF-8 application/x-www-form-urlencoded data."
  (unless (listp fields)
    (error 'invalid-auth-config :category :form-fields))
  (format nil "~{~a~^&~}"
          (loop for field in fields
                unless (consp field)
                  do (error 'invalid-auth-config :category :form-fields)
                collect (format nil "~a=~a"
                                (encode-auth-form-component
                                 (form-field-name (car field)))
                                (encode-auth-form-component (cdr field))))))


;;;; Supervised curl transport

(defun utf8-octet-count (character)
  (let ((code (char-code character)))
    (cond ((<= code #x7f) 1)
          ((<= code #x7ff) 2)
          ((<= code #xffff) 3)
          (t 4))))

(defun seconds-now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun positive-auth-timeout-p (timeout)
  (and (realp timeout) (> timeout 0)))

(defun trusted-auth-url-p (issuer url)
  (and (stringp url)
       (handler-case
           (progn
             (normalize-auth-issuer url)
             t)
         (invalid-auth-config () nil))
       (starts-with-p url issuer)
       (let ((remainder (subseq url (length issuer))))
         (and (plusp (length remainder))
              (char= (char remainder 0) #\/)))))

(defun require-trusted-auth-url (issuer url)
  (unless (trusted-auth-url-p issuer url)
    (error 'invalid-auth-config :category :transport-url))
  url)

(defun auth-request-body (kind fields)
  (ecase kind
    (:json (lisp-to-json-string fields))
    (:form (encode-auth-form fields))))

(defun auth-request-content-type (kind)
  (ecase kind
    (:json "Content-Type: application/json")
    (:form "Content-Type: application/x-www-form-urlencoded; charset=utf-8")))

(defun make-private-auth-directory ()
  #+sbcl
  (handler-case
      (let ((directory
              (string-right-trim
               '(#\Newline #\Return)
               (uiop:run-program
                '("mktemp" "-d" "-t" "apprentice-auth.XXXXXXXX")
                :output :string :error-output nil))))
        (unless (plusp (length directory))
          (error 'auth-http-failure :category :temporary-directory))
        (uiop:ensure-directory-pathname directory))
    (auth-http-failure (condition) (error condition))
    (error ()
      (error 'auth-http-failure :category :temporary-directory)))
  #-sbcl
  (error 'auth-http-failure :category :unsupported-platform))

(defun make-private-auth-header-file (directory)
  #+sbcl
  (handler-case
      (let ((path
              (string-right-trim
               '(#\Newline #\Return)
               (uiop:run-program
                (list "mktemp"
                      (namestring (merge-pathnames "headers.XXXXXXXX" directory)))
                :output :string :error-output nil))))
        (unless (plusp (length path))
          (error 'auth-http-failure :category :temporary-header))
        path)
    (auth-http-failure (condition) (error condition))
    (error ()
      (error 'auth-http-failure :category :temporary-header)))
  #-sbcl
  (error 'auth-http-failure :category :unsupported-platform))

(defun delete-private-auth-directory (directory)
  (when directory
    (ignore-errors
      (uiop:delete-directory-tree directory :validate t))))

(defun auth-header-status (header-path)
  (handler-case
      (with-open-file (stream header-path :direction :input :external-format :utf-8)
        (let ((status nil))
          (loop for line = (read-line stream nil nil)
                while line
                when (starts-with-p line "HTTP/")
                  do (let ((first-space (position #\Space line)))
                       (when first-space
                         (let ((second-space
                                 (position #\Space line :start (1+ first-space))))
                           (setf status
                                 (parse-integer line :start (1+ first-space)
                                                :end second-space :junk-allowed t)))))
                finally (unless (and (integerp status) (<= 100 status 599))
                          (error 'auth-http-failure :category :invalid-status))
                        (return status))))
    (auth-http-failure (condition) (error condition))
    (error ()
      (error 'auth-http-failure :category :invalid-status))))

(defun auth-curl-command (curl-program kind header-path timeout url)
  (list curl-program
        "--disable"
        "--silent"
        "--show-error"
        "--request" "POST"
        "--header" (auth-request-content-type kind)
        "--data-binary" "@-"
        "--output" "-"
        "--dump-header" header-path
        "--max-time" (format nil "~,3f" (coerce timeout 'double-float))
        "--connect-timeout" (format nil "~,3f" (coerce timeout 'double-float))
        "--max-redirs" "0"
        "--proto" "=https"
        "--proto-redir" "=https"
        "--tlsv1.2"
        "--noproxy" "*"
        url))

(defun collect-bounded-auth-output (stream max-response-bytes state lock ready)
  (handler-case
      (let ((bytes 0))
        (setf (getf state :body)
              (with-output-to-string (output)
                (loop for character = (read-char stream nil nil)
                      while character
                      do (incf bytes (utf8-octet-count character))
                         (when (> bytes max-response-bytes)
                           (setf (getf state :overflow) t)
                           (return))
                         (write-char character output)))))
    (error ()
      (setf (getf state :reader-failed) t)))
  (bordeaux-threads:with-lock-held (lock)
    (setf (getf state :done) t)
    (bordeaux-threads:condition-notify ready)))

(defun stop-and-reap-auth-process (process reader)
  (ignore-errors (uiop:terminate-process process :urgent t))
  (ignore-errors (uiop:wait-process process))
  (when reader
    (ignore-errors (bordeaux-threads:join-thread reader))))

(defun %supervised-auth-curl-request (curl-program kind body timeout
                                      max-response-bytes url cancel-p)
  (unless (and (stringp curl-program) (plusp (length curl-program))
               (positive-auth-timeout-p timeout)
               (and (integerp max-response-bytes) (plusp max-response-bytes)))
    (error 'invalid-auth-config :category :transport-options))
  (let ((directory nil)
        (header-path nil)
        (process nil)
        (reader nil))
    (unwind-protect
         (progn
           (setf directory (make-private-auth-directory)
                 header-path (make-private-auth-header-file directory))
           (setf process
                 (uiop:launch-program
                  (auth-curl-command curl-program kind header-path timeout url)
                  :input :stream :output :stream :error-output nil
                  :external-format :utf-8))
           (let ((input (uiop:process-info-input process))
                 (output (uiop:process-info-output process))
                 (state (list :done nil :overflow nil :reader-failed nil :body ""))
                 (lock (bordeaux-threads:make-lock "auth-curl-reader"))
                 (ready (bordeaux-threads:make-condition-variable))
                 (started-at (seconds-now)))
             (unwind-protect
                  (progn
                    ;; The sole secret-bearing request representation is this pipe.
                    (write-string body input)
                    (finish-output input)
                    (close input)
                    (setf reader
                          (bordeaux-threads:make-thread
                           (lambda ()
                             (collect-bounded-auth-output
                              output max-response-bytes state lock ready))
                           :name "apprentice-auth-curl-reader"))
                    (loop
                      (when (funcall cancel-p)
                        (stop-and-reap-auth-process process reader)
                        (error 'openai-auth-cancelled))
                      (when (getf state :overflow)
                        (stop-and-reap-auth-process process reader)
                        (error 'auth-http-failure :category :response-too-large))
                      (when (getf state :reader-failed)
                        (stop-and-reap-auth-process process reader)
                        (error 'auth-http-failure :category :response-read-failed))
                      (when (not (uiop:process-alive-p process))
                        (return))
                      (when (>= (- (seconds-now) started-at) timeout)
                        (stop-and-reap-auth-process process reader)
                        (error 'auth-http-failure :category :request-timed-out))
                      (bordeaux-threads:with-lock-held (lock)
                        (unless (getf state :done)
                          (bordeaux-threads:condition-wait ready lock :timeout 0.02))))
                    (let ((exit-code (uiop:wait-process process)))
                      (when reader
                        (bordeaux-threads:join-thread reader)
                        (setf reader nil))
                      (when (getf state :overflow)
                        (error 'auth-http-failure :category :response-too-large))
                      (when (getf state :reader-failed)
                        (error 'auth-http-failure :category :response-read-failed))
                      (unless (zerop exit-code)
                        (error 'auth-http-failure :category :curl-failed))
                      (make-auth-http-response
                       :status (auth-header-status header-path)
                       :body (getf state :body))))
               (when process
                 (stop-and-reap-auth-process process reader)
                 (setf reader nil))
               (when reader
                 (ignore-errors (bordeaux-threads:join-thread reader))))))
      (delete-private-auth-directory directory))))

(defun supervised-auth-curl-request (curl-program kind body timeout
                                     max-response-bytes url cancel-p)
  (handler-case
      (%supervised-auth-curl-request curl-program kind body timeout
                                     max-response-bytes url cancel-p)
    (openai-auth-error (condition)
      (error condition))
    (auth-http-failure (condition)
      (error condition))
    (error ()
      (error 'auth-http-failure :category :subprocess-failed))))

(defun make-openai-auth-http-transport (config &key (curl-program "curl"))
  "Return the bounded, cancellable transport required by RUN-OPENAI-DEVICE-AUTH.
Request fields travel only through curl's standard input; the returned closure
accepts the protocol transport contract." 
  (unless (openai-auth-config-p config)
    (error 'invalid-auth-config :category :transport-config))
  (let ((issuer (openai-auth-config-issuer config)))
    (lambda (kind url fields timeout max-response-bytes cancel-p)
      (require-trusted-auth-url issuer url)
      (when (funcall cancel-p)
        (error 'openai-auth-cancelled))
      (supervised-auth-curl-request curl-program kind
                                    (auth-request-body kind fields)
                                    timeout max-response-bytes url cancel-p))))
