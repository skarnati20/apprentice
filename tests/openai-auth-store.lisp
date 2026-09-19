;;;; tests/openai-auth-store.lisp

(in-package #:apprentice-tests)

(defun make-temp-project-root ()
  (uiop:run-program '("mktemp" "-d" "-t" "apprentice-auth-store.XXXXXXXX")
                     :output '(:string :stripped t)))

(defun call-with-temp-project-root (function)
  (let ((root (make-temp-project-root)))
    (unwind-protect (funcall function root)
      (ignore-errors
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname root)
                                    :validate t)))))

(defun make-store-test-config (&key (issuer "https://auth.example")
                                    (client-id "store-test-client"))
  (apprentice::make-openai-auth-config :issuer issuer :client-id client-id))

(defun make-store-test-tokens (&key (access "access-sentinel")
                                    (refresh "refresh-sentinel")
                                    (id "id-sentinel")
                                    (obtained-at 1000)
                                    (expires-at 4600))
  (apprentice::make-oauth-tokens
   :access-token (apprentice::make-secret access)
   :refresh-token (apprentice::make-secret refresh)
   :id-token (apprentice::make-secret id)
   :obtained-at obtained-at
   :expires-at expires-at))

(deftest auth-store-roundtrip-reads-exact-tuple
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config))
            (tokens (make-store-test-tokens)))
       (apprentice::save-openai-credentials store config "default" tokens)
       (let ((loaded (apprentice::load-openai-credentials
                      store config "default")))
         (is (apprentice::oauth-tokens-p loaded))
         (is-equal "access-sentinel"
                   (apprentice::secret-value
                    (apprentice::oauth-tokens-access-token loaded))))))))

(deftest auth-store-namespace-matrix-isolates-tuples
  (call-with-temp-project-root
   (lambda (root-a)
     (call-with-temp-project-root
      (lambda (root-b)
        (let* ((store-a (apprentice::make-openai-auth-store root-a))
               (store-b (apprentice::make-openai-auth-store root-b))
               (config-a (make-store-test-config))
               (config-b (make-store-test-config :client-id "other-client"))
               (tokens-a (make-store-test-tokens :access "access-a"))
               (tokens-b (make-store-test-tokens :access "access-b")))
          (apprentice::save-openai-credentials store-a config-a "default" tokens-a)
          (apprentice::save-openai-credentials store-a config-b "default" tokens-b)
          (apprentice::save-openai-credentials
           store-a config-a "other" (make-store-test-tokens :access "access-c"))
          ;; Exact tuple reads back.
          (is-equal "access-a"
                    (apprentice::secret-value
                     (apprentice::oauth-tokens-access-token
                      (apprentice::load-openai-credentials
                       store-a config-a "default"))))
          ;; Switching root/profile/issuer/client does not select another.
          (is (null (apprentice::load-openai-credentials
                      store-b config-a "default")))
          (is (null (apprentice::load-openai-credentials
                      store-a config-a "missing")))
          (is (null (apprentice::load-openai-credentials
                      store-a (make-store-test-config
                               :issuer "https://other.example")
                      "default")))
          (is-equal "access-b"
                    (apprentice::secret-value
                     (apprentice::oauth-tokens-access-token
                      (apprentice::load-openai-credentials
                       store-a config-b "default"))))
          ;; Updating one tuple preserves unrelated entries.
          (apprentice::save-openai-credentials
           store-a config-a "default" (make-store-test-tokens :access "access-a2"))
          (is-equal "access-a2"
                    (apprentice::secret-value
                     (apprentice::oauth-tokens-access-token
                      (apprentice::load-openai-credentials
                       store-a config-a "default"))))
          (is-equal "access-b"
                    (apprentice::secret-value
                     (apprentice::oauth-tokens-access-token
                      (apprentice::load-openai-credentials
                       store-a config-b "default"))))
          ;; Deleting one tuple preserves unrelated entries.
          (apprentice::delete-openai-credentials store-a config-a "other")
          (is (null (apprentice::load-openai-credentials
                      store-a config-a "other")))
          (is (apprentice::oauth-tokens-p
                (apprentice::load-openai-credentials
                 store-a config-a "default")))
          (is (apprentice::oauth-tokens-p
                (apprentice::load-openai-credentials
                 store-a config-b "default")))))))))

(deftest auth-store-uses-private-permissions
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens))
       (is-equal #o700 (logand #o7777
                               (sb-posix:stat-mode
                                (sb-posix:stat
                                 (apprentice::openai-auth-store-directory store)))))
       (is-equal #o600 (logand #o7777
                               (sb-posix:stat-mode
                                (sb-posix:stat
                                 (apprentice::openai-auth-store-path store)))))
       (is-equal (sb-posix:geteuid)
                 (sb-posix:stat-uid
                  (sb-posix:stat
                   (apprentice::openai-auth-store-path store))))
       ;; No temporary data remains after a successful write.
       (is-equal '("credentials.json")
                 (sort (mapcar #'file-namestring
                               (uiop:directory-files
                                (uiop:ensure-directory-pathname
                                 (apprentice::openai-auth-store-directory store))))
                       #'string<))))))

(deftest auth-store-failed-write-preserves-old-record
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens :access "original"))
       ;; Simulate an interrupted write: read-only directory blocks temp
       ;; file creation, so the save must fail without touching the record.
       (sb-posix:chmod (apprentice::openai-auth-store-directory store) #o500)
       (unwind-protect
            (signals apprentice::credential-storage-failure
              (apprentice::save-openai-credentials
               store config "default" (make-store-test-tokens :access "new")))
         (sb-posix:chmod (apprentice::openai-auth-store-directory store) #o700))
       (is-equal "original"
                 (apprentice::secret-value
                  (apprentice::oauth-tokens-access-token
                   (apprentice::load-openai-credentials
                    store config "default"))))
       (is-equal '("credentials.json")
                 (sort (mapcar #'file-namestring
                               (uiop:directory-files
                                (uiop:ensure-directory-pathname
                                 (apprentice::openai-auth-store-directory store))))
                       #'string<))))))

(deftest auth-store-rejects-symlinked-store
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config))
            (outside (merge-pathnames "outside.json"
                                       (uiop:ensure-directory-pathname root))))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens))
       (let ((before (uiop:read-file-string
                       (apprentice::openai-auth-store-path store))))
         (with-open-file (s outside :direction :output :if-exists :supersede)
           (write-string "{}" s))
         (uiop:delete-file-if-exists
          (apprentice::openai-auth-store-path store))
         (sb-posix:symlink (namestring outside)
                            (apprentice::openai-auth-store-path store))
         (signals apprentice::credential-storage-failure
           (apprentice::load-openai-credentials store config "default"))
         (signals apprentice::credential-storage-failure
           (apprentice::save-openai-credentials
            store config "default" (make-store-test-tokens)))
         (is-equal "{}" (uiop:read-file-string outside))
         (uiop:delete-file-if-exists
          (apprentice::openai-auth-store-path store))
         (with-open-file (s (apprentice::openai-auth-store-path store)
                             :direction :output)
           (write-string before s)))))))

(deftest auth-store-insecure-directory-fails-closed
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens))
       (sb-posix:chmod (apprentice::openai-auth-store-directory store) #o755)
       (signals apprentice::credential-storage-failure
         (apprentice::load-openai-credentials store config "default"))
       (signals apprentice::credential-storage-failure
         (apprentice::save-openai-credentials
          store config "default" (make-store-test-tokens)))
       (sb-posix:chmod (apprentice::openai-auth-store-directory store) #o700)
       (is (apprentice::oauth-tokens-p
              (apprentice::load-openai-credentials
               store config "default")))))))

(deftest auth-store-delete-is-scoped-and-idempotent
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens))
       (apprentice::save-openai-credentials
        store config "other" (make-store-test-tokens :access "other-access"))
       (is (apprentice::delete-openai-credentials store config "default"))
       (is (null (apprentice::load-openai-credentials
                   store config "default")))
       (is-equal "other-access"
                 (apprentice::secret-value
                  (apprentice::oauth-tokens-access-token
                   (apprentice::load-openai-credentials
                    store config "other"))))
       ;; Repeated deletion is safe.
       (is (apprentice::delete-openai-credentials store config "default"))
       (is (null (apprentice::load-openai-credentials
                   store config "default")))
       (is-equal :missing
                 (apprentice::openai-auth-store-status
                  store config "default" :wall-time (lambda () 2000)))))))

(deftest auth-store-stale-login-does-not-resurrect-after-logout
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens))
       ;; An in-flight login observes the store before logout completes.
       (let ((observed (apprentice::auth-store-file-revision store)))
         (apprentice::delete-openai-credentials store config "default")
         (is (null (apprentice::load-openai-credentials
                     store config "default")))
         ;; The stale login must not resurrect the deleted credential.
         (signals apprentice::credential-storage-failure
           (apprentice::save-openai-credentials
            store config "default" (make-store-test-tokens :access "stale")
            :observed-revision observed))
         (is (null (apprentice::load-openai-credentials
                     store config "default")))
         ;; A fresh login after logout succeeds.
         (apprentice::save-openai-credentials
          store config "default" (make-store-test-tokens :access "fresh"))
         (is-equal "fresh"
                   (apprentice::secret-value
                    (apprentice::oauth-tokens-access-token
                     (apprentice::load-openai-credentials
                      store config "default")))))))))

(deftest auth-store-concurrent-writers-preserve-unrelated-profiles
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config))
            (errors nil)
            (threads
              (loop for i from 1 to 4
                    collect (let ((n i))
                              (bordeaux-threads:make-thread
                               (lambda ()
                                 (handler-case
                                     (apprentice::save-openai-credentials
                                      store config
                                      (format nil "profile-~a" n)
                                      (make-store-test-tokens))
                                   (error (e) (push e errors)))))))))
       (dolist (thread threads)
         (bordeaux-threads:join-thread thread))
       (is (null errors))
       (let ((document (apprentice::read-auth-store-document store)))
         (is-equal 4 (length (or (apprentice::s document "credentials")
                                  #()))))))))

(deftest auth-store-status-reports-local-lifecycle
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (is-equal :missing
                 (apprentice::openai-auth-store-status
                  store config "default" :wall-time (lambda () 2000)))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens :expires-at 4600))
       (is-equal :present
                 (apprentice::openai-auth-store-status
                  store config "default" :wall-time (lambda () 2000)))
       (is-equal :expired
                 (apprentice::openai-auth-store-status
                  store config "default" :wall-time (lambda () 4600)))
       (apprentice::save-openai-credentials
        store config "noexpiry"
        (make-store-test-tokens :expires-at nil))
       (is-equal :present
                 (apprentice::openai-auth-store-status
                  store config "noexpiry" :wall-time (lambda () 999999)))))))

(deftest auth-store-restart-subprocess-reads-credentials
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens :access "restart-sentinel"))
       (let* ((asd-path (namestring (truename "apprentice.asd")))
              (output
               (uiop:run-program
                (list "sbcl" "--noinform" "--non-interactive"
                      "--load" (namestring
                                 (merge-pathnames ".quicklisp/setup.lisp"
                                                  (user-homedir-pathname)))
                      "--eval" (format nil "(asdf:load-asd ~s)" asd-path)
                      "--eval" "(asdf:load-system :apprentice)"
                      "--eval"
                      (format nil "(let ((s (apprentice::make-openai-auth-store ~s)))(let ((c (apprentice::make-openai-auth-config :issuer ~s :client-id ~s)))(let ((tt (apprentice::load-openai-credentials s c \"default\")))(format t \"~~a\" (apprentice::secret-value (apprentice::oauth-tokens-access-token tt))))))"
                              root "https://auth.example" "store-test-client"))
                :output '(:string :stripped t)
                :error-output :string)))
         (is-equal "restart-sentinel" output))))))

(deftest auth-store-records-print-redacted
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens :access "print-sentinel"))
       (let* ((loaded (apprentice::load-openai-credentials
                       store config "default"))
              (text (printed loaded))
              (condition-text
                (printed (make-condition
                          'apprentice::credential-storage-failure))))
         (is (contains-p "REDACTED" text))
         (is (not (contains-p "print-sentinel" text)))
         (is (not (contains-p "print-sentinel" condition-text))))))))

(deftest auth-store-credentials-stay-out-of-anchors
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config)))
       (with-open-file (s (merge-pathnames "notes.txt"
                                            (uiop:ensure-directory-pathname root))
                           :direction :output)
         (write-string "ordinary project text" s))
       (apprentice::save-openai-credentials
        store config "default" (make-store-test-tokens :access "anchor-sentinel"))
       (let* ((project (uiop:ensure-directory-pathname root))
              (files (apprentice::collect-files project))
              (paths (mapcar #'namestring files)))
         (is (not (find-if (lambda (p) (contains-p "credentials.json" p)) paths)))
         (is (not (find-if (lambda (p) (contains-p "anchor-sentinel" p)) paths)))
         (dolist (file (apprentice::create-files-from-dir project))
           (is (not (contains-p "anchor-sentinel"
                                 (apprentice::file-content file))))))))))

(deftest auth-store-gitignore-covers-credential-paths
  (let ((output (uiop:run-program '("git" "check-ignore" "-v"
                                     ".apprentice/auth/credentials.json"
                                     ".apprentice/auth/.credentials.1.tmp")
                                   :output :string :error-output :string)))
    (is (contains-p ".apprentice/" output))))

(deftest auth-store-corrupt-version-oversize-fail-safely
  (call-with-temp-project-root
   (lambda (root)
     (let* ((store (apprentice::make-openai-auth-store root))
            (config (make-store-test-config))
            (tokens (make-store-test-tokens)))
       (apprentice::save-openai-credentials store config "default" tokens)
       (let ((before (uiop:read-file-string
                       (apprentice::openai-auth-store-path store))))
         ;; Malformed JSON fails safely without overwrite.
         (with-open-file (s (apprentice::openai-auth-store-path store)
                             :direction :output :if-exists :supersede)
           (write-string "not-json{{{sentinel" s))
         (signals apprentice::credential-storage-failure
           (apprentice::load-openai-credentials store config "default"))
         ;; Unknown schema fails closed with re-login guidance.
         (with-open-file (s (apprentice::openai-auth-store-path store)
                             :direction :output :if-exists :supersede)
           (write-string "{\"schema_version\":999,\"credentials\":[]}" s))
         (signals apprentice::reauthentication-required
           (apprentice::load-openai-credentials store config "default"))
         ;; Oversize data fails safely without overwrite.
         (with-open-file (s (apprentice::openai-auth-store-path store)
                             :direction :output :if-exists :supersede)
           (write-string (make-string
                          (1+ apprentice::+openai-auth-store-max-bytes+)
                          :initial-element #\x) s))
         (signals apprentice::credential-storage-failure
           (apprentice::load-openai-credentials store config "default"))
         ;; Restore the good record to prove failures did not clobber it
         ;; through the store API (direct file fixtures bypass the writer).
         (with-open-file (s (apprentice::openai-auth-store-path store)
                             :direction :output :if-exists :supersede)
           (write-string before s))
         (is-equal "access-sentinel"
                   (apprentice::secret-value
                    (apprentice::oauth-tokens-access-token
                     (apprentice::load-openai-credentials
                      store config "default")))))))))
