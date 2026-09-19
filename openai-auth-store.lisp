;;;; openai-auth-store.lisp

(in-package #:apprentice)

(defconstant +openai-auth-store-schema-version+ 1)
(defconstant +openai-auth-store-max-bytes+ 1048576)
(defconstant +openai-auth-store-lock-timeout-seconds+ 5)

(defstruct (openai-auth-store (:constructor %make-openai-auth-store))
  canonical-root
  directory
  path)

(defun normalize-auth-profile (profile)
  (unless (and (stringp profile) (plusp (length profile)))
    (error 'invalid-auth-config :category :auth-profile))
  (when (or (find #\/ profile) (find #\\ profile) (find #\Nul profile))
    (error 'invalid-auth-config :category :auth-profile))
  profile)

(defun canonicalize-auth-project-root (project-dir)
  (unless (and (stringp project-dir) (plusp (length project-dir)))
    (error 'credential-storage-failure))
  (let ((truename
          (handler-case (truename (uiop:ensure-directory-pathname project-dir))
            (error () (error 'credential-storage-failure)))))
    (unless (and truename (uiop:directory-exists-p truename))
      (error 'credential-storage-failure))
    (namestring truename)))

(defun make-openai-auth-store (project-dir)
  (let* ((root (canonicalize-auth-project-root project-dir))
         (directory (concatenate 'string root ".apprentice/auth/"))
         (path (concatenate 'string directory "credentials.json")))
    (%make-openai-auth-store
     :canonical-root root :directory directory :path path)))

;;;; Protected filesystem boundary
;;
;; The auth subdirectory is the private boundary: it is created 0700 and
;; verified on every operation, as are 0600 credential files. Symlinked,
;; mis-owned or mis-permissioned stores fail closed. Writes use
;; same-directory 0600 temporary files plus atomic rename under a
;; cooperating-process lock directory, so interrupted writes preserve the
;; old complete record and leave no temporary data. Plaintext file storage
;; protects only against other users via OS permissions; same-user shell or
;; Lisp access remains trusted and capable of reading these files.

(defun auth-store-lock-path (store)
  (concatenate 'string (openai-auth-store-path store) ".lock"))

(defun strip-trailing-slash (path)
  (string-right-trim "/" path))

(defun auth-store-lstat (path)
  #+sbcl
  (handler-case (sb-posix:lstat path)
    (error () (error 'credential-storage-failure)))
  #-sbcl
  (error 'credential-storage-failure))

(defun auth-store-stat (path)
  #+sbcl
  (handler-case (sb-posix:stat path)
    (error () (error 'credential-storage-failure)))
  #-sbcl
  (error 'credential-storage-failure))

(defun fail-closed-store ()
  (error 'credential-storage-failure))

(defun verify-auth-store-directory-present (store)
  "Signal CREDENTIAL-STORAGE-FAILURE unless the auth directory exists
with mode 0700, owned by the effective user, and not a symlink."
  (let* ((directory (openai-auth-store-directory store))
         (probe (strip-trailing-slash directory)))
    (unless (uiop:directory-exists-p directory)
      (fail-closed-store))
    (let ((link (auth-store-lstat probe)))
      #+sbcl
      (when (sb-posix:s-islNK (sb-posix:stat-mode link))
        (fail-closed-store))
      #-sbcl (fail-closed-store))
    (let ((info (auth-store-stat directory)))
      #+sbcl
      (unless (and (sb-posix:s-isdir (sb-posix:stat-mode info))
                   (= (logand (sb-posix:stat-mode info) #o777) #o700)
                   (= (sb-posix:stat-uid info) (sb-posix:geteuid)))
        (fail-closed-store))
      #-sbcl (fail-closed-store)))
  t)

(defun ensure-auth-store-directory (store)
  "Create the auth directory 0700 when absent; fail closed when a
present directory is symlinked, mis-owned or mis-permissioned."
  (let ((directory (openai-auth-store-directory store)))
    (if (uiop:directory-exists-p directory)
        (verify-auth-store-directory-present store)
        (progn
          #+sbcl
          (handler-case (ensure-directories-exist directory)
            (error () (fail-closed-store)))
          #-sbcl (fail-closed-store)
          #+sbcl
          (handler-case
              (sb-posix:chmod (strip-trailing-slash directory) #o700)
            (error () (fail-closed-store)))
          #-sbcl (fail-closed-store)
          (verify-auth-store-directory-present store)))
    directory))

(defun auth-store-file-exists-p (store)
  (handler-case
      (progn (auth-store-lstat (openai-auth-store-path store)) t)
    (credential-storage-failure () nil)))

(defun verify-auth-store-file (store)
  "Signal CREDENTIAL-STORAGE-FAILURE unless the credential file is a
regular 0600 file owned by the effective user and within bounds."
  (let ((path (openai-auth-store-path store)))
    (let ((link (auth-store-lstat path)))
      #+sbcl
      (when (sb-posix:s-islnk (sb-posix:stat-mode link))
        (fail-closed-store))
      #-sbcl (fail-closed-store))
    (let ((info (auth-store-stat path)))
      #+sbcl
      (unless (and (sb-posix:s-isreg (sb-posix:stat-mode info))
                   (= (logand (sb-posix:stat-mode info) #o777) #o600)
                   (= (sb-posix:stat-uid info) (sb-posix:geteuid))
                   (<= (sb-posix:stat-size info) +openai-auth-store-max-bytes+))
        (fail-closed-store))
      #-sbcl (fail-closed-store)))
  t)

(defun acquire-auth-store-lock (store &key (timeout +openai-auth-store-lock-timeout-seconds+))
  #+sbcl
  (let ((deadline (+ (seconds-now) timeout)))
    (loop
      (handler-case
          (progn (sb-posix:mkdir (auth-store-lock-path store) #o700)
                 (return t))
        (error ()
          (when (>= (seconds-now) deadline)
            (fail-closed-store))
          (sleep 0.02)))))
  #-sbcl (fail-closed-store))

(defun release-auth-store-lock (store)
  #+sbcl (ignore-errors (sb-posix:rmdir (auth-store-lock-path store)))
  #-sbcl nil)

(defmacro with-auth-store-lock ((store) &body body)
  `(progn
     (acquire-auth-store-lock ,store)
     (unwind-protect (progn ,@body)
       (release-auth-store-lock ,store))))

(defun make-auth-store-temp-path (store)
  #+sbcl
  (format nil "~a.credentials.~a.~a.~a.tmp"
          (openai-auth-store-directory store)
          (sb-posix:getpid) (get-universal-time) (random 1000000))
  #-sbcl (fail-closed-store))

(defun auth-store-entry-matches-p (entry root issuer client-id profile)
  (and (string= (s entry "project_root") root)
       (string= (s entry "issuer") issuer)
       (string= (s entry "client_id") client-id)
       (string= (s entry "profile") profile)))

(defun auth-entry-to-tokens (entry)
  (let ((access (s entry "access_token"))
        (refresh (s entry "refresh_token"))
        (id (s entry "id_token"))
        (obtained (s entry "obtained_at"))
        (expires (s entry "expires_at")))
    (unless (and (stringp access) (plusp (length access))
                 (stringp refresh) (plusp (length refresh))
                 (stringp id) (plusp (length id))
                 (integerp obtained))
      (error 'credential-storage-failure))
    (unless (or (null expires) (integerp expires))
      (error 'credential-storage-failure))
    (make-oauth-tokens :access-token (make-secret access)
                       :refresh-token (make-secret refresh)
                       :id-token (make-secret id)
                       :obtained-at obtained
                       :expires-at expires)))

(defun tokens-to-auth-entry (root issuer client-id profile tokens)
  (unless (oauth-tokens-p tokens)
    (error 'invalid-auth-config :category :auth-tokens))
  (j "project--root" root
     "issuer" issuer
     "client--id" client-id
     "profile" profile
     "access--token" (secret-value (oauth-tokens-access-token tokens))
     "refresh--token" (secret-value (oauth-tokens-refresh-token tokens))
     "id--token" (secret-value (oauth-tokens-id-token tokens))
     "obtained--at" (oauth-tokens-obtained-at tokens)
     ;; NIL encodes as JSON null and decodes back to NIL.
     "expires--at" (oauth-tokens-expires-at tokens)))

;;;; Namespace revisions and logout tombstones
;;
;; Every write bumps the document REVISION. Deleting a tuple also records
;; a logout tombstone carrying the revision of the delete. A save carrying
;; stale observed revision refuses to resurrect a tuple whose tombstone is
;; newer than what it observed, so an older in-flight login cannot undo a
;; completed logout. Unrelated concurrent writes merge via re-read under
;; the lock and are never blocked by other namespaces' tombstones.

(defun fresh-auth-store-document ()
  (j "schema--version" +openai-auth-store-schema-version+
     "revision" 0
     "credentials" #()
     "tombstones" #()))

(defun auth-store-document-revision (document)
  (let ((revision (s document "revision")))
    (if (integerp revision) revision 0)))

(defun auth-store-document-tombstones (document)
  (let ((raw (or (s document "tombstones") nil)))
    (if (vectorp raw) (coerce raw 'list) raw)))

(defun auth-store-document-credentials (document)
  (let ((raw (or (s document "credentials") nil)))
    (if (vectorp raw) (coerce raw 'list) raw)))

(defun auth-store-tombstone-revision (document root issuer client-id profile)
  (let ((tombstone
          (find-if (lambda (entry)
                     (auth-store-entry-matches-p
                      entry root issuer client-id profile))
                   (auth-store-document-tombstones document))))
    (if tombstone
        (let ((revision (s tombstone "revision")))
          (if (integerp revision) revision 0))
        0)))

(defun make-auth-tombstone (root issuer client-id profile revision)
  (j "project--root" root
     "issuer" issuer
     "client--id" client-id
     "profile" profile
     "deleted--at" (get-universal-time)
     "revision" revision))

(defun auth-store-file-revision (store)
  "The current document revision, or 0 when no usable store exists.
Used to observe the store before an operation for stale-write checks."
  (handler-case (auth-store-document-revision (read-auth-store-document store))
    (error () 0)))

(defun read-auth-store-document (store)
  (if (not (uiop:directory-exists-p (openai-auth-store-directory store)))
      (fresh-auth-store-document)
      (progn
        (verify-auth-store-directory-present store)
        (if (not (auth-store-file-exists-p store))
            (fresh-auth-store-document)
            (progn
              (verify-auth-store-file store)
              (let ((text
                      (handler-case
                          (uiop:read-file-string (openai-auth-store-path store)
                                               :external-format :utf-8)
                        (error () (fail-closed-store)))))
                (when (> (length text) +openai-auth-store-max-bytes+)
                  (fail-closed-store))
                (handler-case (json:decode-json-from-string text)
                  (error () (fail-closed-store)))))))))

(defun write-auth-store-document (store document)
  "Atomically replace the credential file. The directory must already
be verified and the store lock held by the caller. Temporary data is
0600 from before secrets are written and is always cleaned up; the old
complete record survives any failure before the rename."
  (ensure-auth-store-directory store)
  (let ((text (lisp-to-json-string document))
        (temp (make-auth-store-temp-path store)))
    (unwind-protect
         (progn
           #+sbcl
           (handler-case
               (with-open-file (stream temp :direction :output
                                            :if-exists :error
                                            :if-does-not-exist :create
                                            :external-format :utf-8)
                 (sb-posix:chmod temp #o600)
                 (write-string text stream)
                 (finish-output stream))
             (error () (fail-closed-store)))
           #-sbcl (fail-closed-store)
           #+sbcl
           (handler-case (sb-posix:rename temp (openai-auth-store-path store))
             (error () (fail-closed-store)))
           #-sbcl (fail-closed-store)
           (verify-auth-store-file store))
      #+sbcl (ignore-errors (sb-posix:unlink temp))
      #-sbcl nil)))

(defun save-openai-credentials (store config profile tokens &key observed-revision)
  "Persist TOKENS for the exact store/config/profile tuple. When
OBSERVED-REVISION is supplied (tests and login flows racing a logout),
a logout tombstone newer than the observation aborts the save instead of
resurrecting the deleted credential."
  (unless (and (openai-auth-store-p store) (openai-auth-config-p config))
    (error 'invalid-auth-config :category :auth-store))
  (let* ((clean-profile (normalize-auth-profile profile))
         (root (openai-auth-store-canonical-root store))
         (issuer (openai-auth-config-issuer config))
         (client-id (openai-auth-config-client-id config))
         (new-entry (tokens-to-auth-entry
                     root issuer client-id clean-profile tokens))
         (observed (if (integerp observed-revision)
                       observed-revision
                       (auth-store-file-revision store))))
    (ensure-auth-store-directory store)
    (with-auth-store-lock (store)
      (let* ((document (read-auth-store-document store))
             (revision (auth-store-document-revision document))
             (credentials (auth-store-document-credentials document))
             (tombstones (auth-store-document-tombstones document)))
        (when (> (auth-store-tombstone-revision
                  document root issuer client-id clean-profile)
                 observed)
          (fail-closed-store))
        (let* ((others (remove-if (lambda (entry)
                                    (auth-store-entry-matches-p
                                     entry root issuer client-id clean-profile))
                                  credentials))
               (new-credentials (append others (list new-entry)))
               (new-tombstones
                 (remove-if (lambda (entry)
                              (auth-store-entry-matches-p
                               entry root issuer client-id clean-profile))
                            tombstones)))
          (write-auth-store-document
           store (j "schema--version" +openai-auth-store-schema-version+
                    "revision" (1+ revision)
                    "credentials" (or new-credentials #())
                    "tombstones" (or new-tombstones #()))))))
    tokens))

(defun load-openai-credentials (store config profile)
  (unless (and (openai-auth-store-p store) (openai-auth-config-p config))
    (error 'invalid-auth-config :category :auth-store))
  (let* ((clean-profile (normalize-auth-profile profile))
         (root (openai-auth-store-canonical-root store))
         (issuer (openai-auth-config-issuer config))
         (client-id (openai-auth-config-client-id config))
         (document (read-auth-store-document store))
         (version (s document "schema_version")))
    (unless (eql version +openai-auth-store-schema-version+)
      (error 'reauthentication-required))
    (let* ((raw (or (s document "credentials") nil))
           (credentials (if (vectorp raw) (coerce raw 'list) raw))
           (entry (find-if (lambda (e)
                             (auth-store-entry-matches-p
                              e root issuer client-id clean-profile))
                           credentials)))
      (when entry (auth-entry-to-tokens entry)))))

(defun delete-openai-credentials (store config profile)
  (unless (and (openai-auth-store-p store) (openai-auth-config-p config))
    (error 'invalid-auth-config :category :auth-store))
  (let* ((clean-profile (normalize-auth-profile profile))
         (root (openai-auth-store-canonical-root store))
         (issuer (openai-auth-config-issuer config))
         (client-id (openai-auth-config-client-id config)))
    (ensure-auth-store-directory store)
    (with-auth-store-lock (store)
      (let* ((document (read-auth-store-document store))
             (revision (auth-store-document-revision document))
             (credentials (auth-store-document-credentials document))
             (tombstones (auth-store-document-tombstones document))
             (remaining (remove-if (lambda (e)
                                     (auth-store-entry-matches-p
                                      e root issuer client-id clean-profile))
                                   credentials))
             (had-entry (< (length remaining) (length credentials))))
        ;; Idempotent: deleting an absent tuple changes nothing, whether
        ;; or not its logout was already recorded.
        (when had-entry
          (let ((new-revision (1+ revision)))
            (write-auth-store-document
             store (j "schema--version" +openai-auth-store-schema-version+
                      "revision" new-revision
                      "credentials" (or remaining #())
                      "tombstones"
                      (or (append
                           (remove-if
                            (lambda (e)
                              (auth-store-entry-matches-p
                               e root issuer client-id clean-profile))
                            tombstones)
                           (list (make-auth-tombstone
                                  root issuer client-id clean-profile
                                  new-revision)))
                          #())))))))
    t))

(defun openai-auth-store-status (store config profile &key (wall-time #'get-universal-time))
  (let ((tokens (load-openai-credentials store config profile)))
    (cond ((null tokens) :missing)
          ((null (oauth-tokens-expires-at tokens)) :present)
          ((<= (oauth-tokens-expires-at tokens) (funcall wall-time)) :expired)
          (t :present))))
