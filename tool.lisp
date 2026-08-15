;;;; tool.lisp

(in-package :apprentice)


(defstruct tool
  name
  description
  schema
  fn
  (checks nil))


;;;; Tool Logic Handling

(defun run-tool-checks (tool json-response)
  (let* ((checks (tool-checks tool)))
    (loop for check in checks
          collect
          (funcall check json-response))))

(defun run-tool (tool json-response)
  (funcall (tool-fn tool)
           json-response))


;;;; Tool JSON Handling



(defun tool->openai (tool)
  "One TOOL struct as an OpenAI-format function definition."
  (j "type" "function"
     "function" (j "name" (tool-name tool)
                   "description" (tool-description tool)
                   "parameters" (tool-schema tool))))


;;;; Permission Directories



(defun resolve-path (path)
  (uiop:resolve-symlinks path))

(defun resolve-directory (dir)
  (resolve-path (uiop:ensure-directory-pathname dir)))

(defun is-parent (parent child)
  "True when CHILD resolves to a location inside PARENT. A path that
   cannot be resolved at all counts as outside: checks fail closed."
  (handler-case
      (let ((p (resolve-directory parent))
            (c (resolve-path child)))
        (when (uiop:subpathp c p) t))
    (error () nil)))

(defun is-allowed-path (dirs path)
  (and (uiop:absolute-pathname-p path)
       (some (lambda (root) (is-parent root path)) dirs)
       t))

(defun is-allowed-new-path (dirs path)
  "IS-ALLOWED-PATH for a file that need not exist yet. RESOLVE-PATH
   needs something on disk, so the containing directory is checked."
  (and (uiop:absolute-pathname-p path)
       (is-allowed-path dirs (uiop:pathname-directory-pathname path))))
                               
;;;; Tool Macros



(defmacro deftool (name description params &key checks fn)
  (let* ((name-string (string-downcase (symbol-name name)))
         (tool-var (intern (format nil "*~:@(~a~)-TOOL*" name)))
         (opt-pos (position '&optional params))
         (required (subseq params 0 opt-pos))
         (all (remove '&optional params))
         (syms (mapcar #'first all))
         (args (gensym "ARGS")))
    (labels ((bind (form)
               `(lambda (,args)
                  (declare (ignorable ,args))
                  (let ,(loop for sym in syms
                              collect `(,sym (s ,args
                                                ,(string-downcase
                                                  (symbol-name sym)))))
                    (declare (ignorable ,args))
                    ,form))))
      `(defparameter ,tool-var
         (make-tool
          :name ,name-string
          :description ,description
          :schema (j "type" "object"
                     "properties"
                     (j ,@(loop for (pname ptype pdesc) in all
                               append (list (string-downcase (symbol-name pname))
                                           `(j "type" ,(string-downcase (symbol-name ptype))
                                               "description" ,pdesc))))
                     "required"
                     (list ,@(mapcar (lambda (p) (string-downcase
                                              (symbol-name (first p))))
                                   required)))
          :fn ,(bind fn)
          :checks (list ,@(loop for (test msg) in checks
                                collect (bind `(unless ,test ,msg)))))))))


;;;; Tool Definitions


(deftool grep
    "Search for a regular expression pattern in files under a directory. Returns matching lines prefixed with file path and line number."
    ((pattern :string "The regular expression to search for")
     (path    :string "Directory or file to search in")
     &optional
     (glob    :string "Optional filename filter, e.g. *.lisp"))
  :checks (((is-allowed-path *allowed-dirs* path) "Not allowed to access this path"))
  :fn (run-argv (append (list "grep" "-rn")
                        (when glob (list "--include" glob))
                        (list "-e" pattern path))))

(deftool read
    "Read the contents of a file, with line numbers prefixed."
    ((path :string "Absolute path to the file to read")
     &optional
     (offset :integer "1-based line to start from, default 1")
     (limit  :integer "Maximum lines to read, default 2000"))
  :checks (((is-allowed-path *allowed-dirs* path) "Not allowed to access this path"))
  :fn (let ((start (or offset 1)) (n (or limit 2000)))
        (with-open-file (in path :external-format :utf-8)
          (loop for i from 1
                for line = (read-line in nil)
                while (and line (< (- i start) n))
                when (>= i start)
                  collect (format nil "~5d~a~a" i #\Tab line) into out
                finally (return (format nil "~{~a~^~%~}" out))))))

(deftool write
    "Write text to a file, creating it or overwriting it entirely."
    ((path    :string "Absolute path of the file to write")
     (content :string "Full text to write to the file"))
  :checks (((is-allowed-new-path *allowed-dirs* path) "Not allowed to write to this path"))
  :fn (progn
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
          (write-string content out))
        (format nil "Wrote ~a lines to ~a" (1+ (count #\Newline content)) path)))

(deftool bash
    "Run a shell command in the repository directory. Returns combined stdout and stderr."
    ((command :string "The shell command to run"))
  :checks (((first *allowed-dirs*) "No allowed directory is configured"))
  :fn (multiple-value-bind (out err code)
          (uiop:run-program (list "/bin/sh" "-c" command)
                            :output '(:string :stripped t)
                            :error-output '(:string :stripped t)
                            :ignore-error-status t
                            :directory (resolve-directory (first *allowed-dirs*)))
        (if (and (zerop code) (string= err ""))
            (if (string= out "") "(no output)" out)
            (format nil "~@[~a~%~]~@[~a~%~]exit status: ~a"
                    (and (string/= out "") out)
                    (and (string/= err "") err)
                    code))))

(defparameter *exa-endpoint* "https://api.exa.ai/search")

(defun format-search-result (r)
  "One Exa result as title, url, then its highlight excerpts."
  (format nil "~a~%~a~%~{~a~^~%~}"
	  (s r "title") (s r "url") (s r "highlights")))

(deftool web-search
    "Search the web using the Exa search API. Returns results with title, URL, and highlights."
    ((query :string "The search query")
     &optional
     (limit :integer "Maximum number of results to return, default 5")
     (mode  :string "Search type: auto, fast, instant, deep-lite, deep, or deep-reasoning. Default auto."))
  :checks (((uiop:getenv "EXA_API_KEY") "EXA_API_KEY environment variable is not set"))
  :fn (let* ((body (lisp-to-json-string
		    (j "query" query
		       "type" (or mode "auto")
		       "numResults" (or limit 5)
		       "contents" (j "highlights" t))))
	     (raw (run-argv (list "curl" "-s" *exa-endpoint*
				  "-H" (format nil "x-api-key: ~a"
					       (uiop:getenv "EXA_API_KEY"))
				  "-H" "Content-Type: application/json"
				  "--data-binary" "@-")
			    :input body
			    :limit most-positive-fixnum))
	     (results (s (json:decode-json-from-string raw) "results")))
	(if results
	    (truncate-output
	     (format nil "~{~a~^~%~%~}" (mapcar #'format-search-result results))
	     6000)
	    raw)))


;;;; Tool Bundles



(defparameter *standard-tools*
  (list *grep-tool* *read-tool* *write-tool* *bash-tool* *web-search-tool*))
