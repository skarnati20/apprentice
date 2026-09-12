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


;;;; Bash Permissions


(defparameter *bash-whitelist*
  '("ls" "cat" "head" "tail" "wc" "pwd" "echo" "printf" "date"
    "which" "type" "env" "printenv" "uname" "whoami" "id"
    "git log" "git status" "git diff" "git show" "git branch"
    "git remote" "git stash list" "git tag"
    "find " "grep " "rg " "ag " "fd " "sed "
    "python " "python3 " "node " "ruby " "perl "
    "pip show" "pip list" "npm list" "cargo metadata"
    "df " "du " "free " "top -bn" "ps "
    "curl -I" "curl --head"
    ;; Routine filesystem scaffolding. Trailing space = word boundary, so
    ;; "cp " matches "cp a b" but not "cpufetch". rm stays off the list by
    ;; design; use LITTLE_CODER_BASH_ALLOW=rm if a deployment needs it.
    "cp " "mv " "mkdir " "touch ")
  "Command prefixes permitted without confirmation.")

(defun is-allowed-bash (command)
  (some (lambda (x) (eq x 't))
		  (mapcar
		   (lambda (x) (starts-with-p command x))
		   *bash-whitelist*)))

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
                     ;; Empty alists encode as null, but a schema needs {} and [].
                     "properties"
                     (or (j ,@(loop for (pname ptype pdesc) in all
                               append (list (string-downcase (symbol-name pname))
                                           `(j "type" ,(string-downcase (symbol-name ptype))
                                               "description" ,pdesc))))
                         (make-hash-table))
                     "required"
                     (vector ,@(mapcar (lambda (p) (string-downcase
                                              (symbol-name (first p))))
                                   required)))
          :fn ,(bind fn)
          :checks (list ,@(loop for (test msg) in checks
                                collect (bind `(unless ,test ,msg)))))))))


;;;; Standard Tool Definitions


(deftool grep
    "Search for a regular expression pattern in files under a directory. Returns matching lines prefixed with file path and line number."
    ((pattern :string "The regular expression to search for")
     (path    :string "Directory or file to search in")
     &optional
     (glob    :string "Optional filename filter, e.g. *.lisp"))
  :checks (((is-allowed-path *allowed-dirs* path)
	    (format nil "Not allowed to access this path. Allowed dirs: ~a"
		    (format nil "~{~A~^, ~}" *allowed-dirs*))))
  :fn (run-argv (append (list "grep" "-rn")
                        (when glob (list "--include" glob))
                        (list "-e" pattern path))
                :empty "(no matches found)"))

(deftool read
    "Read the contents of a file, with line numbers prefixed."
    ((path :string "Absolute path to the file to read")
     &optional
     (offset :integer "1-based line to start from, default 1")
     (limit  :integer "Maximum lines to read, default 2000"))
  :checks (((is-allowed-path *allowed-dirs* path)
	    (format nil "Not allowed to access this path. Allowed dirs: ~a"
		     (format nil "~{~A~^, ~}" *allowed-dirs*))))
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
  :checks (((is-allowed-new-path *allowed-dirs* path)
	    (format nil "Not allowed to edit at this path. Allowed dirs: ~a"
		     (format nil "~{~A~^, ~}" *allowed-dirs*))))
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


;;;; Web Tools


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


;;;; Little Coder Tools


(deftool little-coder-write
  "Write text to a new file. Does not work with existing files. To edit, use an edit tool."
  ((path    :string "Absolute path of the file to write")
   (content :string "Full text to write to the file"))
  :checks (((is-allowed-new-path *allowed-dirs* path)
	    (format nil "Not allowed to edit at this path. Allowed dirs: ~a"
		     (format nil "~{~A~^, ~}" *allowed-dirs*)))
	   ((not (uiop:file-exists-p path)) "Not allowed to write to existing file. Use an edit tool instead."))
  :fn (progn
	(with-open-file (out path :direction :output
				  :if-exists :error
				  :if-does-not-exist :create :external-format :utf-8)
	  (write-string content out))
	(format nil "Wrote ~a lines to ~a" (1+ (count #\Newline content)) path)))

(deftool little-coder-edit
  "Edits text of an existing file. To write a new file, use the write tool."
  ((path         :string "Absolute path of the file to write")
   (start-offset :integer "First character in the file to overwrite")
   (end-offset   :integer "Last character in the file to overwrite")
   (content      :string "Content to insert when overwriting file section between START-LINE and END-LINE"))
  :checks (((is-allowed-path *allowed-dirs* path)
	    (format nil "Not allowed to edit at this path. Allowed dirs: ~a"
		     (format nil "~{~A~^, ~}" *allowed-dirs*)))
	   ((uiop:file-exists-p path)
	    "File does not exist. To write a new file, use the write tool.")
	   ((<= start-offset end-offset) "END-OFFSET cannot be less than START-OFFSET"))
  :fn (let* ((original (uiop:read-file-string path :external-format :utf-8))
           (len (length original)))
      (cond
        ((> start-offset len)
         (format nil "START-OFFSET ~a is past end of file (~a characters)"
                 start-offset len))
        ((> end-offset len)
         (format nil "END-OFFSET ~a is past end of file (~a characters)"
                 end-offset len))
        (t
         (let ((new (concatenate 'string
                                 (subseq original 0 start-offset)
                                 content
                                 (subseq original end-offset))))
           (with-open-file (out path :direction :output
                                     :if-exists :supersede
                                     :external-format :utf-8)
             (write-string new out))
           (format nil "Replaced ~a characters with ~a characters in ~a"
                   (- end-offset start-offset) (length content) path))))))

(deftool little-coder-bash
    "Run a shell command in the repository directory. Returns combined stdout and stderr."
    ((command :string "The shell command to run"))
  :checks (((first *allowed-dirs*) "No allowed directory is configured")
	   ((is-allowed-bash command)
	    (format nil "Not an allowed command. These are allowed commands: ~a"
		    (format nil "~{~A~^, ~}" *bash-whitelist*))))
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


;;;; Anchor Tools


(defun offset->line (path offset)
  "The 1-based line OFFSET falls on in PATH, or NIL if unreadable."
  (let ((text (read-file-string-safe path)))
    (when text
      (1+ (count #\Newline text :end (min offset (length text)))))))

(defun format-chunk-result (result)
  "One (SCORE . CHUNK) search hit as path:line with its score, then the text."
  (destructuring-bind (score . chunk) result
    (let* ((path (chunk-file-path chunk))
	   (line (offset->line path (chunk-start-offset chunk))))
      (format nil "~a:~a (similarity ~,2f)~%~a"
	      path
	      (or line (format nil "char ~a" (chunk-start-offset chunk)))
	      score
	      (chunk-text chunk)))))

(defun path-relative-parts (path root)
  "PATH split into its component strings, relative to ROOT when PATH
   falls under it, otherwise as an absolute list of parts."
  (let* ((rel (or (ignore-errors (uiop:enough-pathname path root)) path))
	 (namestring (uiop:native-namestring rel)))
    (remove "" (uiop:split-string namestring :separator "/\\")
	    :test #'string=)))

(defun add-path-to-tree (tree parts)
  "TREE is an alist of (name . subtree), subtree NIL for files. Inserts
   PARTS (a list of path components) into it, returning the new alist."
  (if (null parts)
      tree
      (let* ((name (first parts))
	     (rest (rest parts))
	     (entry (assoc name tree :test #'string=)))
	(if entry
	    (progn
	      (setf (cdr entry) (add-path-to-tree (cdr entry) rest))
	      tree)
	    (append tree (list (cons name (add-path-to-tree nil rest))))))))

(defun paths-to-tree (paths root)
  "An alist tree (see ADD-PATH-TO-TREE) built from PATHS, made relative
   to ROOT when possible."
  (let ((tree nil))
    (dolist (path paths tree)
      (setf tree (add-path-to-tree tree (path-relative-parts path root))))))

(defun format-tree (tree &optional (prefix ""))
  "TREE (see PATHS-TO-TREE) as a directory-listing string, using the
   usual box-drawing branches."
  (with-output-to-string (out)
    (loop for (entry . rest) on tree
	  for name = (car entry)
	  for subtree = (cdr entry)
	  for last = (null rest)
	  do (format out "~a~a~a~%" prefix (if last "└── " "├── ") name)
	     (when subtree
	       (write-string
		(format-tree subtree (concatenate 'string prefix (if last "    " "│   ")))
		out)))))

(deftool file-tree
  "Show the directory structure of an indexed directory as a tree, without touching disk again. Tool only works if the file-tree anchor is enabled for this directory."
    ()
  :checks (((gethash 'paths (anchor-bindings *file-tree-anchor*))
	    "The file tree index is empty or not enabled for this directory."))
  :fn (let* ((paths (gethash 'paths (anchor-bindings *file-tree-anchor*)))
	     (root (or *anchor-dir* ""))
	     (tree (paths-to-tree paths root)))
	(truncate-output (format-tree tree) 6000)))

(deftool dense-vector-search
  "Search indexed files by meaning rather than exact text. Returns the most relevant chunks
   as path with a similarity score. Use it when you do not know the exact name or wording to
   grep for. Tool only works if an anchor directory is defined."
    ((query :string "What to look for, described in plain language")
     &optional
     (limit :integer "Maximum number of passages to return, default 5"))
  :checks (((gethash 'entries (anchor-bindings *dense-vector-search-anchor*))
	    "The dense vector index is empty or not enabled for this directory."))
  :fn (let* ((entries (gethash 'entries (anchor-bindings *dense-vector-search-anchor*)))
	     (query-vec (create-embedding query))
	     (scored (sort (mapcar (lambda (e)
				     (cons (dot-product query-vec (cdr e))
					   (car e)))
				   entries)
			   #'> :key #'car))
	     (top (subseq scored 0 (min (or limit 5) (length scored)))))
	(truncate-output
	 (format nil "~{~a~^~%~%~}" (mapcar #'format-chunk-result top))
	 6000)))


;;;; Sub-Agent Tools


(defparameter *subagent-loop* :little-coder
  "Which loop the subagent runs.")

(defparameter *subagent-max-turns* 20)

(defparameter *subagent-prompt*
  "You are a subagent. Another agent, which cannot read or change files itself, has delegated one task to you. Do it with your tools, always using absolute paths. When you are finished, reply with a concise, self-contained report: what you found or changed, with file paths and line numbers, quoting the relevant code when the task asks about it. The other agent sees only this final reply, never your tool calls or their output.")

(defparameter *subagent-report-limit* 6000
  "Characters of a subagent's report the SUBAGENT tool passes back.")

(defparameter *subagent-brief-limit* 1200
  "The same, for SUBAGENT-BRIEF. Small on purpose: a loop that delegates
   constantly keeps every report it gets back in context for the rest of
   the run.")

(defparameter *subagent-tool-names* '("subagent" "subagent-brief")
  "Withheld from a subagent, so one cannot delegate further.")

(defun run-subagent (task limit)
  "TASK run on the subagent model, its report cut to LIMIT characters."
  (let ((tools (remove-if (lambda (tl)
			    (member (tool-name tl) *subagent-tool-names*
				    :test #'string=))
			  *subagent-tools*)))
    (format t "~&⇢ subagent (~a, ~(~a~) loop): ~a~%"
	    (model-name *subagent-model*) *subagent-loop* task)
    (destructuring-bind (content msgs)
	(funcall (resolve-loop *subagent-loop*) task
		 :model *subagent-model*
		 :system-prompt *subagent-prompt*
		 :system *subagent-prompt*
		 :tools tools
		 :max-turns *subagent-max-turns*)
      (declare (ignore msgs))
      (truncate-output (or content "(the subagent returned no report)")
		       limit))))

(deftool subagent
    "Delegate a task to a subagent that can read, write and edit files and run shell commands. It starts with no memory of this conversation, so give it everything it needs: absolute paths, exactly what to look for or change, and what to report back. Returns the subagent's final report."
    ((task :string "The complete, self-contained instruction for the subagent"))
  :checks ((*subagent-model* "No subagent model is configured.")
	   ((resolve-loop *subagent-loop*)
	    (format nil "Unknown subagent loop ~s." *subagent-loop*)))
  :fn (run-subagent task *subagent-report-limit*))

(deftool subagent-brief
    "Delegate a task to a subagent that can read, write and edit files and run shell commands. It starts with no memory of this conversation, so give it everything it needs: absolute paths, exactly what to look for or change, and what to report back. Its reply is cut short after a small number of characters, so ask it for a brief report -- findings and evidence only, no narration -- or the end of its answer is lost."
    ((task :string "The complete, self-contained instruction for the subagent"))
  :checks ((*subagent-model* "No subagent model is configured.")
	   ((resolve-loop *subagent-loop*)
	    (format nil "Unknown subagent loop ~s." *subagent-loop*)))
  :fn (run-subagent task *subagent-brief-limit*))


;;;; Tool Bundles

(defparameter *standard-tools*
  (list *grep-tool* *read-tool* *write-tool* *bash-tool* *web-search-tool*
	*subagent-tool*))

(defparameter *little-coder-tools*
  (list *grep-tool* *read-tool* *little-coder-write-tool* *little-coder-edit-tool*
	*little-coder-bash-tool* *web-search-tool*))

(defparameter *subagent-tools*
  (substitute *bash-tool* *little-coder-bash-tool* *little-coder-tools*)
  "The little-coder tools, but with unrestricted bash in place of the
   whitelisted one, so a subagent can run any shell command. Declared in
   state.lisp, since RUN-SUBAGENT reads it above.")

(defparameter *apprentice-tools*
  (list *grep-tool* *web-search-tool* *dense-vector-search-tool* *file-tree-tool*
	*subagent-brief-tool*))
