;;;; helpers.lisp

(in-package #:apprentice)


;;;; Strings


(defun starts-with-p (string prefix)
  "True when STRING begins with PREFIX."
  (let ((end (length prefix)))
    (and (<= end (length string))
         (string= prefix string :end2 end))))

(defun substitute-subseq (string old new &key (test #'eql))
  "Replace every occurrence of OLD in STRING with NEW."
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (substitute-subseq (subseq string (+ pos (length old)))
                                        old new :test test))
        string)))

(defun count-subseq (needle haystack)
  "How many non-overlapping times NEEDLE occurs in HAYSTACK. An empty
   NEEDLE counts as zero rather than looping forever."
  (if (zerop (length needle))
      0
      (loop with count = 0
	    with start = 0
	    for pos = (search needle haystack :start2 start)
	    while pos
	    do (incf count)
	       (setf start (+ pos (length needle)))
	    finally (return count))))

(defun one-line (string)
  "STRING with every run of whitespace collapsed to a single space, so a
   multi-line value can sit in one row of a listing."
  (string-right-trim
   " "
   (with-output-to-string (out)
     (let ((in-space t))
       (loop for ch across string
	     do (if (member ch '(#\Space #\Tab #\Newline #\Return #\Page))
		    (unless in-space
		      (write-char #\Space out)
		      (setf in-space t))
		    (progn (write-char ch out)
			   (setf in-space nil))))))))

(defun ellipsize (string limit)
  "STRING cut to LIMIT characters, saying how many were left off."
  (if (<= (length string) limit)
      string
      (format nil "~a... (+~a chars)"
	      (subseq string 0 limit) (- (length string) limit))))


;;;; Lines


(defun file-lines (string)
  "Splits STRING into lines by new-line."
  (let ((parts (uiop:split-string string :separator (list #\Newline))))
    (if (and (> (length parts) 1)
	     (string= (car (last parts)) ""))
	(values (butlast parts) t)
	(values parts nil))))

(defun numbered-window (lines from to)
  "LINES from FROM to TO, inclusive and 1-based, numbered exactly as the
   READ tool numbers them so the two can be read side by side."
  (let* ((n  (length lines))
	 (lo (max 1 from))
	 (hi (min to n)))
    (if (> lo hi)
	""
	(format nil "~{~a~^~%~}"
		(loop for line in (subseq lines (1- lo) hi)
		      for i from lo
		      collect (format nil "~5d~a~a" i #\Tab line))))))


;;;; Paths


(defun expand-dir (dir)
  (let ((path (uiop:ensure-absolute-pathname
	       (uiop:ensure-directory-pathname dir)
	       #'uiop:getcwd)))
    (uiop:native-namestring
     (or (ignore-errors (uiop:resolve-symlinks path)) path))))

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


;;;; Path Trees


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


;;;; Chunks


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


;;;; Index Specs


(defun expand-index-specs (specs n)
  "The indices named by SPECS over a sequence of N items, sorted and
   without duplicates. A spec is an index, or an inclusive range written
   (LO . HI) or (LO HI). A negative index counts from the end, so -1 is
   the last item and (-2 . -1) the last two. Indices outside the sequence
   are ignored rather than signalling, so a stale index left over from an
   earlier listing cannot error or wrap around."
  (let ((out nil))
    (dolist (spec specs)
      (multiple-value-bind (lo hi)
	  (cond ((integerp spec) (values spec spec))
		((and (consp spec) (integerp (car spec)) (integerp (cdr spec)))
		 (values (car spec) (cdr spec)))
		((and (consp spec) (integerp (car spec))
		      (consp (cdr spec)) (integerp (second spec)))
		 (values (first spec) (second spec)))
		(t (error "Not a turn index or range: ~s" spec)))
	(let ((lo (if (minusp lo) (+ n lo) lo))
	      (hi (if (minusp hi) (+ n hi) hi)))
	  (loop for i from (max 0 lo) to (min hi (1- n))
		do (pushnew i out)))))
    (sort out #'<)))


;;;; JSON Encoding


(defmethod json:encode-json ((x (eql :false)) &optional stream)
  "CL-JSON maps NIL to null, and a NIL alist value collapses into a
   one-element list encoding as an array, so false needs a marker."
  (write-string "false" stream))

(defun lisp-to-json-string (data)
  "Key symbols take cl-json's usual mapping, which round-trips decoded
   messages: :ROLE back to role, :REASONING--CONTENT to reasoning_content."
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun lisp-to-verbatim-json-string (data)
  "Key symbols emitted exactly as named, for APIs wanting literal
   camelCase: the default encoder downcases numResults. Only safe for
   hand-built alists, never for anything cl-json decoded."
  (with-output-to-string (s)
    (let ((json:*lisp-identifier-name-to-json* #'string))
      (json:encode-json data s))))

(defun lisp-to-corrected-json-string (data)
  (substitute-subseq
   (lisp-to-json-string data)
   ":null"
   ":false"
   :test #'string=))

(defun alist-p (x)
  (and (consp x) (consp (first x)) (atom (car (first x)))))

(defun json-key-name (symbol)
  "SYMBOL as the JSON key it was decoded from. CL-JSON maps an
   underscore to a double dash, so \"start_offset\" arrives as
   :START--OFFSET; undoing that before single dashes keeps it from
   coming back as start__offset."
  (substitute #\_ #\-
	      (substitute-subseq (string-downcase (symbol-name symbol))
				 "--" "_")))

(defun args-object (args)
  "A decoded arguments alist as a hash table, which always encodes as a
   JSON object."
  (let ((table (make-hash-table :test #'equal)))
    (when (listp args)
      (dolist (pair args)
	(when (consp pair)
	  (setf (gethash (json-key-name (car pair)) table)
		(cdr pair)))))
    table))


;;;; JSON Access


(defun j (&rest plist)
  "Function to format pairs (for JSON)."
  (loop for (k v) on plist by #'cddr
        collect (cons (intern k :keyword) v)))

(defun s (obj &rest keys)
  "Walks OBJ down through list of KEYS. Returns value at end of walk,
   otherwise NIL. Underscores in a KEY are translated: CL-JSON decodes
   \"tool_calls\" to :TOOL--CALLS."
  (let ((curr obj))
    (dolist (k keys curr)
      (unless (consp curr) (return nil))
      (let ((want (substitute-subseq k "_" "--")))
	(setf curr
	      (loop for tail = curr then (cdr tail)
		    while (consp tail)
		    for pair = (car tail)
		    when (and (consp pair)
			      (symbolp (car pair))
			      (string-equal want (symbol-name (car pair))))
		      return (cdr pair)))))))


;;;; Command Running


(defun truncate-output (s limit)
  "Cap S at LIMIT characters, telling the reader it was cut."
  (cond ((null limit) s)
        ((null s) "")
        ((<= (length s) limit) s)
        (t (format nil "~a~%~%[truncated — showing ~a of ~a characters]"
                   (subseq s 0 limit) limit (length s)))))

(defun run-argv (argv &key directory input (limit 6000) (ok-codes '(0 1))
			(empty "(no output)"))
  "EMPTY is returned when the command succeeds but prints nothing, so a
   caller never gets back the empty string."
  (handler-case
      (multiple-value-bind (out err code)
	  (uiop:run-program argv
                            :output :string
                            :error-output :string
                            :directory directory
                            :input (when input (make-string-input-stream input))
                            :ignore-error-status t)
	(if (member code ok-codes)
            (let ((text (truncate-output out limit)))
              (if (string= text "") empty text))
            (format nil "Command failed (exit ~a): ~a"
                    code (truncate-output err 500))))
    (error (e)
      (format nil "Could not run command: ~a" e))))


;;;; Raw String Reader Macro


(defun raw-string-reader (stream subchar arg)
  "Reads a raw string delimited by #\" and \"#, preserving unescaped quotes."
  (declare (ignore subchar arg))
  (with-output-to-string (out)
    (loop for c = (read-char stream t nil t)
          do (if (eql c #\")
                 (let ((next (peek-char nil stream nil nil t)))
                   (if (eql next #\#)
                       (progn (read-char stream t nil t) (return))
                       (write-char c out)))
                 (write-char c out)))))

(set-dispatch-macro-character #\# #\" #'raw-string-reader)
