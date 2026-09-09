;;;; anchor.lisp

(in-package :apprentice)


(defstruct anchor
  name
  description
  bindings
  init-bindings-fn
  process-fn
  serialize-fn
  deserialize-fn)


;;;; Anchor Macro


(defmacro defanchor (name description &key bindings process serialize deserialize)
  (let* ((var (intern (format nil "*~:@(~a~)-ANCHOR*" name)))
	 (table (gensym "TABLE"))
	 (sym-macros (loop for (sym) in bindings
			   collect `(,sym (gethash ',sym (anchor-bindings ,var))))))
    `(progn
       (defvar ,var nil)
       (setf ,var
	     (make-anchor
	      :name ,(string-downcase (symbol-name name))
	      :description ,description
	      :init-bindings-fn
	      (lambda ()
		(let ((,table (make-hash-table :test 'eq)))
		  ,@(loop for (sym init) in bindings
			  collect `(setf (gethash ',sym ,table) ,init))
		  ,table))
	      :process-fn     (symbol-macrolet ,sym-macros ,process)
	      :serialize-fn   (symbol-macrolet ,sym-macros ,serialize)
	      :deserialize-fn (symbol-macrolet ,sym-macros ,deserialize)))
       (setf (anchor-bindings ,var)
	     (funcall (anchor-init-bindings-fn ,var)))
       ,var)))


;;;; Anchor Operations


(defun process-anchor (anchor files)
  (let ((process-fn (anchor-process-fn anchor)))
    (funcall process-fn files)))
    

(defun process-dir (anchors dir)
  (let ((files (create-files-from-dir dir)))
    (loop for anchor in anchors do
	  (process-anchor anchor files))))


;;;; Anchor Helpers


(defvar *apprentice-folder-name*
  ".apprentice/")

(defun apprentice-folder (dir)
  (uiop:subpathname (expand-dir dir) *apprentice-folder-name*))

(defun create-apprentice-folder (dir)
  (let ((path (apprentice-folder dir)))
    (ensure-directories-exist path)
    path))

(defun save-anchors (&optional (dir *anchor-dir*))
  (when (and dir *anchors*)
    (let ((folder (create-apprentice-folder dir)))
      (dolist (anchor *anchors*)
	(funcall (anchor-serialize-fn anchor) folder))
      folder)))

(defun load-anchor (anchor dir)
  (setf (anchor-bindings anchor)
	(funcall (anchor-init-bindings-fn anchor)))
  (let ((folder (apprentice-folder dir)))
    (when (uiop:directory-exists-p folder)
      (handler-case (funcall (anchor-deserialize-fn anchor) folder)
	(error (e)
	  (warn "Anchor ~a: could not load state from ~a (~a). Using defaults."
		(anchor-name anchor) folder e)))))
  anchor)


;;;; Anchor Definitions


(defvar *dense-vector-store-name* "dense-vector-search.sexp")

(defanchor dense-vector-search
  "A Dense-Vector embedding anchor which chunks each file's text and computes
   an embedding. Files are hashed by content, so only what changed is
   re-embedded. Serialized and deserialized through saved files."
  :bindings ((entries nil)                            ; (CHUNK . EMBEDDING) pairs
	     (hashes (make-hash-table :test 'equal))) ; path -> content SXHASH
  :process
  (lambda (files)
    (let* ((present (mapcar #'file-path files))
	   (changed (remove-if (lambda (f)
				 (eql (sxhash (file-content f))
				      (gethash (file-path f) hashes)))
			       files)))
      ;; Drop unchanged or removed entries
      (setf entries
	    (remove-if (lambda (e)
			 (let ((path (chunk-file-path (car e))))
			   (or (not (member path present :test #'equal))
			       (member path changed :key #'file-path :test #'equal))))
		       entries))
      (dolist (gone (loop for path being the hash-keys of hashes
			  unless (member path present :test #'equal)
			    collect path))
	(remhash gone hashes))
      ;; Embed every changed file's chunks in one request.
      (when changed
	(let* ((chunks (loop for f in changed append (split-file-into-chunks f)))
	       (vectors (mapcar #'create-embedding (mapcar #'chunk-text chunks))))
	  (when vectors
	    (setf entries (append entries (mapcar #'cons chunks vectors)))
	    (dolist (f changed)
	      (setf (gethash (file-path f) hashes)
		    (sxhash (file-content f)))))))
      (list :files (length files)
	    :changed (length changed)
	    :chunks (length entries))))
  :serialize
  (lambda (folder)
    (with-open-file (out (merge-pathnames *dense-vector-store-name* folder)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (prin1 (list :version 1
		   :hashes (loop for path being the hash-keys of hashes
				   using (hash-value h)
				 collect (cons path h))
		   :entries (loop for (chunk . vec) in entries
				  collect (list (chunk-file-path chunk)
						(chunk-text chunk)
						(chunk-start-offset chunk)
						(chunk-end-offset chunk)
						(coerce vec 'list))))
	     out)))
  :deserialize
  (lambda (folder)
    (let ((path (merge-pathnames *dense-vector-store-name* folder)))
      (when (probe-file path)
	(let ((data (with-open-file (in path) (read in))))
	  (dolist (pair (getf data :hashes))
	    (setf (gethash (car pair) hashes) (cdr pair)))
	  (setf entries
		(loop for (file-path text start end floats) in (getf data :entries)
		      collect (cons (make-chunk :file-path file-path
						:text text
						:start-offset start
						:end-offset end)
				    (to-embedding floats)))))))))

