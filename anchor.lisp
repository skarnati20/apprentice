;;;; anchor.lisp

(in-package :apprentice)


(defstruct file
  path
  content)

(defstruct anchor
  name
  bindings
  init-bindings-fn
  process-fn
  serialize-fn
  deserialize-fn)


;;;; File Handling


(defun hidden-file-p (path)
  (let ((name (file-namestring path)))
    (and (plusp (length name))
         (char= (char name 0) #\.))))

(defun read-file-string-safe (path)
  (handler-case
      (uiop:read-file-string path :external-format :utf-8)
    (error (c)
      (warn "Skipping unreadable/binary file ~A: ~A" path c)
      nil)))

(defun create-file (path)
  (let ((content (read-file-string-safe path)))
    (when content
      (make-file
       :path path
       :content content))))

(defun create-files-from-dir (dir)
  (let* ((files (remove-if #'hidden-file-p (uiop:directory-files dir)))
         (file-paths (mapcar #'uiop:native-namestring files)))
    (remove nil (mapcar #'create-file file-paths))))


;;;; Anchor Macro


(defmacro defanchor (name &key bindings process serialize deserialize)
  (let* ((var (intern (format nil "*~:@(~a~)-ANCHOR*" name)))
	 (table (gensym "TABLE"))
	 (sym-macros (loop for (sym) in bindings
			   collect `(,sym (gethash ',sym (anchor-bindings ,var))))))
    `(progn
       (defvar ,var nil)
       (setf ,var
	     (make-anchor
	      :name ,(string-downcase (symbol-name name))
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


;;;; Anchor Definitions
