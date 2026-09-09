;;;; file.lisp

(in-package :apprentice)


(defstruct file
  path
  content)


;;;; File Handling


(defun hidden-file-p (path)
  (let ((name (file-namestring path)))
    (and (plusp (length name))
         (char= (char name 0) #\.))))

(defun read-file-string-safe (path)
  (handler-case
      (uiop:read-file-string path :external-format :utf-8)
    (error nil)))

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
