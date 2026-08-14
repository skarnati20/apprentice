;;;; apprentice.lisp

(in-package :apprentice)


(defvar *chat-history* nil)
(defparameter *allowed-dirs* nil)


(defun chat (prompt)
  (destructuring-bind (content msgs)
      (run-agent prompt :tools *standard-tools* :history *chat-history*)
    (setf *chat-history* msgs)
    (format t "~a" content)))

(defun add-allowed-dir (dir)
  (push dir *allowed-dirs*))

(defun clear-allowed-dirs ()
  (setf *allowed-dirs* nil))

(defun clear ()
  (setf *chat-history* nil))
