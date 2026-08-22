;;;; apprentice.lisp

(in-package :apprentice)


(defvar *chat-history* nil)
(defvar *allowed-dirs* nil)


(defun resolve-loop (loop-symbol)
  "Returns the appropriate loop function symbol or nil if unknown."
  (ecase loop-symbol
    (:little-coder 'little-coder-loop)
    (:standard 'standard-loop)
    (:default 'standard-loop)
    (t nil)))

(defun chat (prompt &optional loop-symbol)
  "CHAT runs a conversation using the specified loop.
   
  LOOP-SYMBOL can be one of:
    :little-coder - use little-coder-loop
    :standard - use standard-loop
    :default - use standard-loop (default)
    nil or t - use standard-loop (default)"
  (let ((loop-fn (resolve-loop loop-symbol)))
    (if loop-fn
        (destructuring-bind (content msgs)
            (funcall loop-fn prompt)
          (setf *chat-history* msgs)
          (format t "~a" content))
      (error "Unknown loop: ~a" loop-symbol))))

(defun add-allowed-dir (dir)
  (push dir *allowed-dirs*))

(defun clear-allowed-dirs ()
  (setf *allowed-dirs* nil))

(defun clear ()
  (setf *chat-history* nil))
