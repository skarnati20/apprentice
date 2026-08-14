;;;; loop.lisp

(in-package :apprentice)


(defun send (msgs tools)
  (run-llama-cpp-chat-raw msgs :tools tools))

(defun parse-response (json-output)
  (let* ((choice (first (cdr (assoc :choices json-output))))
	 (msg (cdr (assoc :message choice)))
	 (finish-reason (cdr (assoc :finish--reason choice)))
	 (content (cdr (assoc :content msg)))
	 (tool-calls (cdr (assoc :tool--calls msg))))
    (values content
	    tool-calls
	    finish-reason
	    msg)))

(defun call-name (call)
  (cdr (assoc :name (cdr (assoc :function call)))))

(defun call-id (call)
  (cdr (assoc :id call)))

(defun call-args (call)
  (json:decode-json-from-string
   (cdr (assoc :arguments (cdr (assoc :function call))))))

(defun dispatch-tool (name args tools)
  (let ((tl (find name tools :key #'tool-name :test #'string=)))
    (if tl
	(handler-case
	    (let ((failures (remove nil (run-tool-checks tl args))))
	      (if failures
		  (format nil "~{~a~^~%~}" failures)
		  (run-tool tl args)))
	  (error (e) (format nil "Tool ~a failed: ~a" name e)))
	(format nil "Unknown tool: ~a" name))))

(defun format-tool-result (call-id result)
  (list (cons :|role| "tool")
        (cons :|tool_call_id| call-id)
        (cons :|content| result)))

(defparameter *system-prompt*
  "You are a coding agent. Use tools to inspect files before answering. Always use absolute paths.")

(defun run-agent (prompt &key (system-prompt *system-prompt*) (tools nil) (max-turns 15) (history nil))
  (let* ((formatted-system-prompt (format-message system-prompt :system))
	 (user-prompt (format-message prompt :user))
	 (msgs (if history
		   (append history (list user-prompt))
		   (list formatted-system-prompt user-prompt))))
    (loop repeat max-turns do
      (multiple-value-bind (content calls finish msg)
	  (parse-response (send msgs tools))
	(declare (ignore finish))
	(setf msgs (append msgs (list msg)))
	(if calls
	    (dolist (call calls)
	      (let* ((name (call-name call))
		     (id (call-id call))
		     (args (call-args call))
		     (result (dispatch-tool name args tools))
		     (formatted-result (format-tool-result id result)))
	        (format t "~&→ ~a ~s~%~a~%" name args result)
		(setf msgs (append msgs (list formatted-result)))))
	    (return (list content msgs))))
	  finally (return (list (format nil "[stopped: hit max-turns (~a)]" max-turns)
				msgs)))))
