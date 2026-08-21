;;;; loop.lisp

(in-package :apprentice)


(defun send (msgs tools &rest options)
  (apply #'run-llama-cpp-chat-raw msgs :tools tools options))

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


;;;; Standard Agent Loop


(defparameter *standard-prompt*
  "You are a coding agent. Use tools to inspect files before answering. Always use absolute paths.")

(defun standard-loop (prompt &rest options &key (system-prompt *standard-prompt*) (tools *standard-tools*) (max-turns 15) (history nil) &allow-other-keys)
  (let* ((formatted-system-prompt (format-message system-prompt :system))
	 (user-prompt (format-message prompt :user))
	 (msgs (if history
		   (append history (list user-prompt))
		   (list formatted-system-prompt user-prompt))))
    (loop repeat max-turns do
      (multiple-value-bind (content calls finish msg)
	  (parse-response
	   (apply #'send msgs tools options))
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


;;;; Little Coder Agent Loop
;;;;
;;;; NOTE: Implementation based on https://github.com/itayinbarr/little-coder


(defparameter *little-coder-prompt*
  *standard-prompt*)

(defun without-option (key options)
  "OPTIONS with KEY and its value removed, so a caller-supplied value
   cannot collide with one the loop sets itself. Duplicate keys would
   both reach FORMAT-OPTIONS and both land in the request JSON."
  (loop for (k v) on options by #'cddr
	unless (eq k key)
	  append (list k v)))

(defun deliberation-overflow-p (finish-reason)
  "True when generation stopped because the model ran out of room to
   think rather than finishing on its own."
  (equal finish-reason "length"))

(defun little-coder-loop (prompt &rest options &key (system *little-coder-prompt*) (tools *little-coder-tools*) (max-turns 20) (history nil) &allow-other-keys)
  (let* ((formatted-system-prompt (format-message system :system))
	 (user-prompt (format-message prompt :user))
	 (rest-options (without-option :thinking options))
	 (msgs (if history
		   (append history (list user-prompt))
		   (list formatted-system-prompt user-prompt))))
    (loop repeat max-turns do
      (multiple-value-bind (content calls finish msg)
	  (parse-response
	   (apply #'send msgs tools :thinking t rest-options))
	(if (not (deliberation-overflow-p finish))
	    (progn
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
	    ;; Deliberation overflowed. Keep the partial trace in context so
	    ;; the work is not lost, then retry the same turn with thinking
	    ;; off.
	    (progn
	      (format t "~&⋯ deliberation overflowed, retrying with thinking disabled~%")
	      (setf msgs (append msgs (list msg)))
	      (multiple-value-bind (content2 calls2 finish2 msg2)
		  (parse-response
		   (apply #'send msgs tools :thinking nil rest-options))
		(declare (ignore finish2))
		(setf msgs (append msgs (list msg2)))
		(if calls2
		    (dolist (call calls2)
		      (let* ((name (call-name call))
			     (id (call-id call))
			     (args (call-args call))
			     (result (dispatch-tool name args tools))
			     (formatted-result (format-tool-result id result)))
			(format t "~&→ ~a ~s~%~a~%" name args result)
			(setf msgs (append msgs (list formatted-result)))))
		    (return (list content2 msgs)))))))
	  finally (return (list (format nil "[stopped: hit max-turns (~a)]" max-turns)
				msgs)))))
