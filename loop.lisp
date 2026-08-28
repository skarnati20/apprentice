;;;; loop.lisp

(in-package :apprentice)


(defun dispatch-tool (name args tools)
  (let ((tl (find name tools :key #'tool-name :test #'string=)))
    (cond
      ((null tl) (format nil "Unknown tool: ~a" name))
      ((eq args :malformed)
       (format nil "Arguments to ~a were not valid JSON. Send them again ~
                    as a JSON object." name))
      (t
       (handler-case
	   (let ((failures (remove nil (run-tool-checks tl args))))
	     (if failures
		 (format nil "~{~a~^~%~}" failures)
		 (run-tool tl args)))
	 (error (e) (format nil "Tool ~a failed: ~a" name e)))))))

(defun run-calls (calls tools)
  "Alist of call id to output. One turn's results travel together:
   some providers require them batched into a single message."
  (loop for call in calls
	for name   = (tool-call-name call)
	for args   = (tool-call-args call)
	for result = (dispatch-tool name args tools)
	do (format t "~&→ ~a ~s~%~a~%" name args result)
	collect (cons (tool-call-id call) result)))

(defparameter *loop-keys*
  '(:model :system-prompt :system :tools :max-turns :history)
  "Keys the loops consume themselves. CHECK-OPTIONS rejects any option
   the model does not declare, so these must not reach it.")

(defun model-options (options &rest also)
  "OPTIONS with the loops' own keys, and any in ALSO, removed."
  (loop for (k v) on options by #'cddr
	unless (or (member k *loop-keys*) (member k also))
	  append (list k v)))

(defun seed-messages (system prompt history)
  (if history
      (append history (list (list :user prompt)))
      (list (list :system system) (list :user prompt))))


;;;; Standard Agent Loop


(defparameter *standard-prompt*
  "You are a coding agent. Use tools to inspect files before answering. Always use absolute paths.")

(defun standard-loop (prompt &rest options
		      &key (model *model*)
			   (system-prompt *standard-prompt*)
			   (tools *standard-tools*)
			   (max-turns 15)
			   (history nil)
		      &allow-other-keys)
  (let ((opts (model-options options))
	(msgs (seed-messages system-prompt prompt history)))
    (loop repeat max-turns do
      (multiple-value-bind (content calls stop msg)
	  (apply #'run-model model msgs tools opts)
	(when (eq stop :error)
	  (return (list content msgs)))
	(setf msgs (append msgs (list msg)))
	(if calls
	    (setf msgs (append msgs (list (list :tool-results
						(run-calls calls tools)))))
	    (return (list content msgs))))
	  finally (return (list (format nil "[stopped: hit max-turns (~a)]" max-turns)
				msgs)))))


;;;; Little Coder Agent Loop
;;;;
;;;; NOTE: Implementation based on https://github.com/itayinbarr/little-coder


(defparameter *little-coder-prompt*
  *standard-prompt*)

(defun little-coder-loop (prompt &rest options
			  &key (model *model*)
			       (system *little-coder-prompt*)
			       (tools *little-coder-tools*)
			       (max-turns 20)
			       (history nil)
			  &allow-other-keys)
  (let ((opts (model-options options :thinking))
	(msgs (seed-messages system prompt history)))
    (loop repeat max-turns do
      (multiple-value-bind (content calls stop msg)
	  (apply #'run-model model msgs tools :thinking t opts)
	(when (eq stop :error)
	  (return (list content msgs)))
	(setf msgs (append msgs (list msg)))
	(cond
	  ;; Retry with thinking off, keeping the partial trace in
	  ;; context: the model keeps its work but must now commit.
	  ((eq stop :overflow)
	   (format t "~&⋯ deliberation overflowed, retrying with thinking disabled~%")
	   (multiple-value-bind (content2 calls2 stop2 msg2)
	       (apply #'run-model model msgs tools :thinking nil opts)
	     (when (eq stop2 :error)
	       (return (list content2 msgs)))
	     (setf msgs (append msgs (list msg2)))
	     (if calls2
		 (setf msgs (append msgs (list (list :tool-results
						     (run-calls calls2 tools)))))
		 (return (list content2 msgs)))))
	  (calls
	   (setf msgs (append msgs (list (list :tool-results
					       (run-calls calls tools))))))
	  (t (return (list content msgs)))))
	  finally (return (list (format nil "[stopped: hit max-turns (~a)]" max-turns)
				msgs)))))
