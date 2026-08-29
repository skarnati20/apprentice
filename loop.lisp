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
  "List of (ID NAME OUTPUT) per call. One turn's results travel
   together: some providers require them batched into a single message,
   and some need the function name alongside the id."
  (loop for call in calls
	for name   = (tool-call-name call)
	for args   = (tool-call-args call)
	for result = (dispatch-tool name args tools)
	do (format t "~&→ ~a ~s~%~a~%" name args result)
	collect (list (tool-call-id call) name result)))

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
      (append history (list (make-turn :role :user :text prompt)))
      (list (make-turn :role :system :text system)
	    (make-turn :role :user   :text prompt))))


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
      (let ((turn (apply #'run-model model msgs tools opts)))
	(when (eq (turn-stop turn) :error)
	  (return (list (turn-text turn) msgs)))
	(setf msgs (append msgs (list turn)))
	(if (turn-calls turn)
	    (setf msgs (append msgs (list (make-turn
					    :role :tool-results
					    :results (run-calls (turn-calls turn) tools)))))
	    (return (list (turn-text turn) msgs))))
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
      (let ((turn (apply #'run-model model msgs tools :thinking t opts)))
	(when (eq (turn-stop turn) :error)
	  (return (list (turn-text turn) msgs)))
	(setf msgs (append msgs (list turn)))
	(cond
	  ;; Retry with thinking off, keeping the partial trace in
	  ;; context: the model keeps its work but must now commit.
	  ((eq (turn-stop turn) :overflow)
	   (format t "~&⋯ deliberation overflowed, retrying with thinking disabled~%")
	   (let ((retry (apply #'run-model model msgs tools :thinking nil opts)))
	     (when (eq (turn-stop retry) :error)
	       (return (list (turn-text retry) msgs)))
	     (setf msgs (append msgs (list retry)))
	     (if (turn-calls retry)
		 (setf msgs (append msgs (list (make-turn
						 :role :tool-results
						 :results (run-calls (turn-calls retry) tools)))))
		 (return (list (turn-text retry) msgs)))))
	  ((turn-calls turn)
	   (setf msgs (append msgs (list (make-turn
					   :role :tool-results
					   :results (run-calls (turn-calls turn) tools))))))
	  (t (return (list (turn-text turn) msgs)))))
	  finally (return (list (format nil "[stopped: hit max-turns (~a)]" max-turns)
				msgs)))))
