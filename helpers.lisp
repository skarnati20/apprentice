;;;; helpers.lisp

(in-package #:apprentice)


;;;; Formatting


(defmethod json:encode-json ((x (eql :false)) &optional stream)
  "Emit JSON false. CL-JSON maps NIL to null, and a NIL alist value
   collapses the pair into a one-element list that encodes as an
   array, so booleans need their own marker."
  (write-string "false" stream))

(defun lisp-to-json-string (data)
  "Encode DATA as JSON. Key symbols go through cl-json's usual
   mapping, which is what round-trips decoded messages: :ROLE back to
   role, :REASONING--CONTENT back to reasoning_content."
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun lisp-to-verbatim-json-string (data)
  "Encode DATA as JSON with key symbols emitted exactly as named.
   For APIs wanting literal camelCase params: the default encoder
   would downcase numResults to numresults. Only safe for alists
   built by hand, never for anything cl-json decoded."
  (with-output-to-string (s)
    (let ((json:*lisp-identifier-name-to-json* #'string))
      (json:encode-json data s))))

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

(defun lisp-to-corrected-json-string (data)
  (substitute-subseq
   (lisp-to-json-string data)
   ":null"
   ":false"
   :test #'string=))


;;;; Ease-of-Use JSON Functions


(defun j (&rest plist)
  "Function to format pairs (for JSON)."
  (loop for (k v) on plist by #'cddr
        collect (cons (intern k :keyword) v)))

(defun s (obj &rest keys)
  "Walks OBJ down through list of KEYS. Returns
   value at end of walk, otherwise NIL."
  (let ((curr obj))
    (dolist (k keys curr)
      (when (null curr) (return nil))
      (setf curr (cdr (assoc k curr :key #'symbol-name :test #'string-equal))))))


;;;; Command Running


(defun truncate-output (s limit)
  "Cap S at LIMIT characters, telling the reader it was cut."
  (cond ((null limit) s)
        ((null s) "")
        ((<= (length s) limit) s)
        (t (format nil "~a~%~%[truncated — showing ~a of ~a characters]"
                   (subseq s 0 limit) limit (length s)))))

(defun run-argv (argv &key directory input (limit 6000) (ok-codes '(0 1)))
  (handler-case
      (multiple-value-bind (out err code)
	  (uiop:run-program argv
                            :output :string
                            :error-output :string
                            :directory directory
                            :input (when input (make-string-input-stream input))
                            :ignore-error-status t)
	(if (member code ok-codes)
            (truncate-output out limit)
            (format nil "Command failed (exit ~a): ~a"
                    code (truncate-output err 500))))
    (error (e)
      (format nil "Could not run command: ~a" e))))
