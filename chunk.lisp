;;;; chunk.lisp


(in-package :apprentice)


(defvar *default-chunk-size* 500)
(defvar *chunk-overlap* 50)

(defstruct chunk
  "A chunk of text with its text, and offsets in url content."
  text
  file-path
  start-offset
  end-offset)


(defun split-file-into-chunks (file &key (chunk-size *default-chunk-size*)
				         (overlap *chunk-overlap*))
  "Split TEXT into overlapping chunks of CHUNK-SIZE characters.
   Tries to break at sentence level when possible."
  (let* ((text (file-content file))
	 (chunks nil)
	 (len (length text))
	 (start 0))
    (loop while (< start len)
	  do (let* ((end (min (+ start chunk-size) len))
		    (break-pos
		      (if (>= end len)
			  end
			  (or (position #\. text :start (max start (- end 80))
						 :end end :from-end t)
			      (position #\Newline text :start (max start (- end 80))
						       :end end :from-end t)
			      end)))
		    (actual-end (if (< break-pos end)
				    (1+ break-pos)
				    end))
		    (chunk (make-chunk :text (string-trim '(#\Space #\Newline #\Tab)
							  (subseq text start actual-end))
				       :file-path (file-path file)
				       :start-offset start
				       :end-offset actual-end)))
	       (when (> (length (chunk-text chunk)) 0)
		 (push chunk chunks))
	       (if (>= actual-end len)
		   (setf start len)
		   (setf start (- actual-end overlap)))))
    (nreverse chunks)))
