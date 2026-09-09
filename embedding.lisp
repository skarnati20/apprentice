;;;; embedding.lisp

(in-package :apprentice)


;;;; Types and Constructors


(deftype embedding ()
  "A dense vector of single-floats."
  '(simple-array single-float (*)))

(defun make-embedding (n)
  "A zeroed embedding of length N."
  (make-array n :element-type 'single-float :initial-element 0.0))

(defun to-embedding (numbers)
  "Coerce NUMBERS (a list or vector) into an EMBEDDING."
  (if (typep numbers 'embedding)
      numbers
      (map 'embedding (lambda (x) (float x 0.0)) numbers)))


;;;; Vector Arithmetic


(defun vector-magnitude (vec)
  "Compute the magnitude (L2 norm) of an embedding."
  (declare (type embedding vec)
	   (optimize (speed 3) (safety 1)))
  (let ((sum 0.0))
    (declare (type single-float sum))
    (dotimes (i (length vec))
      (incf sum (* (aref vec i) (aref vec i))))
    (sqrt sum)))

(defun dot-product (vec-a vec-b)
  "Compute the dot product of two embeddings."
  (declare (type embedding vec-a vec-b)
	   (optimize (speed 3) (safety 0)))
  (let ((sum 0.0))
    (declare (type single-float sum))
    (dotimes (i (min (length vec-a) (length vec-b)) sum)
      (incf sum (* (aref vec-a i) (aref vec-b i))))))

(defun cosine-similarity (vec-a vec-b)
  "Compute cosine similarity between two embeddings.
   Returns a value between -1 and 1."
  (let ((mag-a (vector-magnitude vec-a))
	(mag-b (vector-magnitude vec-b)))
    (if (or (zerop mag-a) (zerop mag-b))
	0.0
	(/ (dot-product vec-a vec-b) (* mag-a mag-b)))))

(defun make-unit (vec)
  "Makes VEC a unit vector. A zero vector has no direction, so it is returned
   unchanged rather than dividing by zero."
  (declare (type embedding vec))
  (let ((mag (vector-magnitude vec)))
    (if (zerop mag)
	vec
	(let ((unit (make-embedding (length vec))))
	  (dotimes (i (length vec) unit)
	    (setf (aref unit i) (/ (aref vec i) mag)))))))


;;;; Embedding Client


(defvar *embedding-endpoint* "http://localhost:8081/v1/embeddings")

(defun create-embedding (input &key (endpoint *embedding-endpoint*) (model "default"))
  "Generate normalized unit embedding vector(s) for INPUT (a string or list of strings)
   using the OpenAI-compatible embedding ENDPOINT.
   Returns a single unit EMBEDDING if INPUT is a string, or a list of unit
   EMBEDDINGs if INPUT is a list of strings."
  (let* ((body (lisp-to-json-string
                (j "input" input
                   "model" model)))
         (raw (run-argv (list "curl" "-s" endpoint
                              "-H" "Content-Type: application/json"
                              "--data-binary" "@-")
                        :input body
                        :limit most-positive-fixnum))
         (decoded (ignore-errors (json:decode-json-from-string raw)))
         (err (and decoded (s decoded "error"))))
    (cond
      (err
       (error "Embedding error: ~a" (or (s err "message") err)))
      ((null decoded)
       (error "Failed to decode embedding response: ~a" raw))
      ((stringp input)
       (make-unit (to-embedding (s (first (s decoded "data")) "embedding"))))
      (t
       (mapcar (lambda (item)
                 (make-unit (to-embedding (s item "embedding"))))
               (s decoded "data"))))))
