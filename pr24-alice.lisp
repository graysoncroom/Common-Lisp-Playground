(defparameter *filename* "data-sets/pr24-alice.dat")

(defun file-get-lines (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil)
      (loop for line = (read-line input-stream nil)
	    while line
	    collect line))))

(defun file-get-lines* (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil)
      (do ((line (read-line input-stream nil)
		 (read-line input-stream nil))
	   (lines nil))
	  ((null line) (nreverse lines))
	(push line lines)))))
	   

(defun count-word-occurance (line search-word)
  (let ((alice-count 0))
    (loop for alice-index = (search search-word line)
	  while alice-index
	  do (setf line (concatenate 'string
				     (subseq line 0 alice-index)
				     (subseq line
					     (+ alice-index
						(length search-word))
					     (length line))))
	     (incf alice-count))
    alice-count))

(defun count-word-occurance* (line search-word)
  (do ((alice-index (search search-word line)
		    (search search-word line))
       (alice-count 0 (1+ alice-count)))
      ((null alice-index) alice-count)
    (setf line (concatenate 'string
			    (subseq line 0 alice-index)
			    (subseq line
				    (+ alice-index
				       (length search-word))
				    (length line))))))
  
(defun pr24-alice-main ()
  (dolist (line (file-get-lines *filename*))
    (format t "~a~%" (count-word-occurance line "ALICE"))))

(defun pr24-alice-main* ()
  (format t "~{~a~%~}" (mapcar #'(lambda (line)
				   (count-word-occurance line "ALICE"))
			       (file-get-lines *filename*))))
