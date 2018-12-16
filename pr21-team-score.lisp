(defun file-get-lines (filename)
  (with-open-file (input-stream filename)
    (when input-stream
      (loop for line = (read-line input-stream nil)
	    while line
	    collect line))))

(let ((names (cdr (file-get-lines "data-sets/pr21-team-score.dat"))))
  (loop while names
	do (format t "~&~a" (+ (length (pop names))
			       (length (pop names))
			       (length (pop names))))))
