(defparameter *filename* "data-sets/pr56-is-it-there.dat")

(defun file-get-sentence-index-pair (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (loop for line = (read-line input-stream nil)
	    for index = (read input-stream nil)
	    while (and line index)
	    collect (cons index (line->words line))))))

(defun line->words (line)
  (with-input-from-string (s (substitute #\Newline #\Space line))
    (loop for word = (read-line s nil)
	  while word
	  collect word)))

(defun pr56-is-it-there-main ()
  (dolist (sentence-index-pair (file-get-sentence-index-pair *filename*))
    (destructuring-bind (index &rest words) sentence-index-pair
      (let ((therep (and (< index (length words))
			 (>= index 0))))
	(format t "~:[NOT THERE~;~a~]~%"
		therep
		(when therep 
		  (nth index words)))))))

(defun pr56-is-it-there-main* ()
  (dolist (sentence-index-pair (file-get-sentence-index-pair *filename*))
    (destructuring-bind (index &rest words) sentence-index-pair
      (if (and (< index (length words))
	       (>= index 0))
	  (princ (nth index words))
	  (princ "NOT THERE"))
      (terpri))))

;; Best solution in terms of readability
(defun pr56-is-it-there-main** ()
  (dolist (sentence-index-pair (file-get-sentence-index-pair *filename*))
    (destructuring-bind (index &rest words) sentence-index-pair
      (format t "~a~%" (if (and (< index (length words))
			       (>= index 0))
			  (nth index words)
			  "NOT THERE")))))
