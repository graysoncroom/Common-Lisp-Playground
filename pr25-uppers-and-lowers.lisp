(defparameter *filename* "data-sets/pr25-uppers-and-lowers.dat")

(defun file-get-words (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for line = (read-line input-stream nil)
	    while line
	    collect line))))

(defun pr25-uppers-and-lowers-main ()
  (dolist (word (file-get-words *filename*))
    (format t "~a~%" (if (evenp (length word))
			 (string-upcase word)
			 (string-downcase word)))))
