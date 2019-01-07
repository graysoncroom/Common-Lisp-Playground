(defparameter *filename* "data-sets/pr52-check-please.dat")

(defun file-get-lines (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for line = (read-line input-stream nil)
	    while line
	    collect line))))

(defun file-get-lines* (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (do ((line (read-line input-stream nil)
		 (read-line input-stream nil))
	   (lines nil))
	  ((null line) (nreverse lines))
	(push line lines)))))

(defun pr52-check-please-main ()
  (dolist (line (file-get-lines *filename*))
    (format t "~:[0~;1~]~a~%"
	    (oddp (count-if (lambda (x) (char= x #\1))
			     line))
	    line)))

(defun pr52-check-please-main* ()
  (dolist (line (file-get-lines *filename*))
    (if (evenp (count-if (lambda (x) (char= x #\1))
			 line))
	(format t "0~a~%" line)
	(format t "1~a~%" line))))
	    
