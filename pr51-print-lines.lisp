(defparameter *filename* "data-sets/pr51-print-lines.dat")

(defun file-get-nums (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for num = (read input-stream nil)
	    while num
	    collect num))))

(defun pr51-print-lines-main ()
  (dolist (num (file-get-nums *filename*))
    (format t "[~{~a~}]~%" (loop for x from 1 to num collect #\X))))
