(defparameter *filename* "data-sets/pr25-marathons.dat")

(defun file-get-nums (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for num = (read input-stream nil)
	    while num
	    collect num))))

(defun pr25-marathons-main ()
  (format t "~{~,2f~%~}" (mapcar #'(lambda (num)
				   (/ num 26.2))
			       (file-get-nums *filename*))))
