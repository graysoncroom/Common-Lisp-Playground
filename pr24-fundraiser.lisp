(defparameter *filename* "data-sets/pr24-fundraiser.dat")

(defun file-get-data (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil) ; Ignore the number of cases in file
      (do ((num (read input-stream nil)
		(read input-stream nil))
	   (line (read-line input-stream nil)
		 (read-line input-stream nil))
	   (data nil))
	  ((null (and num line)) (nreverse data))
	(push (cons num (line->items line)) data)))))

(defun file-get-data* (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil) ; Ignore the number of cases in file
      (loop for num = (read input-stream nil)
	    for line = (read-line input-stream nil)
	    while (and num line)
	    collect (cons num (line->items line))))))

(defun line->items (line)
  (with-input-from-string (s line)
    (loop for num = (read s nil)
	  while num
	  collect num)))

(defun line->items* (line)
  (with-input-from-string (s line)
    (do ((num (read s nil)
	      (read s nil))
	 (nums nil))
	((null num) (nreverse nums))
      (push num nums))))

(defun pr24-fundraiser-main ()
  (dolist (datum (file-get-data *filename*))
    (destructuring-bind (num &rest items) datum
      (let ((temp-num num)
	    (required-items nil)
	    (required-items-sum 0))
	(loop for item in (stable-sort items #'>)
	      while (and item (< required-items-sum num))
	      do (setf temp-num (- temp-num item))
		 (push item required-items)
		 (setf required-items-sum (+ required-items-sum item))
	      finally (setf required-items (nreverse required-items)))
	(format t "~{~a~^ ~}~%" required-items)))))
