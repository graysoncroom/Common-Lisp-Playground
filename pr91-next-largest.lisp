(defparameter *filename* "data-sets/pr91-next-largest.dat")

(defun file-get-nums (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (do ((line (read-line input-stream nil)
		 (read-line input-stream nil))
	   (search-num (read input-stream nil)
		       (read input-stream nil))
	   (nums nil))
	  ((null line) (nreverse nums))
	(push (cons search-num (line->nums line)) nums)))))

(defun line->nums (line)
  (with-input-from-string (s line)
    (do ((num (read s nil)
	      (read s nil))
	 (nums nil))
	((null num) (nreverse nums))
      (push num nums))))

(defun pr91-next-largest-main ()
  (dolist (nums (file-get-nums *filename*))
    (destructuring-bind (search-num &rest seq) nums
      (princ (find-if (lambda (num)
			(> num search-num))
		      (sort seq #'<))))
    (terpri)))
