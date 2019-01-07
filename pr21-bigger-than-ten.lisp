(defparameter *filename* "data-sets/pr21-bigger-than-ten.dat")

(defun file-get-nums (filename)
  (with-open-file (input-stream filename)
    (when input-stream
      (loop for num = (read input-stream nil)
	    while num
	    collect num))))

(defun file-get-nums* (filename)
  (with-open-file (input-stream filename)
    (when input-stream
      (do ((num (read input-stream nil)
		(read input-stream nil))
	   (nums nil))
	  ((null num) (nreverse nums))
	(push num nums)))))
	

(defun pr21-bigger-than-ten-main ()
  (loop for num in (cdr (file-get-nums *filename*))
	while num
	do (format t "~&~a" (if (> num 10) "Bigger than ten." "Not bigger than ten."))))

(defun pr21-bigger-than-ten-main* ()
  (dolist (num (cdr (file-get-nums *filename*)))
    (format t "~&~a" (if (> num 10) "Bigger than ten." "Not bigger than ten."))))

(defun pr21-bigger-than-ten-main** ()
  (let ((nums (cdr (file-get-nums *filename*))))
    (do ((num (pop nums) (pop nums)))
	((null num))
      (format t "~&~a" (if (> num 10) "Bigger than ten." "Not bigger than ten.")))))

;; look into ~&~a vs ~a~& after going over PCL loop chapter
(defun pr21-bigger-than-ten-main*** ()
  (let ((nums (cdr (file-get-nums *filename*))))
    (loop while nums do
      (format t "~a~&" (if (> (pop nums) 10) "Bigger than ten." "Not bigger than ten.")))))
