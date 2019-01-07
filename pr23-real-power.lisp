(defparameter *filename* "data-sets/pr23-real-power.dat")

(defun file-get-num-pairs (filename)
  (with-open-file (input-stream filename)
    (when input-stream
      (read-line input-stream nil)
      (loop for line = (read-line input-stream nil)
	    while line
	    collect (line->num-pair line)))))

(defun line->num-pair (line)
  (with-input-from-string (s line)
    (when s
      (cons (read s nil)
	    (read s nil)))))

(defun get-powers (num-pair)
  (destructuring-bind (num . max-power) num-pair
    (loop for pow from 0 to max-power
	  collecting (expt num pow))))
  
(defun pr23-real-power-main ()
  (format t "~{~{~,2f~^ ~}~%~}" (mapcar #'get-powers
					(file-get-num-pairs *filename*))))

(defun pr23-real-power-main* ()
  (loop for num-pair in (file-get-num-pairs *filename*)
	while num-pair
	do (format t "~{~,2f~^ ~}~%" (get-powers num-pair))))

(defun pr23-real-power-main** ()
  (do ((num-pairs (file-get-num-pairs *filename*)))
      ((null num-pairs))
    (do ((powers (get-powers (pop num-pairs))))
	((null powers))
      (if (null (cadr powers))
	  (format t "~,2f" (pop powers))
	  (format t "~,2f " (pop powers))))
    (format t "~%")))

;; This version wouldn't have proper spacing
(defun pr23-real-power-main*** ()
  (dolist (num-pair (file-get-num-pairs *filename*))
    (let ((powers (get-powers num-pair)))
      (dolist (power powers)
	(if (= power (car (last powers)))
	    (format t "~,2f" power)
	    (format t "~,2f " power)))
      (format t "~%"))))
