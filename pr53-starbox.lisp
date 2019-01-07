(defparameter *filename* "data-sets/pr53-starbox.dat")

(defun file-get-rc-pairs (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for row = (read input-stream nil)
	    for col = (read input-stream nil)
	    while (and row col)
	    collect (cons row col)))))

(defun file-get-rc-pairs* (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (do ((row (read input-stream nil)
		(read input-stream nil))
	   (col (read input-stream nil)
		(read input-stream nil))
	   (pairs nil))
	  ((null (and row col)) (nreverse pairs))
	(push (cons row col) pairs)))))

(defun pr53-starbox-main ()
  (dolist (pair (file-get-rc-pairs *filename*))
    (destructuring-bind (row . col) pair
      (dotimes (i row)
	(dotimes (j col)
	  (princ "*"))
	(terpri))
      (terpri))))

;; Formating is not quite right
;; Needs a newline between boxes
(defun pr53-starbox-main* ()
  (dolist (pair (file-get-rc-pairs *filename*))
    (destructuring-bind (row . col) pair
      (format t "~{~{~a~}~%~}"
	      (loop for i to row
		    collecting (loop for j to col
				     collecting #\*))))))
