(defparameter *filename* "data-sets/pr26-sums-it-all-up.dat")

(defun file-get-lines (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for line = (read-line input-stream nil)
	    while line
	    collect line))))

(defun get-pair-sums (pairs)
  (do ((sums nil))
      ((null pairs) (nreverse sums))
    (push (+ (pop pairs)
	     (pop pairs))
	  sums)))

(defun pr26-sums-it-all-up-main ()
  (dolist (line (file-get-lines *filename*))
    (let ((trimmed-line (remove #\Space line)))
      (format t "~{~a~^ ~}~%" (get-pair-sums (map 'list #'char-code trimmed-line))))))
