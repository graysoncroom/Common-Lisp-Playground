(defparameter *filename* "data-sets/pr52-probability.dat")

(defun file-get-probabilities (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for total = (read input-stream nil)
	    for heads = (read input-stream nil)
	    while (and total heads)
	    collect (/ heads total)))))

(defun pr52-probability-main ()
  (format t "~{~,3f probability~%~}" (file-get-probabilities *filename*)))
