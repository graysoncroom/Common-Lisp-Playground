(defparameter *filename* "data-sets/pr92-mountain.dat")

(defun file-get-terrains (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for line = (read-line input-stream nil)
            while line
            collect (line->nums line)))))

(defun line->nums (line)
  (with-input-from-string (s line)
    (loop for num = (read s nil)
          while num
          collect num)))

(defun mountain? (terrain)
  (let* ((peak-position (position (reduce #'max terrain) terrain))
        (first (subseq terrain 0 peak-position))
        (second (subseq terrain peak-position)))
    (and (equal first (sort (copy-seq first) #'<))
         (> (length second) 1)
         (equal second (sort (copy-seq second) #'>)))))

(defun pr92-mountain-main ()
  (dolist (terrain (file-get-terrains *filename*))
    (format t "~:[NOT MOUNTAIN~;MOUNTAIN~]~%" (mountain? terrain))))
