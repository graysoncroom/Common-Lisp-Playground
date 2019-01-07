(defparameter *filename* "data-sets/pr55-time-for-a-winner.dat")

(defun file-get-times (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for team1 = (read-line input-stream nil)
	    for team2 = (read-line input-stream nil)
	    while (and team1 team2)
	    collect (list (parse-line team1)
			  (parse-line team2))))))

;; Time is in mins
(defun parse-line (line)
  (let* ((name-score-fence (position #\: line))
	 (team-name (subseq line 0 name-score-fence))
	 (team-times (with-input-from-string (s (substitute #\Space #\:
							   (subseq line name-score-fence)))
		       (loop for hours = (read s nil)
			     for mins = (read s nil)
			     while (and hours mins)
			     collect (+ (* hours 60) mins)))))
    (cons team-name team-times)))

(defun pr55-time-for-a-winner-main ()
  (dolist (team-pair (file-get-times *filename*))
    (destructuring-bind (team1 team2) team-pair
      (let ((team1-total-time (reduce #'+ (cdr team1)))
	    (team2-total-time (reduce #'+ (cdr team2))))
	(format t "~@:(~a~)~%" (if (< team1-total-time team2-total-time)
				   (car team1)
				   (car team2)))))))
