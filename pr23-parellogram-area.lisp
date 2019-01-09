(defparameter *filename* "data-sets/pr23-parellogram-area.dat")

(defun file-get-nums (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil) ; Ignore the line giving us the # of cases
      (loop for line = (read-line input-stream nil)
            while line
            collect (line->nums line)))))

(defun file-get-nums* (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil) ; Ignore the line giving us the # of cases
      (do ((line (read-line input-stream nil)
                 (read-line input-stream nil))
           (lines nil))
          ((null line) (nreverse lines))
        (push (line->nums* line) lines)))))

(defun line->nums (line)
  (with-input-from-string (s line)
    (when s
      (loop for num = (read s nil)
            while num
            collect num))))

(defun line->nums* (line)
  (with-input-from-string (s line)
    (when s
      (do ((num (read s nil)
                (read s nil))
           (nums nil))
          ((null num) (nreverse nums))
        (push num nums)))))

(defun pr23-parellogram-area-main ()
  (format t "~{~,2f~%~}" (mapcar #'(lambda (dimensions) (reduce #'* nums))
                               (file-get-nums *filename*))))

(defun pr23-parellogram-area-main* ()
  (format t "~{~,2f~%~}" (loop for dimensions in (file-get-nums *filename*)
                               while dimensions
                               collect (reduce #'* dimensions))))

(defun pr23-parellogram-area-main** ()
  (format t "~{~,2f~%~}" (do* ((nums (file-get-nums *filename*))
                              (dimensions (pop nums)
                                          (pop nums))
                              (areas nil))
                             ((null dimensions) (nreverse areas))
                           (push (reduce #'* dimensions) areas))))
