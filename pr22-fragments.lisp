(defparameter *filename* "data-sets/pr22-fragments.dat")

(defun file-get-data (filename)
  (with-open-file (input-stream filename)
    (when input-stream
      (read-line input-stream nil)
      (do ((line (read-line input-stream nil)
                 (read-line input-stream nil))
           (data nil))
          ((null line) (nreverse data))
        (push (line->datum line) data)))))

(defun line->datum (line)
  (with-input-from-string (s line)
    (when s
      (read s nil)
      (list (subseq line 0 (position #\Space line))
            (read s nil)
            (read s nil)))))
            
(defun sanitize-datum (datum)
  (if (= (third datum) -1)
      (list (first datum)
            (second datum))
      datum))

(defun process-datum (datum)
  (eval `(subseq ,@(sanitize-datum datum))))

(defun pr22-fragments-main ()
  (loop for datum in (file-get-data *filename*)
        while datum
        do (format t "~a~%" (eval `(subseq ,@(sanitize-datum datum)))))) 

(defun pr22-fragments-main* ()
  (format t "~{~a~%~}" (mapcar #'process-datum
                               (file-get-data *filename*))))
