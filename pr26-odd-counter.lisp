(defparameter *filename* "data-sets/pr26-odd-counter.dat")

(defun file-get-pairs (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for line = (read-line input-stream nil)
            while line
            collect (line->pair line)))))

(defun line->pair (line)
  (with-input-from-string (s line)
    (cons (read s nil) (read s nil))))

(defun pr26-odd-counter-main ()
  (dolist (pair (file-get-pairs *filename*))
    (destructuring-bind (start . end) pair
      (format t "~a~%" (loop for i from start to end
                             counting (oddp i))))))
           

