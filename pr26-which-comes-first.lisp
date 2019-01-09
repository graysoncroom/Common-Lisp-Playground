(defparameter *filename* "data-sets/pr26-which-comes-first.dat")

(defun file-get-pairs (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for first = (read input-stream nil)
            for second = (read input-stream nil)
            while (and first second)
            collect (cons (string first)
                          (string second))))))

(defun pr26-which-comes-first-main ()
  (dolist (pair (file-get-pairs *filename*))
    (let ((first (car pair))
          (second (cdr pair)))
      (format t "~(~a~)~%" (if (string<= first second)
                               first
                               second)))))
