(defparameter *filename* "data-sets/pr24-where-is-it.dat")

(defun file-get-pairs (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil) ; Ignore the line giving us the # of cases
      (loop for quote = (read-line input-stream nil)
            for letter = (read-line input-stream nil)
            while (and quote letter)
            collect (cons (char letter 0) quote)))))

(defun file-get-pairs* (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil) ; Ignore the line giving us the # of cases
      (do ((quote (read-line input-stream nil)
                  (read-line input-stream nil))
           (letter (read-line input-stream nil)
                   (read-line input-stream nil))
           (pairs nil))
          ((or (null quote) (null letter)) (nreverse pairs))
        (push (cons (char letter 0)
                    quote)
              pairs)))))
           

(defun pr24-where-is-it-main ()
  (dolist (pair (file-get-pairs *filename*))
    (destructuring-bind (letter . quote) pair
      (format t "~:[NOT THERE~;~:*~a~]~%" (position letter quote :test #'char=)))))

(defun pr24-where-is-it-main* ()
  (do* ((pairs (file-get-pairs *filename*))
        (pair (pop pairs)
              (pop pairs)))
       ((null pairs))
    (destructuring-bind (letter . quote) pair
      (format t "~:[NOT THERE~;~:*~a~]~%" (position letter quote :test #'char=)))))

(defun pr24-where-is-it-main** ()
  (do* ((pairs (file-get-pairs *filename*))
        (pair (pop pairs)
              (pop pairs)))
       ((null pairs))
    (destructuring-bind (letter . quote) pair
      (let ((index-of-letter (position letter quote :test #'char=)))
        (if index-of-letter
            (format t "~a~%" index-of-letter)
            (format t "NOT THERE~%"))))))

(defun pr24-where-is-it-main*** ()
  (dolist (pair (file-get-pairs *filename*))
    (destructuring-bind (letter . quote) pair
      (let ((index-of-letter (position letter quote :test #'char=)))
        (if index-of-letter
            (format t "~a~%" index-of-letter)
            (format t "NOT THERE~%"))))))

(defun pr24-where-is-it-main**** ()
  (dolist (pair (file-get-pairs *filename*))
    (destructuring-bind (letter . quote) pair
    (format t "~:[NOT THERE~;~:*~a~]~%" (position letter quote :test #'char=)))))
