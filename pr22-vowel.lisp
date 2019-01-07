(defparameter *filename* "data-sets/pr22-vowel.dat")

(defun file-get-lines (filename)
  (with-open-file (input-stream filename)
    (when input-stream
      (read-line input-stream nil)
      (loop for line = (read-line input-stream nil)
	    while line
	    collect line))))

(defun vowelp (letter)
  (find letter "aeiou" :test #'char=))

(defun last-letter-vowel-p (word)
  (vowelp (char (nreverse word) 0)))

(defun pr22-vowel-main ()
  (format t "~{~:[No Vowel~;Vowel~]~%~}"
	  (mapcar #'last-letter-vowel-p
		  (file-get-lines *filename*))))

(defun pr22-vowel-main* ()
  (loop for word in (file-get-lines *filename*)
	while word
	do (format t "~:[No Vowel~;Vowel~]~% (last-letter-vowel-p word))))
