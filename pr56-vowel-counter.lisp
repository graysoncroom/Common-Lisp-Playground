(defparameter *filename* "data-sets/pr56-vowel-counter.dat")

(defun file-get-vowel-counts (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read-line input-stream nil)
      (loop for line = (read-line input-stream nil)
            while line
            collect (line->vowels line)))))

(defun line->vowels (line)
  (list #\A (count #\a line :test #'char-equal)
        #\E (count #\e line :test #'char-equal)
        #\I (count #\i line :test #'char-equal)
        #\O (count #\o line :test #'char-equal)
        #\U (count #\u line :test #'char-equal)))

(defun pr56-vowel-counter-main ()
  (format t "~{~{~a - ~a~^ ~}~%~}" (file-get-vowel-counts *filename*)))
