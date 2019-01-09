(defparameter *filename* "data-sets/pr54-translation-please.dat")

(defun file-get-lines (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (do ((line (read-line input-stream nil)
                 (read-line input-stream nil))
           (lines nil))
          ((null line) (nreverse lines))
        (push line lines)))))

(defun translate-line (line)
  (let ((word-count (1+ (count-if (lambda (x) (char= #\Space x)) line))))
    (loop for x across line
          collect (if (alpha-char-p x)
                      (progn
                        (code-char (let ((code (+ (char-code x) word-count)))
                                     (if (> code (char-code (if (upper-case-p x) #\Z #\z)))
                                         (- code 26)
                                         code))))
                      x))))

(defun pr54-translation-please-main ()
  (format t "~{~{~a~}~%~}" (mapcar #'translate-line (file-get-lines *filename*))))
