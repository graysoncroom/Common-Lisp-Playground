;;; Goofy first reader macro
(defun 5-is-6 (stream char)
  (declare (ignore char stream))
  6)

(set-macro-character #\5 #'5-is-6)

;;; First attempt at some syntactic sugar for lambdas
(defun lambda-reader (stream char)
  (declare (ignore char))
  `(lambda (&optional $0 $1 $2 $3 $4 $5 $6 $7 $8 $9)
     (declare (ignorable $0 $1 $2 $3 $4 $5 $6 $7 $8 $9))
     ,(read stream t nil t)))

(set-macro-character #\! #'lambda-reader)
