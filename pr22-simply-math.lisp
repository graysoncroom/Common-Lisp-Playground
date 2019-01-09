(defparameter *filename* "data-sets/pr22-simply-math.dat")

(defun file-get-statements (filename)
  (with-open-file (input-stream filename)
    (when input-stream
      (read-line input-stream nil)
      (loop for line = (read-line input-stream nil)
            while line
            collect (line->statement line)))))

(defun line->statement (line)
  (with-input-from-string (s line)
    (when s
      (loop for x = (read s nil)
            while x
            collect x))))

(defun postfix->infix (statement)
  (cons (car statement)
        (reverse (cdr statement))))

(defun postfix->prefix (statement)
  (list (caddr statement)
        (car statement)
        (cadr statement)))

(defun pr22-simply-math-main ()
  (loop for row in (file-get-statements *filename*)
        while row
        do (format t "~&~{~a ~}= ~a"
                   (postfix->infix row)
                   (eval (postfix->prefix row)))))
