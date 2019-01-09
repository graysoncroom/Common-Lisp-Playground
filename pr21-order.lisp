(defun file-get-lines (filename)
  (with-open-file (input-stream filename)
    (when input-stream
      (loop for line = (read-line input-stream nil)
            while line
            collect line))))

(defun line->nums (line)
  (with-input-from-string (in line)
    (loop for num = (read in nil)
          while num
          collect num)))

(defun pr21-order-main ()
  (loop for line in (file-get-lines "data-sets/pr21-order.dat")
        while line
        do (format t "~&~{~a~^ ~}" (sort (line->nums line) #'<))))

;; not quite right
;; (line->nums "1 2 3") produces: (1 2) instead of: (1 2 3)
;; not sure why
(defun line->nums* (line)
  (do* ((i 0 (1+ j))
        (j (position #\Space line :start i)
           (position #\Space line :start i))
        (nums nil))
       ((null j) (nreverse nums))
    (push (parse-integer (subseq line i j)) nums)))

(defun line->nums** (line)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space line :start i)
        collect (parse-integer (subseq line i j))
        while j))
