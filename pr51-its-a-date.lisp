(defparameter *filename* "data-sets/pr51-its-a-date.dat")

(defun file-get-dates (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for month = (read input-stream nil)
            for day = (read input-stream nil)
            for year = (read input-stream nil)
            while (and month day year)
            collect (list month day year)))))

(defun pr51-its-a-date-main ()
  (dolist (date (file-get-dates *filename*))
    (format t "~@:(~a~) ~a, ~a~%"
            (num->month (car date))
            (cadr date)
            (caddr date))))

(defun num->month (num)
  (case num
    (1 "January")
    (2 "Febuary")
    (3 "March")
    (4 "April")
    (5 "May")
    (6 "June")
    (7 "July")
    (8 "August")
    (9 "September")
    (10 "October")
    (11 "November")
    (12 "December")))
