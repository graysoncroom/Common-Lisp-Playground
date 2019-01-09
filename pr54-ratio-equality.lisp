(defparameter *filename* "data-sets/pr54-ratio-equality.dat")

(defun file-get-ratios (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (loop for numerator1 = (read input-stream nil)
            for denominator1 = (read input-stream nil)
            for numerator2 = (read input-stream nil)
            for denominator2 = (read input-stream nil)
            while (and numerator1 denominator1 numerator2 denominator2)
            collect (cons (/ numerator1 denominator1)
                          (/ numerator2 denominator2))))))

;; Equality as specified by the problem.
;; We will use this as this is exactly what the problem says to do.
(defun pr54-ratio-equality-main ()
  (dolist (ratio-pair (file-get-ratios *filename*))
    (destructuring-bind (r1 . r2) ratio-pair
      (format t "~,6f ~:[NOT EQUAL~;EQUAL RATIOS~]~%"
              (abs (float (- r1 r2)))
              (< (abs (float (- r1 r2))) 0.00001)))))

;; Since common lisp deals with ratios well, we can
;; write the solution without caring about precision.
(defun pr54-ratio-equality-main* ()
  (dolist (ratio-pair (file-get-ratios *filename*))
    (destructuring-bind (r1 . r2) ratio-pair
      (format t "~,6f ~:[NOT EQUAL~;EQUAL RATIOS~]~%"
              (abs (float (- r1 r2)))
              (= r1 r2)))))
