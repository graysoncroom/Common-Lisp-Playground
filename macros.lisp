(defmacro my-when (condition &body body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &body body)
  `(if (not ,condition) (progn ,@body)))

(format t "~{~a~%~}" (loop for i from 1 to 10 collecting i))

(defmacro range (start end &optional (step 1))
  `(list ,@(loop for i from start to end by step collecting i)))

(format t "~{~a~%~}" (range 1 10))

#|
(do (variable-definition*)
    (end-test-form result-form*)
  statement*)

where (variable-definition) -> (var init-form step-form)(
|#

(defun efficient-fibonacci (n)
  (do ((i 0 (+ i 1))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= n i) cur)))
