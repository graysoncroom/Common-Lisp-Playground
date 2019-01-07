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

(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))
;; -> same as
(dotimes (i 4)
  (print i))

;;; The Mighty LOOP

(do ((nums nil)
     (i 1 (+1 i)))
    ((> i 10) (nreverse nums))
  (push i nums))
;; -> same as
(loop for i from 1 to 10 collecting i)

;; Sums the first ten squares
(loop for x from 1 to 10 summing (expt x 2)) 

;; Counts the number of vowels in a string
(loop for x across "the quick brow fox jumps over the lazy dog"
      counting (find x "aeiou"))

;; Computes the eleventh Fibonacci number
(loop for i below 10
      and cur = 0 then next
      and next = 1 then (+ next cur)
      finally (return cur))
