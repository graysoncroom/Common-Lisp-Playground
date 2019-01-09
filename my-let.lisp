;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun get-binding-symbols (bindings)
  (loop while bindings
        collecting (car (pop bindings))))

(defun get-binding-values (bindings)
  (loop while bindings
        collecting (cadr (pop bindings))))
  
(defmacro my-let (bindings &body body)
  `((lambda ,(get-binding-symbols bindings) ,@body) ,@(get-binding-values bindings)))
