(defparameter *filename* "data-sets/pr92-committee-selection.dat")
(defparameter *ED-count* 0)

(defun file-get-members-lists (filename)
  (with-open-file (input-stream filename :if-does-not-exist nil)
    (when input-stream
      (read input-stream nil)
      (loop for line1 = (read-line input-stream nil)
            for line2 = (read-line input-stream nil)
            for line3 = (read-line input-stream nil)
            while (and line1 line2 line3)
            collect (line->members (concatenate 'string
                                                line1 " "
                                                line2 " "
                                                line3))))))

(defun line->members (line)
  (with-input-from-string (s line)
    (loop for name = (read s nil)
          while name
          collect name)))

(defun name-filter (name) (lambda (x) (eq x name)))

(defun symbol< (x y)
  (string< (string x)
           (string y)))

(defun members-list->members-alist (members-list)
  (let ((members-alist nil))
    (loop for member-name in members-list
          for member-count = (count-if (lambda (x)
                                         (eq x member-name))
                                       members-list)
          do (if (eq member-name 'ED)
                 (incf *ED-count*)
                 (when (not (assoc member-name members-alist))
                   (push (cons member-name member-count)
                         members-alist))))
    (nreverse members-alist)))

(defun add-ed (members-alist)
  (cons (cons 'ED
              *ED-count*)
        member-alist))

(defun member-alist->string (member-alist)
  (concatenate 'string
               (string (car member-alist))
               "="
               (write-to-string (cdr member-alist))))

(defun pr92-committee-selection-main ()
  (setf *ED-count* 0)
  (let* ((members-alists (mapcar #'members-list->members-alist
                                 (file-get-member-lists *filename*)))
         (members-alists-with-ed (mapcar #'add-ed members-alists)))
    (dolist (members-alist-with-ed members-alists-with-ed)
      (format t "~{~a~^,~^ ~}~%" (mapcar #'member-alist->string
                                         (stable-sort members-alist-with-ed
                                                      (lambda (m1 m2)
                                                        (symbol< (car m1)
                                                                 (car m2)))))))))
