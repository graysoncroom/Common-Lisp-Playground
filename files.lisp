;; Common Lisp provides a stream abstraction for reading and writing data and an abstraction,
;; called pathnames, for manipulating filenames in an operating system-independent way.
;; Additionally, Common Lisp provides other bits of functionality unique to Lisp such as the
;; ability to read and write s-expressions.

(let ((input-stream (open "data-sets/pr92-monster-maze.dat" :if-does-not-exist nil)))
  (when input-stream
    (format t "~a~%" (read-line input-stream nil))
    (close input-stream)))

(let ((input-stream (open "data-sets/pr92-monster-maze.dat" :if-does-not-exist nil)))
  (when input-stream
    (loop for line = (read-line input-stream nil)
	  while line
	  do (format t "~a~%" line))
    (close input-stream)))

(let ((input-stream (open "data-sets/pr92-monster-maze.dat"
			  :if-does-not-exist nil
			  :element-type '(unsigned-byte 8))))
  (when input-stream
    (loop for byte = (read-byte input-stream nil)
	  while byte
	  do (format t "~a~%" byte))
    (close input-stream)))

(let ((input-stream (open "data-sets/pr92-monster-maze.dat"))
      (sequence (make-array 100 :initial-element nil
				:adjustable t)))
  (when input-stream
    (read-sequence sequence input-stream))
  sequence)

(with-open-file (input-stream "data-sets/pr92-monster-maze.dat" :if-does-not-exist nil)
  (when input-stream
    (loop for line = (read-line input-stream nil)
	  while line
	  do (format t "~a~%" line))))

(pathname-directory (pathname "/foo/bar/baz.txt")) ; ==> (:ABSOLUTE "foo" "bar")
(pathname "/foo/bar/baz.txt") ; ==> #P"/foo/bar/baz.txt"
(pathname-name (pathname "/foo/bar/baz.txt")) ; ==> "baz"
(pathname-type (pathname "/foo/bar/baz.txt")) ; ==> "txt"

;; The other functions -- PATHNAME-HOST, PATHNAME-DEVICE, and PATHNAME-VERSION -- allow you to
;; get at the other three pathname components, though they're unlikely to have interesting values
;; on Unix. On Windows either PATHNAME-HOST or PATHNAME-DEVICE will return the drive letter.


;; The functions below translate a pathname back to a namestring
(namestring #p"/foo/bar/baz.txt") ; ==> "/foo/bar/baz.txt"
(directory-namestring #p"/foo/bar/baz.txt") ; ==> "/foo/bar/"
(file-namestring #p"/foo/bar/baz.txt")

;;; Constructing New Pathnames
;; You can construct arbitrary pathnames using the MAKE-PATHNAME function. It takes one keyword
;; argument for each pathname component and returns a pathname with any supplied components filled
;; in and the rest NIL.

(make-pathname
 :directory '(:absolute "foo" "bar")
 :name "baz"
 :type "txt") ; ==> #p"/foo/bar/baz.txt"

(make-pathname :device "c"
	       :directory '(:relative "data-sets")
	       :name "pr92-monster-maze"
	       :type "dat")
