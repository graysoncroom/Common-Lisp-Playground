;; Append at work
(let* ((x '(1 2 3))
       (y (append '(-2 -1 0) (copy-seq x))))
  (setf (cadddr y) 100)
  x)

;; The N in NREVERSE stands for non-consing, meaning it doesn't need to
;; allocate any new cons cells.

;; Most recycling functions, like NREVERSE, have nondestructive counterparts
;; that compute the same result. In general, the recycling functions have
;; names that are the same as their non-destructive counterparts except with
;; a learding N. However, not all do, including several of the more commonly
;; used recycling functions such as NCONC, the recycling version of APPEND,
;; and DELETE, DELETE-IF, DELETE-IF-NOT, and DELETE-DUPLICATES, the
;; recycling versions of the REMOVE family of sequence functions.

(defun foo (lst)
  (setf (car lst) 100))

(let ((x '(1 2 3)))
  (foo x)
  x)

;;; Idomatic uses of recycling functions
(defun upto (max)
  (let ((result '()))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(upto 10) ; ==> (0 1 2 3 4 5 6 7 8 9 10)

(defparameter foo '(1 nil 2 nil 3 nil 4))
(setf foo (delete nil foo))

;; The PUSH/NREVERSE and SETF/DELETE idioms probably account for
;; 80 percent of the uses of recycling functions. Other uses are possible
;; but require keeping careful track of which functions return shared
;; structure and which do not.

;; In general, when manipulating lists, it's best to write your own code in
;; a functional style--your functions should depend only on the contents
;; of their list arguments and shouldn't modify them.
;; Following that rule will, of course, rule out using any destructive
;; functions, recycling or otherwise. Once you have your code working,
;; if profiling shows you need to optimize, you can replace nondestructive
;; list operations with their recycling counterparts but only if you're
;; certain the argument lists aren't referenced from anywhere else.

#|
LAST - Returns the last cons cell in a list. With an integer,
       argument returns the last n cons cells.
BUTLAST - Returns a copy of the list, excluding the last cons cell. With an
          integer argument, excludes the last n cells.
NBUTLASt - The recycling version of BUTLAST; may modify and return the
           argument list but has no reliable side effects.
LDIFF - Returns a copy of a list up to given cons cell.
TAILP - Returns true in a given object is a cons cell that's part
        of the structure of a list.
LIST* - Builds a list to hold all but the last of its arguments and then makes
        the last argument the cdr of the last cell in the list. In other
        words, a cross between list and append.
MAKE-LIST - Builds an n item list. The initial elements of the list are NIL
            or the value speciied with the :initial-element keyword argument.
REVAPPEND - Combination of REVERSE and APPEND; reverses first argument as 
            with REVERSE and then appends the second argument.
NRECONC - Recycling version of REVAPPEND; reverses first argument as if
          by NREVERSE and then appends the second argument. No reliable side
          effects.
CONSP - Predicate to test whether an object is a cons cell
ATOM - Predicate to test whether an object is either a cons cell or nil
LISTP - Predicate to test whether an object is either a cons cell or nil
NULL - Predicate to test whether an object is NIL. Functionally equivalent
       to NOT but stylistically preferable when testing for an empty list as
       opposed to boolean false
|#

(map 'list #'+ '(1 2 3) '(4 5 6)) ; ==> (5 7 9)

(mapcar #'(lambda (x) (* 2 x)) '(1 2 3)) ; ==> (2 4 6)
(mapcar #'+ '(1 2 3) '(10 20 30)) ; ==> (11 22 33)

(maplist #'print '(1 2 3 4 5 6))
(maplist #'(lambda (lst) (print (reduce #'+ lst))) '(1 2 3 4 5 6))

;; MAPCAR <--> MAPCAN <--> MAPC
;; MAPLIST <--> MAPCON <--> MAPL

#| MAPCAN and MAPCON work like MAPCAR and MAPLIST except for the way they build up their
result. While MAPCAR and MAPLIST build up a completely new list to hold the results of the
function calls, MAPCAN and MAPCON build their result by splicing together the results--which
must be lists--as if by NCONC. Thus, each function invocation can provide any number of 
elements to be included in the result. MAPCAN, like MAPCAR, passes the elements of the list
to the mapped functionwhile MAPCON, like MAPLIST, passes the cons cells.
|#

;; MAPC and MAPL are control constructs disguised as functions--they simply return their
;; first list argument, so they're useful only when the side effects of the mapped
;; function do something interesting. MAPC is the cousin of MAPCAR and MAPCAN while
;; MAPL is the MAPLIST/MAPCON family.

(mapc #'+ '(1 2 3)) ; ==> (1 2 3)
(mapc #'print '(1 2 3))
(mapl #'print '(1 2 3))
