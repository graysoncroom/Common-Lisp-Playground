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

#| MAPCAN and MAPCON work like MAPCAR and MAPLIST except for the 
way they build up their result. While MAPCAR and MAPLIST build up 
a completely new list to hold the results of the function calls, 
MAPCAN and MAPCON build their result by splicing together the 
results--which must be lists--as if by NCONC. Thus, each function 
invocation can provide any number of elements to be included in 
the result. MAPCAN, like MAPCAR, passes the elements of the list
to the mapped functionwhile MAPCON, like MAPLIST, passes the cons cells.
|#

;; MAPC and MAPL are control constructs disguised as
;; functions--they simply return their first list argument,
;; so they're useful only when the side effects of the mapped
;; function do something interesting.
;; MAPC is the cousin of MAPCAR and MAPCAN
;; while MAPL is the MAPLIST/MAPCON family.

;; MAPCAR <--> MAPCAN <--> MAPC
;; MAPLIST <--> MAPCON <--> MAPL

(mapc #'+ '(1 2 3)) ; ==> (1 2 3)
(mapc #'print '(1 2 3))
(mapl #'print '(1 2 3))

(let ((lst '(0 1 2 3 4 5 6 7 8 9)))
  (mapcan #'(lambda (x) (if (= x 5) nil (list x))) lst))
;; this is the same as
(let ((lst '(0 1 2 3 4 5 6 7 8 9)))
  (remove 10 lst))
;; which is the same as
(let ((lst '(0 1 2 3 4 5 6 7 8 9)))
  (mapcon #'(lambda (x) (if (= (car x) 5) nil (list (car x)))) lst))


#| COPY-LIST doesn't copy the sublists (1 2), (3 4), or (5 6) in the list
((1 2) (3 4) (5 6)).
COPY-TREE, on the other hand, makes a new cons cell for each of the
cons cells in list and links them together in the same structure.
|#

;;; TREES

;; The function SUBST, like SUBSTITUTE, takes a new item, and old item,
;; and a tree (as opposed to a sequence), along with :key and :test keyword
;; arguments, and it returns a new tree with the same shape as the
;; original tree but with all instances of the old item replaced with
;; the new item. For example:
(subst 10 1 '(1 2 (3 2 1) ((1 1) (2 2))))

#| SUBST-IF is analogous to SUBSTITUTE-IF.
|#
(subst-if 0 #'evenp '(1 2 (3 2 1) ((1 1) (2 2))))
(subst-if-not 0 #'listp '(1 2 (3 2 1) ((1 1) (2 2))))
(substitute-if #\2 #'alpha-char-p "a1b1c1d1e1f1g1h1i1j1k1l1m")

;;; SETS

#| Sets can also be implemented in terms of cons cells. In fact, you
can treat any list as a set--Common Lisp provides several functions for
performing set-theoretic operations on lists. However, you should bear in
mind that because of the way lists are structured, these operations get less
and less efficient the bigger the sets get.

That said, using the built-in set functions makes it easy to write
set-manipulation code. And for small sets they may well be more efficient
than the alternatives. If profiling shows you that these functions are a
performance bottleneck in you code, you can always replace the lists with
sets built on top of hash tables or bit vectors.

To build up a set, you can use the function ADJOIN. ADJOIN takes an item and
a list representing a set and returns a list represeting the set containing
the item and all the items in the original set. To determine whether the
item is present, it must scan the list; if the item isn't found, ADJOIN
creates a new cons cell holding the item and pointing to the original list
and returns it. Otherwise, it returns the original list.

ADJOIN also takes :key and :test keyword arguments, which are used when
determining whether the item is present in the original list.
Like CONS, ADJOIN has no effect on the original list--if you want to
modify a particular list, you need to assign the value returned by
ADJOIN to the place where the list came from. The modify macro
PUSHNEW does this for you automatically.
|#
(defparameter *set* ())

(adjoin 1 *set*) ; ==> (1)
(setf *set* (adjoin 1 *set)) ; *set* ==> (1)
(pushnew 2 *set*) ; *set* ==> (2 1)
(pushnew 2 *set*) ; *set* ==> (2 1)

#| You can test whether a given item is in a set with MEMBER and the
related functions MEMBER-IF and MEMBER-IF-NOT. These functions are similar
to the sequence functions FIND, FIND-IF, and FIND-IF-NOT except they can
be used only with lists. And instead of returning the item when it's present,
they return the cons cell containing the item--in other words, the sublist
starting with the desired item. When the desired item isn't present in the
list, all three functions return NIL.

The remaining set-theoretic functions provide bulk operations:
INTERSECTION, UNION, SET-DIFFERENCE, SET-EXCLUSIVE-OR. Each of these
functions takes two lists and :key and :test keyword arguments and returns
a new list representing the set resulting from performing the appropriate 
set-theoretic operation on the two lists: 
    INTERSECTION returns a list containing all the elements 
found in both arguments.
    UNION returns a list containing one instance of each unique element
found in the two arguments.
    SET-DIFFERENCE returns a list containing all the elements from the
first argument that don't appear in the second argument.
    SET-EXCLUSIVE-OR returns a list containing those elements appearing
in only one or the other of the two argument lists but not in both.

Each of these functions also has a recycling counterpart whose name is the
same except with an N prefix.

Finally, the function SUBSETP takes two lists and the usual :key and :test
keyword arguments and returns true if the first list is a subset of the
second--if every element in the first list is also present in the
second list. The order of the elements in the lists does not matter.
|#
(subsetp '(3 2 1) '(1 2 3 4)) ; ==> t
(subsetp '(1 2 3 4) '(3 2 1)) ; ==> nil

;;; Lookup Tables: Alists and Plists

#| In addition to trees and sets, you can build tables that map
keys to values out of cons cells. Two flavors of cons-based lookup
tables are comonly used, both of which I've mentioned in passing in
previous chapters. They're association lists, also called alists, and
property lists, also known as plists. While you wouldn't use either
alists or plists for large tables--for that you'd use a hash table--it's
worth knowing how to work with them both because for small tables they can
be more efficient than hash tables and because they have some useful
properties of their own.

An alist is a data structure that maps keys to values and also supports
reverse lookups, finding the key when given a value. Alists also support
adding key/value mappings that shadow existing mappings in such a way that
the shadowing mapping can later be removed and the original mappings
exposed again.

Under the covers, an alist is essentially a list whose elements are
themselves cons cells. Each element can be thought of as a key/value pair
with the key in the cons cell's CAR and the value in the CDR. For instance,
the following is an alist mapping A to 1, B to 2, and C to 3:

'((A . 1) (B . 2) (C . 3))

Unless the value in the CDR is a list, cons cells representing the key/value
pairs will be dotted pairs in s-expression notation.

The main lookup function for alists is assoc, which takes a key and an
alist and returns the first cons cell whose CAR matches they key or NIL if
no match is found.
|#
(assoc 'a '((a . 1) (b . 2) (c . 3))) ; ==> (A . 1)
(assoc 'c '((a . 1) (b . 2) (c . 3))) ; ==> (C . 3)
(assoc 'd '((a . 1) (b . 2) (c . 3))) ; ==> nil

;; To get the value corresponding to a given key, you simply pass the
;; result of ASSOC to CDR.
(cdr (assoc 'a '((a . 1) (b . 2) (c . 3))))

#| By default the key given is compared to the keys in the alist using
EQL, but you can change that with the standard combination of :key and
:test keyword arguments. For instance, if you wanted to use string keys,
you might write this: |#
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=)
