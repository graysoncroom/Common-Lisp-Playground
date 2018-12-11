;;; vectors

;; Fixed-size vectors
(vector)
(vector 1)
(vector 1 2)
#()
#(1)
#(1 2)

;; General vector creation syntax
(make-array 5 :initial-element nil)

(defparameter *x* (make-array 5 :fill-pointer 0))

(vector-push 'a *x*)
(vector-push 'b *x*)
(vector-push 'c *x*)
(vector-pop *x*)

;; Resizable vector
;; * vector-push-extend <--> vector-push
;; works like vector-push but will automatically expand the array if
;; you try to push an element onto a full vector

(defparameter *array* (make-array 5 :fill-pointer 0 :adjustable t))

(defparameter *x* (vector 1 2 3))
(length *x*) ; ==> 3
(elt *x* 0) ; ==> 1
(elt *x* 1) ; ==> 2
(elt *x* 2) ; ==> 3
(elt *x* 3) ; ==> error

(setf (elt *x* 0) 10) ; *x* ==> #(10 2 3)

#|
(count item sequence) ==> number of times item appears in sequence
(find item sequence) ==> item or nil
(position item sequence) ==> index of item in sequence or nil
(remove item sequence) ==> sequence with instances of item removed
(substitute new-item item sequence) ==> replace all instances of item with new-item
|#

(count 1 #(1 2 1 2 4 1 2 1 1 4 5 6 8 1 1))
(remove 1 #(1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1))
(remove 1 '(1 2 1 2 1 1 1 3 4 2 1))
(remove #\a "foobarbaz") ; ==> "foobrbz"
(substitute 10 1 #(1 2 1 2 1 3 1 2 3 4))
(substitute 10 1 #(1 2 1 2 3 1 2 3 4))
(substitute #\x #\b "foobarbaz") ; ==> "fooxarxaz"
(find 1 #(1 2 1 2 3 1 2 3 4)) ; ==> 1
(find 10 #(1 2 1 2 3 1 2 3 4)) ; ==> nil
(position 1 #(1 2 1 2 3 1 2 3 4)) ; ==> 0

(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'car) ; ==> (C 30)
(count "foo" #("foo" "bar" "baz") :test #'string=) ; ==> 1

#| To limit the effects of these functions to a particular subsequence
of the sequence argument, you can provide bounding indices with :start
and :end arguments. Passing nil for :end or omitting it is the same
as specifying the length of the sequence.
-> :start is inclusive
-> :end is exclusive
-> [start, end)
|#

(position #\b "foobarbaz" :start 4) ; ==> 6
(position #\b "foobarbaz" :start 4 :end 6) ; ==> nil

(position #\b "foobarbaz" :start 7 :from-end t) ; ==> nil
(position #\b "foobarbaz" :from-end t) ; ==> 6
(position #\b "foobarbaz" :end 6 :from-end t) ; ==> 3

(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first) ; ==> (A 10)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ; ==> (A 30)

#| REMOVE and SUBSTITUTE can be used in conjunction with the keyword
parameters :from-end and :count
|#

(remove #\a "foobarbaz" :count 2) ; ==> "foobrbz"
(remove #\a "foobarbaz" :count 1) ; ==> "foobrbaz"
(remove #\a "foobarbaz" :count 1 :from-end t) ; ==> "foobarbz"

(defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
(defun verbose-car (x) (format t "Looking at ~s~%" x) (car x))

(count 'a *v* :key #'verbose-car)
(count 'a *v* :key #'verbose-car :from-end t)

#|
:test Two-argument function used to compare item (or value extracted
      by :key function
:key One-argument function to extract key value from actual sequence element
     NIL means use element as is
:start Starting index (inclusive) of subsequence
:end Ending index (exclusive) of subsequence
     NIL indicates end of sequence
:from-end If true, the sequence will be traversed in reverse order, 
          from end to start
:count Number indicating the number of elements to remove or substitute
       or NIL to indicate all (REMOVE and SUBSTITUTE only).
|#

#| For each of the functions just discussed, Common Lisp provides 
two higher-order function variants that, in the place of the item 
argument, take a function to be called on each element of the sequence.

One set of variants are named the same as the basic function with
an -IF appended. These function count, find, remove, and substitute elements
of the sequence for which the function argument returns true.

The other set of variants are named with an -IF-NOT suffix and
count, find, remove, and substitute elements for which the function argument
does NOT return true.
|#

(count-if #'evenp #(1 2 3 4 5)) ; ==> 2
(count-if-not #'evenp #(1 2 3 4 5)) ; ==> 3
(position-if #'digit-char-p "abcd0001") ; ==> 4
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
	       #("foo" "bar" "baz" "foom")) ; ==> #("foo" "foom")

;; REMOVE-IF-NOT returns the elements that satisfy the predicate

;; The -IF and -IF-NOT variants accept all the same keyword arguments as
;; their vanilla counterparts except for :test, which isn't needed since the
;; main argument is already a function

(remove-duplicates #(1 2 1 2 3 1 2 3 4))

(reverse '(1 2 3)) ; ==> '(3 2 1)
(reverse #(1 2 3)) ; ==> #(3 2 1)
(reverse "123") ; ==> "321"

(defun foo (lst) (setf (caar lst) 100))
(defparameter *zz* '((1 2 3) (4 5 6) (7 8 9)))

;; Side note: Functions arguments are always pass by value

;;; Whole sequence Manipulations
