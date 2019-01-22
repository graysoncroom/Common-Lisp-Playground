;;;; Notes over Chapter 16 PCL

#| The langauge now generally considered the first object-oriented language, Simula, was
invented in the early 1960s, only a few years after McCarthy's first Lisp. However, object
orientation didn't really take off until the 1980s when the first widely available
version of Smalltalk was released, followed by the release of C++ a few years later. Smalltalk
took quite a bit of inspiration from Lisp and combined it with ideas from Simula to produce
a dynamic object-oriented language, while C++ combined Simula with C, another fairly static
language, to yield a static object-oriented language. This early split has led to much
confusion in the definition of object orientation. Folks who come from the C++
tradition tend to consider certain aspects of C++, such as strict data encapsulation, to be
key characteristics of object orientation. Folks from the Smalltalk tradition, however, consider
many features of C++ to be just that, features of C++, and not core to object orientation. Indeed,
Alan Kay, the inventor of Smalltalk, is reported to have said, "I invented the term object
oriented, and I can tell you that C++ wasn't what I had in mind." |#

#| Object Reorientation: Generic Functions
Because the invention of Lisp predated the rise of object-oriented programming by
a couple decades, new Lispers are sometimes surprised to discover what a thoroughly
object-oriented langauge Common Lisp is. Common Lisp's immediate predecessors were developed
at a time when object orientation was an exciting new idea and there were many experiments
with ways to incorporate the ideas of object orientation, especially as manifested in
Smalltalk, into Lisp. As part of the Common Lisp standardization, a synthesis of several of
these experiments emerged under the name Common Lisp Object System, or CLOS. The ANSI standard
incorporated CLOS into the language, so it no longer really makes sense to speak of the CLOS
as a seperate entity.

The CLOS contributed to Common Lisp range from those that can hardly be avoided to relatively
esoteric manifestations of Lisp's language-as-language-building-tool philosophy.
Complete coverage of all these features is beyond the scope of this book, but in this chapter and
the next I'll describe the bread-and-butter features and give an overview of Common Lisp's approach
to objects.

You should note at the outset that Common Lisp's object system offers a fairly different
embodiment of the principles of object orientation than many other languages. If you have a
deep understanding of the fundamental ideas behind object orientation, you'll likely
appreciate the particularly powerful and general way Common Lisp manifests those ideas. On the other
hand, if your experience with object orientation has been largely with a single language, you
may find Common Lisp's approach somewhat foreign; you should try to avoid assuming that
there's only one way for a language to support object orientation. If you have little
object-oriented programming experience, you should have no trouble understanding the explanations here,
though it may help to ignore the occasional comparisons to the way other languages do
things. 

There are those who reject the notion that Common Lisp is in fact object oriented at all. In particular,
folks who consider strict data encapsulation a key characteristic of object orientation -- usually
advocates of relatively static languages such as C++, Eiffel, or Java -- don't consider Common Lisp
to be properly object oriented. Of course, by that definition, Smalltalk, arguably one of the
original and purest object-oriented languages, isn't object oriented either. On the other hand, folks
who consider message passing to be the key to object orientation will also no be happy with the
claim that Common Lisp is object oriented since Common Lisp's generic function orientation
provides degrees of freedom not offered by pure message passing.
|#

#| Generic Functions and Classes 
The fundamental idea of object orientation is that a powerful way to organize a program is to
define data types and then associate operations with those data types. In particular, you want to
be able to invoke an operation and have the exact behavior determined by the type of the object
or objects on which the operation was invoked. The classic example used, seemingly by all
introductions to object orientation, is an operation draw that can be applied to objects
representing various geometric shapes. Different implementations of the draw operation can
be provided for drawing circles, triangles, and squares, and a call to draw will actually result
in drawing a circle, triangle, or square, depending on the type of the object to which the draw
operation is applied. the different implementations of draw are defined separately, and new versions
can be defined that daw other shapes without having to change the code of either the caller or any of
the other draw implementations. This feature of object orientation goes by the fancy Greek name
polymorphism, meaning "many forms," because a single conceptual operation, such as drawing an object,
can take many different concrete forms.

Common Lisp, like most object-oriented languages today, is class-based; all objects are instances of
a particular class. The class of an object determines its representation -- built-in classes such as
NUMBER and STRING have opaque representations accessible only via the standard functions for
manipulating those types, while instances of user-defined classes, as you'll see in the next chapter,
consist of named parts called slots.

(Prototype-based languages are the other style of object-oriented language. In these languages,
JavaScript being perhaps the most famous example, objects are created by cloning a prototypical
object. The clone can then be modified and used as a prototype for other objects.)

Classes are arranged in a hierarchy, a taxonomy for all objects. A class can be defined as a subclass
of other classes, called its superclasses. A class inherits part of its definition from its superclass
and instances of a class are also considered instances of the superclasses. In Common Lisp, the
hierarchy of classes has a single root, the class T, which is a direct or indirect superclass of 
every other class. Thus, every datum in Common Lisp is an instance of T. Common Lisp also supports
multiple inheritance -- a single class can have multiple direct superclasses.

(T the constant value and T the class have no particular relationship except they happen to have the
same name. T the value is a direct instance of the class SYMBOL and only indirectly an instance
of the class T)



|#
