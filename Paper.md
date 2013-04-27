# Background #
 In January of 2001, Yee and Rossum proposed a new iterator interface for objects in Python.
The proposed implementation was an itarator object that allows programmers to loop over objects 
more easily and elegantly.  The overarching goal of the proposal can be summarized as follows: performance 
enhancements for pbject iteration with respect to dictionaries, files, lists, and other objects implemented
as collections and sequences. 

## Prior to Iterators ##

## Object Iterators ##
  The iterator proposed in PEP 234 added new memory space "slots" for the next item in the sequence and 
for a constructor that creates a new iterator for the object.  The first is called tp_iternext and it 
uses the PyIter_Next() method to obtain the next element.  This slot should only be present for objects with 
iterators.  The second is called tp_iter, which is used to "request" the iterator and can be present in any object.
  tp/_iternext contains the return value from the method PyIter/_Next(), which is either the next element in the collection 
object (Py Object *) or Null.  PyIter-Next() is a higher order function that takes a Python iterator object as its argument.  
In the below example, the programmer declares two python objects; one for the collection items and one for the iterator.  
The iterator is then checked for a Null value (meaning some error or empty).  The code then proceeds with a while loop
over the iterator using the PyIter_Next() higher order function:
```python
PyObject *iterator = PyObject_GetIter(obj);
PyObject *item;

if (iterator == NULL) {
    /* propagate error */
}

while (item = PyIter_Next(iterator)) {
    /* do something with item */
}
```
(reference: [http://docs.python.org/2/c-api/iter.html]).  In the Principles of Programming Languages lectures this semester,
we learned that higher order functions are important in the functional programming paradigm.  For loops and traditional 
iteration is normally associated with imperative programming, but using functions as data (and focusing on function calls
to drive the program flow) is in the spirit of the functional programming paradigm.
  The Null return value of PyIter_Next() simply signifies that there are no more items, or an error occurred.  These Null return 
possibilities are much simpler than those of the tp_iternext slot.  For the latter, The PEP also called for a new 
exception called "StopIteration" to signal the exhaustion of a list or collection, subsequently yielding the Null return
value.  The PyIter_Next() function actually clears this exception, making the Null return value easier to understand.
 


# The Proposal #
## How Was it Implemented and Why? ##
 Understanding the difference between something that is iterable and an iterator is crucial to understanding how for loops work. 
 A container is said to be iterable if it has the '__iter__' method defined.
 
 An iterator is defined as an object that supports the iterator protocol. This basically means that the following two methods need 
 to be defined:
 
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;It has an '__iter__' method defined which returns itself.
  
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;It has a next method defined (__next__ in Python 3.x) which returns the next value every time the next method is invoked on it.

 As an example, consider a list: A list is iterable, but a list is not its own iterator.  The iterator of a list is actually a listiterator object. A listiterator 
 is its own iterator.
```
>>> a = [1, 2, 3, 4]
>>> # a list is iterable because it has the __iter__ method
>>> a.__iter__
<method-wrapper '__iter__' of list object at 0x014E5D78>
>>> # However a list does not have the next method, so it's not an iterator
>>> a.next
AttributeError: 'list' object has no attribute 'next'
>>> # a list is not its own iterator
>>> iter(a) is a
False
```
(Source=[http://www.shutupandship.com/2012/01/understanding-python-iterables-and.html])

  This propsoal modified Python's API, defining a new built-in function iter(). It could be called in one of two ways:
  
  1: iter(obj) calls PyObject_GetIter(obj) or 
  
  2: iter(callable, sentinel)
  
  This second instance returns a special kind of iterator that calls the callable to produce a new value, and compares the 
return value to the sentinel value. Iteration is terminated if the return value equals the sentinel. If they are not equal,
the return value is returned as the next value from the iterator. Alternatively, the aforementioned "StopIteration" exception
can be called to terminate the iteration. Iterator objects returned by either of these iter() calls will have a next() method. If this next() method does not return
the next value in the iteration or a StopIteration, the iterator should not terminate, but propagate an error.

 The author of the proposal lists benefits of implementing all parts of the proposal:
 >   1. It provides an extensible iterator interface.
 >   2. It allows performance enhancements to list iteration.
 >   3. It allows big performance enhancements to dictionary iteration.
 >   4. It allows one to provide an interface for just iteration without pretending to provide random access to elements.
 >   5. It is backward-compatible with all existing user-defined classes and extension objects that emulate sequences 
 and mappings, even mappings that only implement a subset of {__getitem__, keys, values, items}.
 >   6. It makes code iterating over non-sequence collections more concise and readable.

## Proposed Revisions ##
The proposal includes three distinct sections.  Each one addresses an aspect of the python programming language that 
would change as a result of the proposal's implementation.  First, a basic protocol for the iterator objects is presented.
The second and third sections discuss the iterators in the context of maps and sequences respectively.

After initially reading the proposal, many in the Python community questioned exactly how the three sections 
fit into the proposal as a whole.  The authors were quick to point out that the each section is a natural corollary of 
the one before it.  The authors wrote that the sections by themselves seemed to have "loose ends," but the entirety of 
the proposal had a quality of "closure."  With respect to maps, for example, the authors asked: "If we can iterate over 
two of the basic collection types, why can't we iterate over the third type, dictionaries?" 
(Source=[http://tech.groups.yahoo.com/group/python-iter/message/11])
# Conclusion #
