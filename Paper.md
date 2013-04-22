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

## Proposed Revisions ##

# Conclusion #
