# Lab 3 Write up #
## Kevin Barry - kbarry ##

## 2a. ##
Using sbt to evaluate the test case results in an answer of 0.0, but when evaluated in node.js, an answer of 3 is obtained. Under dynamic scoping, the program can access the global variables a1 and a2, allowing it to give a solution for any f(#). With static scoping, the variables a1 and a2 are not defined in the scope, so it returns 0 instead of an error.

## 3c. ##
The small step interpreter is deterministic. The way the small step interpreter works is step by step, and each step will have only **one** result. Thus, evaluation order is deterministic.

## 4a. ##

	def procedure():(Any,Any) => Unit = println("procedure")
 	def arg1():Any = println("arg1")
 	def arg2():Any = println("arg2")

Output from above code using procedure(arg1(),arg2()):
	procedure
	arg1
	arg2

i, ii. Based on the code above, scala evaluates the arguments for a procedure after evaluating the procedure itself (i.e left -> right).

## 4b. ##
Based on the semantics, e1 will be evaluated before e2. If e1 is a string, there is nothing to evaluate, so e2 will be evaluated. Otherwise, e1 will be converted into a number before e2.

## 5a. ##
Short circuit evaluation on an operator such as && can be useful if your second argument is an operation that will take a long time. For example, if your first argument is false, you don't want to complete the second operation because false && anything will evaluate to false, meaning a lot of time was wasted completing the second argument.

## 5b. ##
Scala does use short-circuit evaluation of boolean operators.

	def x() = {
		println("t")
		true
	}
	def y() = {
		println("f")
		false
	}

x() && y() && any number of x() or y()'s &&'d on will return false because x() && y() is false. 


## 5c. ##
Using the sematics for JavaScripty, e1 && e2 can short circuit. The second argument (e2) need not be looked at if e1 is false. It should not short circuit if e1 is true, as e2 needs to be examined as well.
