3c. The evaluation is deterministic because the search rules explicitly state how an expression is evaluated.

4ai. Scala evaluates arguments before the methods that call them. In the following
example, method1 (the argument) prints before method2 (the calling method). This
code was executed using the command line Scala interpreter.

scala> def method1(): Int = { println(“method1 is running”) ; 1; }

scala> def method2(a: Int) = { println(“method2 is running”) }

scala> method2(method1)

method1 is running

method2 is running

4aii. Scala evaluates arguments from left to right. In the following example, arg1
prints before arg2, which in turn prints before arg3. The arguments are called in the
order 1, 2, 3, from left to right. This code was executed using the command line Scala
interpreter.

scala> def arg1(): Int = { println(“arg1 is running”) ; 1; }

scala> def arg2(): Int = { println(“arg2 is running”) ; 2; }

scala> def arg3(): Int = { println(“arg3 is running”) ; 3; }

scala> def method1(a: Int, b: Int, c: Int) = { println(“method1 is running”) }

scala> method1(arg1, arg2, arg3)

arg1is running

arg2is running

arg3is running

method1 is running

4b. DoPlusNumber clearly shows the first value being evaluated
before the second.

5a. Short-circuit evaluation can be useful in several situations. For instance, it
can prevent a second expression from running in a situation where it may cause
an error. Consider a number, x, and a function, divider(x), which takes x as an
argument. Divider(x) simply divides 5 by x. In the event that x is zero, a division by
zero error will be encountered. Short-circuit evaluation can be used to check if x is
zero and preventing divider(x) from running in that case. Pseudocode:

x=0
divider(int x) { return 5 / x }

if (x != 0 && divider(x) > 2) { performTask() }

This if statement executes without an error using short-circuit evaluation. If short-
circuit evaluation were not used, divider(x) would still run and generate an error.

5b. Scala uses short-circuit evaluation. In the following example, trueMeth() issues
a print statement and returns true, while falseMeth() issues a print statement
and returns false. Due to short-circuit evaluation, only the print statement from
falseMeth() runs. The following code was run using the command line Scala
interpreter.

scala> def falseMeth(): Boolean = { println(“falseMeth is running”); false;}

scala> def trueMeth(): Boolean = { println(“trueMeth is running”); true;}

scala> if(falseMeth == true && trueMeth == true) { println(“test”) }

falseMeth is running

trueMeth()’s print statement is never seen because it does not execute, due to short-
circuit evaluation.

5c. Based
on the order DoEquality is presented in (v1 bop v2) à b’, I would argue that
JavaScripty does use short-circuit evaluation in the case of e1 && e2. Wikipedia
confirms that JavaScript does use short-circuit evaluation for &&, but I’m not sure if
we can apply that directly to JavaScripty.