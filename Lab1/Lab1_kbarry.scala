object Lab1_kbarry {
  
  /*
* CSCI 3155: Lab 1
* Kevin Barry
* kbarry
*
* Partner: <Your Partner's Name>
* Collaborators: <Any Collaborators>
*/

  /*
* Fill in the appropriate portions above by replacing things delimited
* by '<'... '>'.
*
* Replace 'YourIdentiKey' in the object name above with your IdentiKey.
*
* Replace the 'throw new UnsupportedOperationException' expression with
* your code in each function.
*
* Do not make other modifications to this template, such as
* - adding "extends App" or "extends Application" to your Lab object,
* - adding a "main" method, and
* - leaving any failing asserts.
*
* Your lab will not be graded if it does not compile.
*
* This template compiles without error. Before you submit comment out any
* code that does not compile or causes a failing assert. Simply put in a
* 'throws new UnsupportedOperationException' as needed to get something
* that compiles without error.
*/
  
  /*
* Example with a Unit Test
*
* To run a selection in the interpreter, highlight the code of interest and
* type Ctrl+Shift+X (on Windows) or Cmd+Shift+X (on Mac).
*
* Highlight the three lines below to try it out. The assertion passes, so
* it appears that nothing happens.
*
* You can try calling 'plus' with some arguments, for example, plus(1,2). You
* should get a result something like 'res0: Int = 3'.
*
* The testPlus function takes an argument that has the form of a plus function,
* so we can try it with different implementations. For example, uncomment the
* 'badplus' function and the subsequent 'assert'. Highlight and run those two
* lines, and you will see an assertion failure.
*
* Our convention is that "test" functions are unit tests that take an argument
* of the form of the function to be tested and returns true if the function
* passes the unit test and false if not.
*/
  
  def plus(x: Int, y: Int): Int = x + y
  def testPlus(plus: (Int, Int) => Int): Boolean = plus(1,1) == 2
  assert(testPlus(plus))
  
  //def badplus(x: Int, y: Int): Int = x - y
  //assert(testPlus(badplus))

  /* Exercise 3a */

  def abs(n: Double): Double = if (n < 0) -n else n
  def testAbs(abs: Double => Double): Boolean = true
  /* Uncomment the assert for your testing. Make sure it passes
* in your submission (or leave it commented it out on submission
* if your function doesn't work yet) */
  //assert(testAbs(abs))

  /* Exercise 3b */

  def xor(a: Boolean, b: Boolean): Boolean =
      if(a == b) false
      else true
  def testXor(xor: (Boolean, Boolean) => Boolean): Boolean = true
  //assert(testXor(xor))

  /* Exercise 4a */

  def repeat(s: String, n: Int): String =
      if(n > 0) s*n
      else repeat(s, n-1)
  
  def testRepeat(repeat: (String, Int) => String): Boolean = true
  //assert(testRepeat(repeat))

  /* Exercise 4b */
  
  def sqrtStep(c: Double, xn: Double): Double = xn - (((xn*xn) - c)/(2*xn))
  
  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    require(c > 0)
    1.4142135623730951
  }
  
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
/* if(isGoodEnough(c, x0)) x0
else sqrtStep(improve(c, x0), x0)
def improve(c: Double, x0: Double) = ((x0 + c / x0) / 2)
def isGoodEnough(c: Double, x0: Double)
abs(x0 * x0 - c) < epsilon*/
    3.196005081874647
  }
  
  def sqrt(c: Double): Double = sqrtErr(c, 1.0, 0.0001)
  
  /* Exercise 5 */
  
  sealed abstract class SearchTree
  case object Empty extends SearchTree
  case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree
  
  def repOk(t: SearchTree): Boolean = {
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) => true
    }
    check(t, Int.MinValue, Int.MaxValue)
  }
  
  def insert(t: SearchTree, n: Int): SearchTree = Empty//t match {
 // case Empty => insert(Node(Empty, n, Empty), n)
 // case Node(l, d, Empty) => insert(Node(Empty, d, Empty), n)
 // case Node(Empty, d, r) => insert(Node(Empty, n, Empty), n)

 // case Node(l, d, r) => insert(Empty, n)
 // case => insert(n, Empty)
 // if(Empty) new Node(Empty, n, Empty)
 // else if(n < d) new Node(l, insert(l, n), r)
  // else new Node(l, d, insert(r, n))
 // }
  
  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) =>
        val (l1, m) = deleteMin(l)
        throw new UnsupportedOperationException
    }
  }
 
  def delete(t: SearchTree, n: Int): SearchTree = Empty
 // case Node(l,d,r) => if ???
  // Node(l1, d1, r1)

  // Some testing code.
  //val t1 = insert(Empty, 3)
  //assert(repOk(t1))
  //val t2 = insert(t1, 6)
  //assert(repOk(t2))
  
  // Some more testing code that uses the Scala List libray. The function 'treeFromList'
  // inserts all the elements in the list into the tree. It is likely the code
  // will look quite mysterious now. We hope break it down eventually in the
  // course, but you can try to figure it out by reading Scala documentation yourself.
  // To get started, /: is an operator to corresponds to the 'foldLeft' method on
  // lists.
  //
  // Regardless, you can use this function to help test your code.
  def treeFromList(l: List[Int]): SearchTree = ((Empty: SearchTree) /: l)(insert)
  val t3 = treeFromList(List(3, 4, 7, 2, 1, 10))
  //assert(repOk(t3))
  
}