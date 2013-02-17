// Lab1 Driver

// Import your Lab1 object
import Lab1_kbarry._

object Lab1Driver {

  // The main function will be run when you type "scala Lab1Driver.scala"
  def main(args: Array[String]): Unit = {

      // You can print results of a function call directly
      // if the results are simple enough
      println("abs(1) = " + abs(1))
      println("xor(false, true) = " + xor(false, true))
      println("repeat('ab',3) = " + repeat("ab",3))
      println("sqrtStep(2.0, 1.0) = " + sqrtStep(2.0, 1.0))
      println("sqrtN(15.0, 1.0, 3) = " + sqrtN(15.0, 1.0, 3))
      println("sqrtErr(10.0, 1.0, 0.1) = " + sqrtErr(10.0, 1.0, 0.1))

      // Sometimes, we need to keep values around, so you can just
      // assign results to temporary values and print them that way

      val tree1 = Node(Node(Empty,3,Empty),5,Node(Empty,7,Empty))   // Very LISP like...
      println("tree1 is " + tree1)
      println("tree1 is repOk? " + repOk(tree1))
      
      // We're going to put the number 4 in our first tree, and then assign the
      // resulting tree to tree2
      val tree2 = insert(tree1, 4)  
      println("tree2 is " + tree2)
      println("tree1 is " + tree1)
      println("** NOTE:  tree1 should not have changed! **")

      // Check out what happens with deleteMin...
      val (tree3, lowest_Number) = deleteMin(tree2)    // Note how the tuple return value is handled
      println("deleteMin on tree2 gives " + tree3)
      println("and the minimum value was " + lowest_Number)
  
      // And delete a number from tree2 now
      val tree4 = delete(tree2, 5)
      println("deleting 5 from tree2 gives " + tree4) 
  }
}

  
