object Lab2 {
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
   * Justin Spurgeon
   * 
   * Partner: None
   * Collaborators: Adnan Al-Sannaa
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
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
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(true) => 1
      case B(false) => 0
      case Undefined => Double.NaN
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(0) => false
      case N(Double.NaN) => false
      case N(n) => true
      case Undefined => false
      case _ => throw new UnsupportedOperationException
    }
  }
    
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case _ if (isValue(e)) => e
      
      /* Inductive Cases */
      case Print(e1) => println(eval(env, e1)); Undefined

      /* Const declaration */
      case ConstDecl(x, e1, e2) => x + N(toNumber(eval(env,e1))); N(toNumber(eval(env, e1)))

      /* const */
      case Var(x) => e

      /* - unary */
      case Unary(Neg, e1) => N(-toNumber(eval(env,e1)))

      /* ! */
      case Unary(Not, e1) => B(!toBoolean(eval(env,e1)))           
      
      /* + */
      case Binary(Plus, e1, e2) => N(toNumber(eval(env,e1)) + toNumber(eval(env,e2)))

      /* - binary */
      case Binary(Minus, e1, e2) => N(toNumber(eval(env,e1)) - toNumber(eval(env,e2)))
      
      /* * */
      case Binary(Times, e1, e2) => N(toNumber(eval(env,e1)) * toNumber(eval(env,e2)))
      
      /* / */
      case Binary(Div, e1, N(0)) => N(Double.PositiveInfinity)
      case Binary(Div, e1, e2) => N(toNumber(eval(env,e1)) / toNumber(e2))
      
      /* === */
      case Binary(Eq, e1, e2) => B(toNumber(eval(env,e1)) == toNumber(eval(env,e2))) 

      /* !== */
      case Binary(Ne, Undefined, Undefined) => B(false)
      case Binary(Ne, e1, e2) => B(toNumber(eval(env,e1)) != toNumber(eval(env,e2)))

      /* < */
      case Binary(Lt, e1, e2) => B(toNumber(eval(env,e1)) < toNumber(eval(env,e2)))

      /* <= */
      case Binary(Le, e1, e2) => B(toNumber(eval(env,e1)) <= toNumber(eval(env,e2)))

      /* > */
      case Binary(Gt, e1, e2) => B(toNumber(eval(env,e1)) > toNumber(eval(env,e2)))

      /* >= */
      case Binary(Ge, e1, e2) => B(toNumber(eval(env,e1)) >= toNumber(eval(env,e2)))

      /* && */
      case Binary(And, e1, e2) => B(toBoolean(eval(env,e1)) && toBoolean(eval(env,e2)))

      /* || */
     case Binary(Or, e1, e2) => B(toBoolean(eval(env,e1)) || toBoolean(eval(env,e2)))

      /* , */
      case Binary(Seq, e1, e2) => e1 ; eval(env, e2)

      /* if */
      //case If(e1, e2, e3) => (eval(env, e1) ? e2 : e3)

      case _ => throw new UnsupportedOperationException
    }
  }
      
  def evaluate(e: Expr): Expr = eval(emp, e)
    
}
