object Lab2 {
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
   * Kevin Barry
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
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
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case _ => throw new UnsupportedOperationException
    }
  }
    
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case _ if (isValue(e)) => e
 
      /* Inductive Cases */
      case Print(e1) => println(eval(env, e1)); Undefined
     
      case Binary(Plus, N(e1), N(e2)) => (N(e1 + e2))
      case Binary(Minus, N(e1), N(e2)) => (N(e1 - e2))
      case Binary(Times, N(e1), N(e2)) => (N(e1 * e2))
      case Binary(Div, N(e1), N(e2)) => (N(e1 / e2))
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => eval(env, B(v1 == v2))

      case Binary(Ne, e1, e2) => eval(env, e1)
      case Binary(Lt, N(n1), N(n2)) => eval(env, B(n1 < n2))
      case Binary(Gt, N(n1), N(n2)) => eval(env, B(n1 > n2))
      case Binary(Le, N(n1), N(n2)) => eval(env, B(n1 <= n2))
      case Binary(Ge, N(n1), N(n2)) => eval(env, B(n1 >= n2))
     
      case Binary(And, B(true), e2) => eval(env, e2)
      case Binary(And, B(false), _) => eval(env, B(false))
      case Binary(Or, B(true), _) => eval(env, B(true))
      case Binary(Or, B(false), e2) => eval(env, e2)

      case Binary(Seq, v1, e2) if isValue(v1) => eval(env, e2)
      
      case Unary(Neg, N(n1)) => eval(env, N(-n1))
      case Unary(Not, B(b1)) => eval(env, B(!b1))

      case If(B(true), e2, e3) => eval(env, e2)
      case If(B(false), e2, e3) => eval(env, e3)
      case If(e1, e2, e3) => eval(env, If(e1, e2, e3))

      case _ => throw new UnsupportedOperationException
  }
}
      
  def evaluate(e: Expr): Expr = eval(emp, e)
    
}
