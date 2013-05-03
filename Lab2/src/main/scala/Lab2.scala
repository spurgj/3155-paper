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
      case B(b) => if (b) 1.0 else 0.0
      case Undefined => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => n != 0.0
      case Undefined => false
    }
  }
    
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case _ if (isValue(e)) => e
      case ConstDecl(x, e1, e2) => {
        eval(extend(env, x, eval(env, e1)), e2)
      }
      case Var(x) => get(env, x)
      /* Inductive Cases */
      case Print(e1) => println(eval(env, e1)); Undefined
     
      case Binary(bop, e1, e2) => bop match{
        case Plus => N(toNumber(eval(env, e1)) + toNumber(eval(env, e2)))
        case Minus => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))
        case Times => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))
        case Div => N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))
        
        case Eq => (eval(env, e1), eval(env, e2)) match {
          case (N(x1), N(x2)) => B(x1 == x2)
          case (B(p1), B(p2)) => B(p1 == p2)
          case (Undefined, Undefined) => B(true)
          case _ => B(false)
        }
        
        case Ne => B(!toBoolean(eval(env, Binary(Eq, e1, e2))))
        case Gt => B(toNumber(eval(env, e1)) > toNumber(eval(env, e2)))
        case Lt => B(toNumber(eval(env, e1)) < toNumber(eval(env, e2)))
        case Ge => B(toNumber(eval(env, e1)) >= toNumber(eval(env, e2)))
        case Le => B(toNumber(eval(env, e1)) <= toNumber(eval(env, e2)))
        
        case And => {
          val e1v = eval(env, e1)
          if (!toBoolean(e1v)) e1v else eval(env, e2)
        }
        case Or => {
          val e1v = eval(env, e1)
          if (toBoolean(e1v)) e1v else eval(env, e2)
        }
        case Seq => {
        eval(env, e1)
        eval(env, e2)
        }
      }
      
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
