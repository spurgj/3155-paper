object Lab3 {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * Justin Spurgeon
   * 
   * Partner: Thomas Dressler
   * Collaborators: Adnan Al-Sannaa
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
      case B(false) => 0
      case B(true) => 1
      case Undefined => Double.NaN
      case S(s) => try s.toDouble catch { case _ => Double.NaN }
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) if ((n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN()) => false
      case N(_) => true
      case B(b) => b
      case Undefined => false
      case S("") => false
      case S(_) => true
      case Function(_, _, _) => true
    }
  }
  
  def toString(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(_, _, _) => "function"
    }
  }
  
  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * This code is a reference implementation of JavaScripty without
   * strings and functions (i.e., Lab 2).  You are to welcome to
   * replace it with your code from Lab 2.
   */
  def eval(env: Env, e: Expr): Expr = {
    def eToN(e: Expr): Double = toNumber(eval(env, e))
    def eToB(e: Expr): Boolean = toBoolean(eval(env, e))
    def eToVal(e: Expr): Expr = eval(env, e)
    e match {
      /* Base Cases */
      case _ if (isValue(e)) => e
      case Var(x) => get(env, x)

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      
      case Unary(Neg, e1) => N(- eToN(e1))
      case Unary(Not, e1) => B(! eToB(e1))
      									  				
      case Binary(Plus, e1, e2) => (eToVal(e1), eToVal(e2)) match {
    	//First check if expressions are strings
        //A possible mistake is whether or not to use eToN() because it recurses through eval() again.
      	case (S(s1), v1) => S(s1 + toString(v1))
        case (v1, S(s2)) => S(toString(v1) + s2)       
        case _ => N(eToN(e1) + eToN(e2))  	//Both expressions are numbers.
      }
        								
      case Binary(Minus, e1, e2) => N(eToN(e1) - eToN(e2))
      case Binary(Times, e1, e2) => N(eToN(e1) * eToN(e2))
      case Binary(Div, e1, e2) => N(eToN(e1) / eToN(e2))
      
      case Binary(Eq, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (Function(_, _, _), _) => throw new DynamicTypeError (Binary(Eq, e1, e2))
      	case (_, Function(_, _, _)) => throw new DynamicTypeError (Binary(Eq, e1, e2))
		case (v1, S(v2)) => B(eToVal(v1) == S(v2))
		case (S(v1), v2) => B(S(v1) == eToVal(v2))
        case _ => B(eToVal(e1) == eToVal(e2))
      }
      case Binary(Ne, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (Function(_, _, _), _) => throw new DynamicTypeError (Binary(Eq, e1, e2))
      	case (_, Function(_, _, _)) => throw new DynamicTypeError (Binary(Eq, e1, e2))
		case (v1, S(v2)) => B(eToVal(v1) != S(v2))
		case (S(v1), v2) => B(S(v1) != eToVal(v2))
        case _ => B(eToVal(e1) != eToVal(e2))
      }
      
      case Binary(Lt, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        //If we encounter a string, the other expression expression is converted to a string for the comparison
        case (S(s1), v2) => B(s1 < toString(v2))
        case (v1, S(s2)) => B(toString(v1) < s2)
        case _ => B(eToN(e1) < eToN(e2))
      }
      case Binary(Le, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (S(s1), v2) => B(s1 <= toString(v2))
        case (v1, S(s2)) => B(toString(v1) <= s2)
        case _ => B(eToN(e1) <= eToN(e2))   
      }												
      case Binary(Gt, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (S(s1), v2) => B(s1 > toString(v2))
        case (v1, S(s2)) => B(toString(v1) > s2)
        case _ => B(eToN(e1) > eToN(e2))    
      }										
      case Binary(Ge, e1, e2) => (eToVal(e1), eToVal(e2)) match { 
        case (S(s1), v2) => B(s1 >= toString(v2))
        case (v1, S(s2)) => B(toString(v1) >= s2)
      	case _ => B(eToN(e1) >= eToN(e2))  
      }										
      case Binary(And, e1, e2) => if (eToB(e1)) eToVal(e2) else B(false)
      case Binary(Or, e1, e2) => if (eToB(e1)) B(true) else eToVal(e2)
      																	
      case Binary(Seq, e1, e2) => eToVal(e1); eToVal(e2)
         					
      case If(e1, e2, e3) => {
        if (eToB(e1)) eToVal(e2)
        else eToVal(e3)
      }									
    
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)
      											
      case Call(e1, e2) => eval(env, e1) match {
        case Function(Some(p), x2, e3) => {
          eval(extend(extend(env, p, (eval(env, e1))), x2, eToVal(e2)), e3)
        } 
      	case Function(None, x, e3) => eval(extend(env, x, eToVal(e2)), e3)                				
        case _ => throw new DynamicTypeError (Call(e1, e2))
      }							
      									
      case _ => throw new UnsupportedOperationException
    }
  }
    
  def evaluate(e: Expr): Expr = eval(emp, e)
  
  
  /* Small-Step Interpreter with Static Scoping */
	
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    /* Simple helper that calls substitute on an expression
     * with the input value v and variable name x. */
    def subst(e: Expr): Expr = substitute(e, v, x)
    /* Body */
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))      
      case Var(z) if (z==x) => v 
      case Var(y) => Var(y)
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))      
      case Function(p, z, e1) if (Some(x)==p || x==z) => Function(p, z, e1)
      case Function(p, z, e2) => Function(p, z, subst(e2))      
      case Call(e1, e2) => Call(subst(e1), subst(e2))
      case ConstDecl(z, e1, e2) if (x==z) => ConstDecl(z, subst(e1), e2)
      case ConstDecl(z, e1, e2) => ConstDecl(z, subst(e1), subst(e2))
      
      case _ => throw new UnsupportedOperationException
    }
  }
  
  
  def step(e: Expr): Expr = {
    require(!isValue(e))
    e match {
      /* Base Cases: Do Rules */

      
      case Print(v1) if (isValue(v1)) => println(pretty(v1)); Undefined
      case Unary(Neg, v1) if (isValue(v1)) => N(-toNumber(v1)) 
      case Unary(Not, v1) if (isValue(v1)) => B(!toBoolean(v1))
      
      
      case Binary(Plus, v1, v2) if (isValue(v1) && isValue(v2)) => (v1, v2) match {
        case (S(v1), v2) => S(v1 + toString(v2))
        case (v1, S(v2)) => S(toString(v1) + v2)
        case (v1, v2) => N(toNumber(v1) + toNumber(v2)) 
      }
      
      //Arithmetic Do Rules
      case Binary(Minus, N(v1), N(v2)) if (isValue(N(v1)) && isValue(N(v2))) => N(v1 - v2)
      case Binary(Times, N(v1), N(v2)) if (isValue(N(v1)) && isValue(N(v2))) => N(v1 * v2)
      case Binary(Div, N(v1), N(v2)) if (isValue(N(v1)) && isValue(N(v2))) => N(v1 / v2)

      //Inequality Operators with numbers, Do Rules 
      case Binary(Lt, N(v1), N(v2)) if (isValue(N(v1)) && isValue(N(v2))) => B(v1 < v2)
      case Binary(Le, N(v1), N(v2)) if (isValue(N(v1)) && isValue(N(v2))) => B(v1 <= v2)
      case Binary(Gt, N(v1), N(v2)) if (isValue(N(v1)) && isValue(N(v2))) => B(v1 > v2)
      case Binary(Ge, N(v1), N(v2)) if (isValue(N(v1)) && isValue(N(v2))) => B(v1 >= v2)

      //Inequality Operators with strings, Do Rules
      case Binary(Lt, S(v1), v2) if (isValue(S(v1)) && isValue(v2)) => B(v1 < toString(v2))
      case Binary(Le, S(v1), v2) if (isValue(S(v1)) && isValue(v2)) => B(v1 <= toString(v2))
      case Binary(Gt, S(v1), v2) if (isValue(S(v1)) && isValue(v2)) => B(v1 > toString(v2))
      case Binary(Ge, S(v1), v2) if (isValue(S(v1)) && isValue(v2)) => B(v1 >= toString(v2))
      case Binary(Lt, v1, S(v2)) if (isValue(v1) && isValue(S(v2))) => B(toString(v1) < v2)
      case Binary(Le, v1, S(v2)) if (isValue(v1) && isValue(S(v2))) => B(toString(v1) <= v2)
      case Binary(Gt, v1, S(v2)) if (isValue(v1) && isValue(S(v2))) => B(toString(v1) > v2)
      case Binary(Ge, v1, S(v2)) if (isValue(v1) && isValue(S(v2))) => B(toString(v1) >= v2)

      //Equal, Not Equal Do Rules
     
      case Binary(Eq, v1, v2) if (isValue(v1) && isValue(v2)) => (v1, v2) match {
        case (Function(_,_,_), v2) =>  throw new DynamicTypeError(e)
        case (v1, Function(_,_,_)) =>  throw new DynamicTypeError(e)
        case (S(v1), v2) => B(S(v1) == toString(v2))
        case (v1, S(v2)) => B(toString(v1) == S(v2))
        case _ => B(v1 == v2)
      }
      case Binary(Ne, v1, v2) if (isValue(v1) && isValue(v2)) => (v1, v2) match {
        case (Function(_,_,_), v2) =>  throw new DynamicTypeError(e)
        case (v1, Function(_,_,_)) =>  throw new DynamicTypeError(e)
        case (S(v1), v2) => B(S(v1) != toString(v2))
        case (v1, S(v2)) => B(toString(v1) != S(v2))
        case _ => B(v1 != v2)
      } 
      //And (&&), Or(||) Do Rules
      case Binary(And, v1, e2) if (isValue(v1))=> {
        if(toBoolean(v1)) e2	
        else B(false)  
      }
      case Binary(Or, v1, e2) if (isValue(v1))=> {
        if(toBoolean(v1)) B(true)	
        else e2
      }
      
      
      
      
      //Turnary If and constant declaration Do Rules
      case If(v1, e2, e3) if (isValue(v1)) => {
        if(toBoolean(v1)) e2
        else e3
      }
      case ConstDecl(x, v1, e2) if (isValue(v1)) => substitute(e2, v1, x)
      case Call(v1, v2) if (isValue(v1) && isValue(v2)) => (v1, v2) match {
        case (Function(None, x, e), v2) => substitute(e, v2, x)
        case (Function(Some(x1), x2, e), v2) => substitute(substitute(e, v1, x1), v2, x2)
        case _ => throw new DynamicTypeError(e)
      } 
      
      
           
      /* 
       * 
       * Inductive Cases: Search Rules 
       * 
       */
      case Print(e1) => Print(step(e1))
      case Unary(uop, e1) => Unary(uop, step(e1)) 

      case ConstDecl(x1, e2, e3) => ConstDecl(x1, step(e2), e3)

      case Binary(Seq, v1, e2) if (isValue(v1)) => e2
      
      case Call(v1, e2) if(isValue(v1)) => (v1, e2) match {
      	case (Function(p, x2, e3), e2) => Call(v1, step(e2))
        case _ => throw new DynamicTypeError(e)
      }  
      case Call(e1, e2) => Call(step(e1), e2)
      //case Binary(bop, v1, e2) if(isValue(v1)) => step(Binary(bop, v1, step(e2)))
      
      /*case Binary(Plus, v1, e2) if (isValue(v1)) => step(v1+step(e2))
      case Binary(Minus, v1, e2) if (isValue(v1)) => step(v1-step(e2))
      case Binary(Times, v1, e2) if (isValue(v1)) => step(v1*step(e2))
      case Binary(Div, v1, e2) if (isValue(v1)) => step(v1/step(e2))
         
      case Binary(Lt, v1, e2) if (isValue(v1)) => step(v1<step(e2))
      case Binary(Le, v1, e2) if (isValue(v1)) => step(v1<=step(e2))
      case Binary(Gt, v1, e2) if (isValue(v1)) => step(v1>step(e2))
      case Binary(Ge, v1, e2) if (isValue(v1)) => step(v1>=step(e2))
      */
      case Binary(bop, v1, e2) if(isValue(v1)) => Binary(bop, v1, step(e2))
      
      case Binary(And, e1, e2) => Binary(And, step(e1), e2)
      case Binary(Or, e1, e2) => Binary(Or, step(e1), e2)
      
      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)
      
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def iterateStep(e: Expr): Expr =
    if (isValue(e)) e else iterateStep(step(e))
    
}