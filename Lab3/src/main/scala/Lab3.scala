object Lab3 {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * Kevin Barry - kbarry
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
      
      case Binary(Plus, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (S(_1), S(_2)) => S(_1 + _2)
        case (_1, S(_2)) => S(toString(_1) + _2)
        case (S(_1), _2) => S(_1 + toString(_2))
        case _ => N(eToN(e1) + eToN(e2))
      }
        
      case Binary(Minus, e1, e2) => N(eToN(e1) - eToN(e2))
      case Binary(Times, e1, e2) => N(eToN(e1) * eToN(e2))
      case Binary(Div, e1, e2) => N(eToN(e1) / eToN(e2))
      
       case Binary(Eq, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (S(_1), _2) => B(S(_1) == eToVal(_2))
        case (_1, S(_2)) => B(eToVal(_1) == S(_2))
        case _ => B(eToVal(e1) == eToVal(e2))
      }
      case Binary(Ne, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (S(_1), _2) => B(S(_1) != eToVal(_2))
        case (_1, S(_2)) => B(eToVal(_1) != S(_2))
        case _ => B(eToVal(e1) != eToVal(e2))
      }      
      case Binary(Gt, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (S(_1), _2) => B(_1 > toString(eToVal(_2)))
        case (_1, S(_2)) => B(toString(eToVal(_1)) > _2)
        case _ => B(eToN(e1) > eToN(e2))
      }
      case Binary(Ge, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (S(_1), _2) => B(_1 >= toString(eToVal(_2)))
        case (_1, S(_2)) => B(toString(eToVal(_1)) >= _2)
        case _ => B(eToN(e1) >= eToN(e2))
      }
      case Binary(Lt, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (S(_1), _2) => B(_1 < toString(eToVal(_2)))
        case (_1, S(_2)) => B(toString(eToVal(_1)) < _2)
        case _ => B(eToN(e1) < eToN(e2))
      }
      case Binary(Le, e1, e2) => (eval(env, e1), eval(env, e2)) match {
        case (S(_1), _2) => B(_1 <= toString(eToVal(_2)))
        case (_1, S(_2)) => B(toString(eToVal(_1)) <= _2)
        case _ => B(eToN(e1) <= eToN(e2))
      }    

      case Binary(And, e1, e2) => if (eToB(e1)) eToVal(e2) else B(false)
      case Binary(Or, e1, e2) => if (eToB(e1)) B(true) else eToVal(e2)
      
      case Binary(Seq, e1, e2) => eToVal(e1); eToVal(e2)
      
      case If(e1, e2, e3) => if (eToB(e1)) eToVal(e2) else eToVal(e3)
      
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)

      case Call(e1, e2) => {
        val fnct = eval(env, e1)
        fnct match {
          case Function(Some(f), x, e3) => {
            eval(extend(extend(env, f, fnct), x, eToVal(e2)), e3)
          }
          case Function(_, x, e3) => eval(extend(env, x, eToVal(e2)), e3)
          case _ => throw new DynamicTypeError(e)
        }
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
      
      case Var(s) if x == s => v
      case Var(q) => Var(q)
      
      case ConstDecl(y, e1, e2) if x == y => ConstDecl(y, subst(e1), e2)
      case ConstDecl(y, e1, e2) => ConstDecl(y, subst(e1), subst(e2))
      
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Call(e1, e2) => Call(subst(e1), subst(e2))
      
      case Function(p, y, e1) if Some(x) == p || x == y => Function(p, y, e1)
      case Function(p, y, e2) => Function(p, y, subst(e2))

      case _ => throw new UnsupportedOperationException
    }
  }
    
  def step(e: Expr): Expr = {
    require(!isValue(e))
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if (isValue(v1)) => println(pretty(v1)); Undefined

      case Unary(uop, v1) if (isValue(v1)) => uop match {
        case Neg => N(-toNumber(v1))
        case Not => B(!toBoolean(v1))
      }
      case Binary(bop, v1, v2) if (isValue(v1) && isValue(v2)) => bop match {
        case Plus => (v1, v2) match {
          case (S(a), x) => S(a + toString(x))
          case (y, S(b)) => S(toString(y) + b)
          case _ => N(toNumber(v1) + toNumber(v2))
        }
        case Minus => N(toNumber(v1) - toNumber(v2))
        case Times => N(toNumber(v1) * toNumber(v2))
        case Div => N(toNumber(v1) / toNumber(v2))
        case Eq => (v1, v2) match {
          case (v1, S(v2)) => B(toString(v1) == S(v2))
          case (S(v1), v2) => B(S(v1) == toString(v2))
          case(v1, Function(a, b, c)) => throw new DynamicTypeError(e)
          case(Function(a, b, c), v2) => throw new DynamicTypeError(e)
          case _ => B(v1 == v2)
        }
        case Ne => (v1, v2) match {
          case (v1, S(v2)) => B(toString(v1) != S(v2))
          case (S(v1), v2) => B(S(v1) != toString(v2))
          case _ => B(v1 != v2)
        }
        case Ge => (v1, v2) match {
          case (v1, S(v2)) => B(toString(v1) >= v2)
          case (S(v1), v2) => B(v1 >= toString(v2))
          case _ => B(toNumber(v1) >= toNumber(v2))
        }
        case Gt => (v1, v2) match {
          case (v1, S(v2)) => B(toString(v1) > v2)
          case (S(v1), v2) => B(v1 > toString(v2))
          case _ => B(toNumber(v1) > toNumber(v2))
        }
        case Le => (v1, v2) match {
          case (v1, S(v2)) => B(toString(v1) <= v2)
          case (S(v1), v2) => B(v1 <= toString(v2))
          case _ => B(toNumber(v1) <= toNumber(v2))
        }
        case Lt => (v1, v2) match {
          case (v1, S(v2)) => B(toString(v1) < v2)
          case (S(v1), v2) => B(v1 < toString(v2))
          case _ => B(toNumber(v1) < toNumber(v2))
        }
        case And => if (toBoolean(v1)) v2 else B(false)
        case Or => if (!toBoolean(v1)) v2 else B(true)
        case Seq => v2
      }

      case Call(v1, v2) if (isValue(v1) && isValue(v2)) => {
        v1 match {
          case Function(None, x, e3) => substitute(e3, v2, x)
          case Function(Some(f), x, e3) => {
            val funct = Function(Some(f), x, e3)
            substitute(substitute(e3, funct, f), v2, x)
          }
          case _ => throw new DynamicTypeError(e)
        }
      }

      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))

      case Unary(uop, e1) => Unary(uop, step(e1))
      
      case Binary(And, v1, e2) if (isValue(v1)) => toBoolean(v1) match {
          case true => B(true)
          case false => e2
      }
      case Binary(And, e1, e2) => Binary(And, step(e1), e2)

      case Binary(Or, v1, e2) if (isValue(v1)) => toBoolean(v1) match {
          case true => B(true)
          case false => e2
      }
      case Binary(Or, e1, e2) => Binary(Or, step(e1), e2)

      case Binary(Seq, v1, e2) if (isValue(v1)) => e2

      case Binary(bop, v1, e2) if (isValue(v1)) => Binary(bop, v1, step(e2))
      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)

      case If(v1, e2, e3) if (isValue(v1)) => {
        if (toBoolean(v1)) {
          if (isValue(e2)) e2 else step(e2)
        } 
        else {
          if (isValue(e3)) e3 else step(e3)
        }
      }
      case If(e1, e2, e3) => If(step(e1), e2, e3)

      
      case ConstDecl(x, v1, e2) if (isValue(v1)) => substitute(e2, v1, x)
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)
      case Call(v1, e2) if (isValue(v1)) => v1 match {
        case Function(x, y, z) => Call(v1, step(e2))
        case _ => throw new DynamicTypeError(e)
      } 
      case Call(e1, e2) => Call(step(e1), e2)
      
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def iterateStep(e: Expr): Expr =
    if (isValue(e)) e else iterateStep(step(e))
    
}
