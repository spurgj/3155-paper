  object Lab4 {
  import jsy.lab4.ast._
  
  /*
   * CSCI 3155: Lab 4
   * Justin Spurgeon
   * 
   * Partner: Thomas Dressler
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
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    //@ operator binds t1 to the matched pattern (rather than binding to the contents of h2::_)
    case h1 :: (t1 @ (h2 :: _)) => 
      //If h1 == h2, return h1 prepended to a recursion through the tail of the list EXCEPT for the tail's head element
      if(h1 == h2) compressRec(h1::t1.tail)
      //If not equal, return the head prepended to a recursion through the entire tail
      else h1::compressRec(t1)
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    //First, if list is Null, just prepend h 
    //else if acc.head != h, add h to the accum!!
    //else, add h to the accumulator
    (h, acc) => {
      if(acc == Nil) h::acc 
      else if(h != acc.head) h::acc  
      else acc
    }
  }
  
  def testCompress(compress: List[Int] => List[Int]): Boolean =
    compress(List(1, 2, 2, 3, 3, 3)) == List(1, 2, 3)
  assert(testCompress(compressRec))
  assert(testCompress(compressFold))
  
  def mapFirst[A](f: A => Option[A])(l: List[A]): List[A] = l match {
    case Nil => l
    //match the result of applying the function to the element.
    //If it yields Some(a), then we can return our list replacing h with a.
    case h :: t => f(h) match {
      case Some(a) => a :: t
      case _ => h :: mapFirst(f)(t)
    } 
  }
  
  def testMapFirst(mapFirst: (Int => Option[Int]) => List[Int] => List[Int]): Boolean =
    mapFirst((i: Int) => if (i < 0) Some(-i) else None)(List(1, 2, -3, 4, -5)) == List(1, 2, 3, 4, -5)
  assert(testMapFirst(mapFirst))
  
  /* Trees */
  
  sealed abstract class Tree {
    def insert(n: Int): Tree = this match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) => if (n < d) Node(l insert n, d, r) else Node(l, d, r insert n)
    } 
    
    def map(f: Int => Int): Tree = this match {
      case Empty => throw new UnsupportedOperationException
      case Node(l, d, r) => throw new UnsupportedOperationException
    }
    
    def foldLeft[A](z: A)(f: (A, Int) => A): A = {
      def loop(acc: A, t: Tree): A = t match {
        //Traverse the tree recursively in order.
        //If node is empty, return z (the accumulator)
        case Empty => acc
        //If node isn't empty, do f(d) with recursion down tree.  Recurse on left first so association will be in- order 
        case Node(l, d, r) => loop(loop(f(acc, d), l), r)
      }
      loop(z, this)
    }
    
    def pretty: String = {
      def p(acc: String, t: Tree, indent: Int): String = t match {
        case Empty => acc
        case Node(l, d, r) =>
          val spacer = " " * indent
          p("%s%d%n".format(spacer, d) + p(acc, l, indent + 2), r, indent + 2)
      } 
      p("", this, 0)
    }
  }
  case object Empty extends Tree
  case class Node(l: Tree, d: Int, r: Tree) extends Tree
  
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }
  
  def incr(t: Tree): Tree = t.map(i => i + 1)
  //def incr(t: SearchTree): SearchTree = t.map{ i => i + 1 }
  //def incr(t: SearchTree): SearchTree = t.map{ _ + 1 } // using placeholder notation
  
  def testIncr(incr: Tree => Tree): Boolean =
    incr(treeFromList(List(1,2,3))) == treeFromList(List(2,3,4))
  //assert(testIncr(incr))
  
  def sum(t: Tree): Int = t.foldLeft(0){ (acc, d) => acc + d }
  
  def testSum(sum: Tree => Int): Boolean =
    sum(treeFromList(List(1,2,3))) == 6
  assert(testSum(sum))
  
  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = t.foldLeft((true, None: Option[Int])){
      (b, v) => if (b._1) {
          b._2 match {
            case None => (true, Some(v))
            case Some(e) => if (e < v) {
				(true, Some(e))
			}
			else {
				(false, None)
			}
          }
        } else {
          (false, None)
        }
    }
    b
  }
  
  def testStrictlyOrdered(strictlyOrdered: Tree => Boolean): Boolean =
    !strictlyOrdered(treeFromList(List(1,1,2)))
  //assert(testStrictlyOrdered(strictlyOrdered))
  
  /* Type Inference */
  
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case _ => throw new UnsupportedOperationException
  }
  
  
  def typeInfer(env: Map[String,Typ], e: Expr): Typ = {
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw new StaticTypeError(tgot, e1, e)
    e match {
      case Print(e1) => typ(e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env(x)
      //Might need to change constDecl so that it throws error when e1 is not well typed...
      case ConstDecl(x, e1, e2) => typeInfer(env + (x -> typ(e1)), e2)
      case Unary(Neg, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typ(e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(rt)) =>
            val tprime = TFunction(params, rt)
            env + (f -> tprime)
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1)((currentEnv, paramTuple) => currentEnv + (paramTuple._1-> paramTuple._2))
        tann match {
          case None => {
            TFunction(params, typeInfer(env2, e1))            
          }
          case Some(rt) => {
        	TFunction(params, rt)           
          }
        }
      }
      //Begin adding required cases here
      case Binary(Seq, e1, e2) => (typ(e1), e2) match {
        case (TNumber, e2) => typ(e2)
        case (TBool, e2) => typ(e2)
        case (TUndefined, e2) => typ(e2)
        case (TString, e2) => typ(e2)
        case (tgot, _) => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TString, TString) => TString
        case (TNumber, tgot) => err(tgot, e2)
        case (TString, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }
      ///////////////////////////////////////////////////////////
      case Binary(Minus, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TNumber, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
        case _ => err(TUndefined, e1)
      }
      case Binary(Div, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TNumber, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)        
        case _ => err(TUndefined, e1)
      }
      case Binary(Times, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TNumber, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
        case _ => err(TUndefined, e1)
      }
      ///////////////////////////////////////////////////////////
      case Binary(Lt, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (TNumber, tgot) => err(tgot, e2)
        case (TString, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }
      case Binary(Le, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (TNumber, tgot) => err(tgot, e2)
        case (TString, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }
      case Binary(Gt, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (TNumber, tgot) => err(tgot, e2)
        case (TString, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }
      case Binary(Ge, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (TNumber, tgot) => err(tgot, e2)
        case (TString, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }
      ///////////////////////////////////////////////////////////
      case Binary(Eq, e1, e2) => (typ(e1), typ(e2)) match {
        case (TFunction(_, _), _) => err(TUndefined, e1)
        case (_, TFunction(_, _)) => err(TUndefined, e2)
        case (TNumber, TNumber) => TBool
        case (TBool, TBool) => TBool
        case (TString, TString) => TBool
        case (tgot, _) => err(tgot, e1)
      }
      case Binary(Ne, e1, e2) => (typ(e1), typ(e2)) match {
        case (TFunction(_, _), _) => err(TUndefined, e1)
        case (_, TFunction(_, _)) => err(TUndefined, e2)
        case (TNumber, TNumber) => TBool
        case (TBool, TBool) => TBool
        case (TString, TString) => TBool
        case (tgot, _) => err(tgot, e1)
      }
      ///////////////////////////////////////////////////////////
      case Binary(And, e1, e2) => (typ(e1), typ(e2)) match {
        case (TBool, TBool) => TBool
        case (TBool, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }
      case Binary(Or, e1, e2) => (typ(e1), typ(e2)) match {
        case (TBool, TBool) => TBool
        case (TBool, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }
      ///////////////////////////////////////////////////////////
      case If(e1, e2, e3) => typ(e1) match {
        case TBool => (typ(e2), typ(e3)) match {
          case (TNumber, TNumber) => TNumber
          case (TBool, TBool) => TBool
          case (TString, TString) => TString
          case _ => err(TUndefined, e2)
        }        
        case tgot => err(tgot, e1)
      }
      
      ///////////////////////////////////////////////////////////
      
      case Call(e1, args) => typ(e1) match {
        case TFunction(params, tret) => {
          if(args.length != params.unzip._1.length) err(TUndefined, e1)
          val newList = params zip (args map {typ(_)}) 
          val b = newList.foldLeft(true){
            (b, tuple) => b match {
          	  case false => false
          	  case true => tuple match {
          	    case ((x,y),z) => y==z
          	  }    
          	}
          }
          if(!b) err(TUndefined, e1)
          return tret
        }
        case _ => err(TUndefined, e1)
      }
       ///////////////////////////////////////////////////////////
       
      case Obj(fields) => TObj(fields mapValues {typ(_)})
      case GetField(e1, f) => typ(e1) match {
        case TObj(tfields) => tfields getOrElse(f, err(TUndefined, e1))
        case tgot => err(tgot, e1)
      }
      ///////////////////////////////////////////////////////////

      
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def inferType(e: Expr): Typ = typeInfer(Map.empty, e)
  
  /* Small-Step Interpreter */
  
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    
    def subst(e: Expr): Expr = substitute(e, v, x)
    
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Call(e1, args) => Call(subst(e1), args map subst)
      case Var(y) => if (x == y) v else e
      case ConstDecl(y, e1, e2) =>
        ConstDecl(y, subst(e1), if (x == y) e2 else subst(e2))
      case Function(p, params, tann, func_e) => {
        Function(p, params, tann, subst(func_e))    
      }
      case Obj(fields) => Obj(fields.mapValues((e: Expr) => subst(e)))
      case GetField(e1, f) => if (x != f) GetField(subst(e1), f) else e
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def step(e: Expr): Expr = {
    require(!isValue(e))
    
    def stepIfNotValue(e: Expr): Option[Expr] = if (isValue(e)) None else Some(step(e))

    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      case Unary(Neg, N(n1)) => N(- n1)
      case Unary(Not, B(b1)) => B(! b1)
      case Binary(Seq, v1, e2) if isValue(v1) => e2
      case Binary(Plus, S(s1), S(s2)) => S(s1 + s2)
      case Binary(Plus, N(n1), N(n2)) => N(n1 + n2)
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => B(inequalityVal(bop, v1, v2))
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => B(v1 == v2)
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => B(v1 != v2)
      case Binary(And, B(b1), e2) => if (b1) e2 else B(false)
      case Binary(Or, B(b1), e2) => if (b1) B(true) else e2
      case ConstDecl(x, v1, e2) if isValue(v1) => substitute(e2, v1, x)
      /*** Fill-in more cases here. ***/
      case Binary(Minus, N(n1), N(n2)) => N(n1 - n2) 
      case Binary(Times, N(n1), N(n2)) => N(n1 * n2) 
      case Binary(Div, N(n1), N(n2)) => N(n1 / n2) 
      //////////////////////////////////////////////////////////////////////////////
      
      case If(B(v1), e2, e3) => {
        if(v1) step(e2)
        else step (e3)
      }
      
      //////////////////////////////////////////////////////////////////////////////
      case Call(Function(p, params, tann, ebody), args) => {
        val newp = p match {
          case None => ebody
          case Some(s) => substitute(ebody, Function(p, params, tann, ebody), s)  
          }
          params.zip(args).foldLeft(ebody){
              (e1: Expr, t1: ((String, Typ), Expr)) => substitute(e1, t1._2, t1._1._1)          
          }
      }
      
        case Call(e1, args) => step(e1) match {
        case Function(None, params, tann, ebody) => Call(Function(None, params, tann, ebody), args map {step(_)})
        case Function(Some(s1), params, tann, ebody) => Call(Function(Some(s1), params, tann, ebody), args map {step(_)})
        case _ => throw new StuckError(e1)
      }
      ///////////
///////////////////////////////////////////////////////////////////
      case GetField(Obj(fields), field_name) =>  {
        fields getOrElse(field_name, throw new StuckError(Obj(fields)))
      }
      
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case Unary(uop, e1) => Unary(uop, step(e1))
      case Binary(bop, v1, e2) if isValue(v1) => Binary(bop, v1, step(e2))
      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)
      /*** Fill-in more cases here. ***/
      

      case Obj(fields) => fields.find((a) => !isValue(a._2)) match {
      	  case Some(option_tuple) => {
      	    step(fields(option_tuple._1))
      	    step(Obj(fields))  
      	  }
      	  case None => step(Obj(fields))  
      
      }
  
      case GetField(e1, field_name) => GetField(step(e1), field_name)
      
      /* Everything else is a stuck error. */
      case _ => throw new StuckError(e)
    }
  }    
  

  def iterateStep(e: Expr): Expr =
    if (isValue(e)) e else iterateStep(step(e))
    
}
  
