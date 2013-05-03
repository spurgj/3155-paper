object Lab4 {
  import jsy.lab4.ast._
  
  /*
   * CSCI 3155: Lab 4
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
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    case h1 :: (t1 @ (h2 :: _)) => if (h1 == h2) compressRec(t1) else h1 :: compressRec(t1)
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    //Does this: (1, 1, 1, 2, 2, 3, 3, 1, 1, 1) --> (1, 2, 3, 1)
    // h and acc are parameters to the higher-order function
    // h is the next (rightmost) element of the list l
    // acc is our accumulator

    //Syntax: Lists can be expressed as h :: t
    (h, acc) => acc match {
      case Nil => h :: Nil
      case h1 :: t => if(h == h1) acc else h :: acc
    }
  }
  
  def testCompress(compress: List[Int] => List[Int]): Boolean =
    compress(List(1, 2, 2, 3, 3, 3)) == List(1, 2, 3)
  //assert(testCompress(compressRec))
  //assert(testCompress(compressFold))
  
  def mapFirst[A](f: A => Option[A])(l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: t => f(h) match {
      case None => h :: mapFirst(f)(t)
      case Some(x) => x :: t
    }
  }
  
  def testMapFirst(mapFirst: (Int => Option[Int]) => List[Int] => List[Int]): Boolean =
    mapFirst((i: Int) => if (i < 0) Some(-i) else None)(List(1, 2, -3, 4, -5)) == List(1, 2, 3, 4, -5)
  //assert(testMapFirst(mapFirst))
  
  /* Trees */
  
  sealed abstract class Tree {
    def insert(n: Int): Tree = this match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) => if (n < d) Node(l insert n, d, r) else Node(l, d, r insert n)
    } 
    
    def map(f: Int => Int): Tree = this match {
      case Empty => Empty
      case Node(l, d, r) => Node(l.map(f), f(d), r.map(f))
    }
    
    def foldLeft[A](z: A)(f: (A, Int) => A): A = {
      def loop(acc: A, t: Tree): A = t match {
        case Empty => acc
        case Node(l, d, r) => loop(f(loop(acc, l),d), r)
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
  //assert(testSum(sum))
  
  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = t.foldLeft((true, None: Option[Int])){
      (bv, v) => if (bv._1) {
        bv._2 match {
          case None => (true, Some(v))
          case Some(x) => (v > x, Some(v))
        }
      }
      else {
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
    case TFunction(params, t1) => true
    case _ => false
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
      case ConstDecl(x, e1, e2) => typeInfer(env + (x -> typ(e1)), e2)
      case Unary(Neg, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typ(e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      /*case Binary(Plus, e1, e2) => (typ(e1), typ(e2)) => {
        (TNumber, TNumber) => TNumber
        (TString, TString) => TString
        (t1, t2) => err(t1, e1)
      }*/
      case Binary(op, e1, e2) => (op, typ(e1), typ(e2)) match {
        case (Plus, TString, TString) => TString
        case (Eq, tgot1, tgot2) => (tgot1, tgot2) match {
          case (TBool, TBool) => TBool
          case (TNumber, TNumber) => TBool
          case (TObj(x1), TObj(x2)) => TBool
          case (TString, TString) => TBool
          case (TUndefined, TUndefined) => TBool
          case _ => err(tgot1, e1)
        }
        case (Ne, tgot1, tgot2) => TBool
        
        case (Lt, tgot1, tgot2) => (tgot1, tgot2) match {
          case (TNumber, TNumber) => TBool
          case _ => err(tgot1, e1)
        }
        case (Le, tgot1, tgot2) => (tgot1, tgot2) match {
          case (TNumber, TNumber) => TBool
          case _ => err(tgot1, e1)
        }
        case (Gt, tgot1, tgot2) => (tgot1, tgot2) match {
          case (TNumber, TNumber) => TBool
          case _ => err(tgot1, e1)
        }
        case (Ge, tgot1, tgot2) => (tgot1, tgot2) match {
          case (TNumber, TNumber) => TBool
          case _ => err(tgot1, e1)
        }
        
        case (Seq, _, tgot2) => tgot2
        case (And, TBool, TBool) => TBool
        case (Or, TBool, TBool) => TBool
        case (op, TNumber, TNumber) => TNumber
        case (op, tgot1, tgot2) => err(tgot1, e1)
      }
      case If(p, e1, e2) => (typ(p), typ(e1), typ(e2)) match {
        case (TBool, tgot1, tgot2) if tgot1 == tgot2 => tgot1
        case (TBool, tgot1, tgot2) => err(tgot2, e2)
        case (x, _, _) => err(x, p)
      }

      case Call(funct, args) => { 
        val rt = typ(funct)
        rt match {
          case TFunction(params, t1) => {
            val acc = params.zip(args).foreach { (type_x: ((String, Typ), Expr)) => {
                val tgot = typ(type_x._2)
                if (type_x._1._2 != tgot) 
                  err(tgot, type_x._2)
              }
            }
            if (params.length == args.length) t1 
            else err(rt, funct)
          }
          case _ => err(rt, funct)
        }
      }

      case Obj(fields) => TObj(fields.mapValues((e: Expr) => typ(e)))
      
      case GetField(obj, z) => {
        val obj1 = typ(obj)
        obj1 match {
          case TObj(fields) => fields.get(z) match {
            case None => err(obj1, obj)
            case Some(z) => z
          }
          case _ => err(obj1, obj)
        }
      }

      case Function(p, params, tann, e1) => {
        val env1 = (p, tann) match {
          case (Some(f), Some(rt)) =>
            val tprime = TFunction(params, rt)
            env + (f -> tprime)
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        
        val env2 = env1 ++ params

        // Match on whether the return type is specified.
        tann match {
          case None => TFunction(params, typeInfer(env2, e1))
          case Some(rt) => { TFunction(params, if (rt == typeInfer(env2, e1)) rt
            else err(rt, e1))
          }
        }
      }
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
      case Obj(f) => Obj(f.mapValues((e: Expr) => subst(e)))
      case x @ Function(p, params, tann, e1) => {
        if (params.exists((t1: (String, Typ)) => t1._1 == x) || Some(x) == p) {
          x
        }
        else {
          Function(p, params, tann, subst(e1))
        }
      }
      case GetField(e1, f) => if (x != f) GetField(subst(e1), f) else e
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def step(e: Expr): Expr = {
    require(!isValue(e))
    
    def stepIfNotValue(e: Expr): Option[Expr] = if (isValue(e)) None else Some(step(e))
    
    /*def substitutionHelper(acc: Expr, param_arg: ((String, Typ), Expr)): Expr = {
      param_arg match{
        case ((param_name, param_typ), arg) => substitute(acc, param_name, arg)
      }
    }*/

    e match {
      /* Base Cases: Do Rules */
      /*** Fill-in more cases here. ***/
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      case Unary(Neg, N(n1)) => N(- n1)
      case Unary(Not, B(b1)) => B(! b1)
      case Binary(Seq, v1, e2) if isValue(v1) => e2
      case Binary(Plus, S(s1), S(s2)) => S(s1 + s2)
      case Binary(Plus, N(n1), N(n2)) => N(n1 + n2)
      case Binary(Minus, N(n1), N(n2)) => N(n1 - n2)
      case Binary(Times, N(n1), N(n2)) => N(n1 * n2)
      case Binary(Div, N(n1), N(n2)) => N(n1 / n2)
      case If(B(b1), e2, e3) => if (b1) e2 else e3
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => B(inequalityVal(bop, v1, v2))
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => B(v1 == v2)
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => B(v1 != v2)
      case Binary(And, B(b1), e2) => if (b1) e2 else B(false)
      case Binary(Or, B(b1), e2) => if (b1) B(true) else e2
      case ConstDecl(x, v1, e2) if isValue(v1) => substitute(e2, v1, x)
        
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case Unary(uop, e1) => Unary(uop, step(e1))
      case Binary(bop, v1, e2) if isValue(v1) => Binary(bop, v1, step(e2))
      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)
      /*** Fill-in more cases here. ***/
      
      case Call((x @ Function(p, params, _, y)), args) if args.forall(isValue) => {
        val yp = p match {
          case None => y
          case Some(f) => substitute(y, x, f)
        }
        params.zip(args).foldLeft(yp) {
          (e1: Expr, x1: ((String, Typ), Expr)) => substitute(e1, x1._2, x1._1._1)
        }
      }

      case Call(Function(p, params, x, body), args) => {
        Call(Function(p, params, x, body), mapFirst((arg: Expr) => 
          if (isValue(arg)) None
          else Some(step(arg)))(args))
      }
     /* case Call(v1, args) if isValue(v1) && (args forall isValue) => v1 match {
        case Function(p, params, _, e1) => 
        //Get the parameters and args into a single list
          val params_and_args_list = a.zip(b)
          //Using the helper function, substitue all args for the associated params
          var ebodyp = params_and_args_list.foldLeft(ebody)(substitutionHelper)
          case _ => throw new StuckError(e)
      }*/
      case Call(e1, args) => Call(step(e1), args)

      case Obj(f) => { 
        Obj(f.mapValues((e: Expr) => if (isValue(e)) e 
        else step(e)))
    }

      case GetField(Obj(fields), f) => fields.get(f) match {
        case None => throw new StuckError(e)
        case Some(x) => x
      }

      case GetField(e1, f) => GetField(step(e1), f)
      /* Everything else is a stuck error. */
      case _ => throw new StuckError(e)
    }
  }

  def iterateStep(e: Expr): Expr =
    if (isValue(e)) e else iterateStep(step(e))
    
}