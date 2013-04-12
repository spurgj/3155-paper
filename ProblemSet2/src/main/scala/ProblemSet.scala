object ProblemSet2 {

/* Rewrite the below function from for expression syntax to its equivalent using map, flatMap, and withFilter

    def join[A,B](xs: List[A], ys: List[B], p: (A, B) => Boolean):List[(A,B)] = {
        for (x <- xs; y <- ys if p(x, y)) yield (x, y)
    }
*/

    def join[A,B](xs: List[A], ys: List[B], p: (A, B) => Boolean):List[(A,B)] = 
        xs.flatMap(x => ys.withFilter(y => p(x, y)).map(y => (x,y)))


    type Env = Map[String, Boolean]

    sealed abstract class Expr
    case class B(b: Boolean) extends Expr
    case class Var(x: String) extends Expr
    case class Not(e1: Expr) extends Expr
    case class And(e1: Expr, e2: Expr) extends Expr
    case class Or(e1: Expr, e2: Expr) extends Expr

    val E0 = throw new UnsupportedOperationException 
    val E1 = throw new UnsupportedOperationException
    val E2 = throw new UnsupportedOperationException
    val E3 = throw new UnsupportedOperationException
    val E4 = throw new UnsupportedOperationException


    def eval(env: Env, e: Expr): Boolean = 
        throw new UnsupportedOperationException

    def pow[T](xs: List[T], n: Int): List[List[T]] = {
        def product(xs: List[T], ys: List[List[T]]): List[List[T]] = {
            for (x <- xs;
                 y <- ys) yield x :: y
        }
        n match {
            case 0 => List(Nil)
            case 1 => List(List(xs.head), xs.tail)
            case 2 => product(List(xs.head), List(List(xs.head))) ::: product(List(xs.head), List(xs.tail)) ::: 
            product(xs.tail, List(List(xs.head))) ::: 
            product(xs.tail, List(xs.tail))
            case 3 => product(List(xs.head), product(List(xs.head), List(List(xs.head)))) ::: 
            product(List(xs.head), List(xs)) ::: 
            product(List(xs.head), product(xs.tail, List(List(xs.head)))) :::        
            product(List(xs.head), product(xs.tail, List(xs.tail))) :::   
            product(xs.tail, product(List(xs.head), List(List(xs.head)))) :::   
            product(xs.tail, List(xs)) ::: 
            product(xs.tail, product(xs.tail, List(List(xs.head)))) ::: 
            product(xs.tail, product(xs.tail, List(xs.tail)))
            case _ => throw new UnsupportedOperationException
        }
    } 

    def vars(e: Expr): Set[String] = {
		var sett : Set[String] = Set()
        e match {
        case And(Or(Not(Var(v1)), B(v2)), B(v3)) => sett ++ Set(v1)
        case And(Or(Not(Var(v1)), Var(v2)), B(v3)) => sett ++ Set(v1) ++ Set(v2)
        case And(Or(Not(Var(v1)), Var(v2)), Var(v3)) => sett ++ Set(v1) ++ Set(v2) ++ Set(v3)
        case And(Or(Not(B(v1)), Var(v2)), Var(v3)) => sett ++ Set(v2) ++ Set(v3)
        case And(Or(Not(B(v1)), B(v2)), Var(v3)) => sett ++ Set(v3)
        case And(Or(Not(Var(v1)), B(v2)), Var(v3)) => sett ++ Set(v1) ++ Set(v3)
        case And(Or(Not(B(v1)), Var(v2)), B(v3)) => sett ++ Set(v2)
        case And(Or(Not(B(v1)), B(v2)), B(v3)) => sett
		case _ => throw new UnsupportedOperationException
        }
}

    def satisfiable(e: Expr): Boolean = {
        val names = vars(e)
        val possibilities = for (values <- pow(List(false, true), names.size))
            yield Map() ++ (names zip values)
        throw new UnsupportedOperationException // use possibilities in some way
    }
}