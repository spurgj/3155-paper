import org.scalatest._
import jsy.lab4.ast._
import Lab4._

class FunctionCallSpec extends FlatSpec {

  "Functions" should "be considered values" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, List((x, TNumber)), Some(TNumber), Var(x))
    val e2 = Function(Some(f), List((x, TNumber)), Some(TNumber), Var(x))
    assert(isValue(e1))
    assert(isValue(e2))
  } 
  
  "Call" should "evaluate a function using small-step semantics" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, List((x, TNumber)), Some(TNumber), Binary(Plus, Var(x), N(1)))
    val e2 = N(2)
    val e3 = iterateStep(Call(e1, List(e2)))
    assert(e3 === N(3))
  }

  "Call" should "handle recursive functions using small-step semantics" in {
    val f = "f"
    val x = "x"
    val fbody = If(Binary(Eq, Var(x), N(0)), Var(x), Binary(Plus, Var(x), Call(Var(f), List(Binary(Minus, Var(x), N(1))))))
    val e1 = Function(Some(f), List((x, TNumber)), Some(TNumber), fbody)
    val e2 = N(3)
    val e3 = iterateStep(Call(e1, List(e2)))
    assert(e3 === N(6))
  } 
}
