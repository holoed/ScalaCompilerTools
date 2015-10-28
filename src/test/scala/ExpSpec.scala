import org.scalatest._
import compiler._

class ExpSpec extends FlatSpec with Matchers {

  "An Exp" should "toString Var to its representation" in {
    Exp.Var("x").toString should be ("x")
  }

  it should "toString Lam to its representation" in {
    Exp.Lam("x", Exp.Var("x")).toString should be ("\\x -> x")
  }

  it should "toString Tuple to its representation" in {
    Exp.Tuple(List(Exp.Var("x"), Exp.Var("y"))).toString should be ("(x,y)")
  }

  it should "toString App to its representation" in {
    Exp.App(Exp.Var("f"), Exp.Var("x")).toString should be ("f x")
  }

  it should "toString Let to its representation" in {
    Exp.Let("x", Exp.Var("y"), Exp.Var("x")).toString should be ("let x = y in x")
  }

  it should "toString Lit to its literal representation" in {
    Exp.Lit(Literal.FloatLit(5)).toString should be ("5.0")
  }
}
