import org.scalatest._
import types._
import substitutions._

class SubstitutionsSpec extends FlatSpec with Matchers {
  "A substitution" should "be extended adding a (symbol, type) pair" in {
    (Substitutions.extend("y")(Type.TyVar("T1"))(Subst(Map[String,Type]()))
     should be (Subst(Map("y" -> Type.TyVar("T1")))))
  }

  it should "be extended when not empty" in {
    (Substitutions.extend("x")(Type.TyVar("T"))(Subst(Map("y" -> Type.TyVar("V"))))
     should be (Subst(Map("y" -> Type.TyVar("V"),
                          "x" -> Type.TyVar("T")))))
  }
}
