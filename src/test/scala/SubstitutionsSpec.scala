import org.scalatest._
import types._
import substitutions._

class SubstitutionsSpec extends FlatSpec with Matchers {
  "A substitution" should "be extended adding a (symbol, type) pair" in {
    (Substitutions.extend("y")(Type.TyVar("T1"))(Subst(Map[String,Type]()))
     should be (Subst(Map("y" -> Type.TyVar("T1")))))
  }
}
