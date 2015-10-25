import org.scalatest._
import types._
import substitutions._

class SubstitutionsSpec extends FlatSpec with Matchers {
  "A substitution" should "be extended adding a (symbol, type) pair" in {
    (Substitutions.extend("T1")(Type.TyVar("T1"))(Subst(Map[String,Type]()))
     should be (Subst(Map("T1" -> Type.TyVar("T1")))))
  }

  it should "be extended when not empty" in {
    (Substitutions.extend("T")(Type.TyVar("T"))(Subst(Map("V" -> Type.TyVar("V"))))
     should be (Subst(Map("V" -> Type.TyVar("V"),
                          "T" -> Type.TyVar("T")))))
  }

  it should "be able to be looked up by symbol name" in {
    (Substitutions.lookup("T")(Subst(Map("T" -> Type.TyLam(Type.TyVar("V"), Type.TyVar("K")))))
     should be (Type.TyLam(Type.TyVar("V"), Type.TyVar("K"))))

    (Substitutions.lookup("T")(Subst(Map[String, Type]()))
     should be (Type.TyVar("T")))
  }
}
